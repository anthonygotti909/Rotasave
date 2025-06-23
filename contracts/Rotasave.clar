(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_CYCLE_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_JOINED (err u102))
(define-constant ERR_CYCLE_FULL (err u103))
(define-constant ERR_CYCLE_STARTED (err u104))
(define-constant ERR_NOT_PARTICIPANT (err u105))
(define-constant ERR_ALREADY_CONTRIBUTED (err u106))
(define-constant ERR_CYCLE_NOT_ACTIVE (err u107))
(define-constant ERR_NOT_RECIPIENT_TURN (err u108))
(define-constant ERR_INSUFFICIENT_CONTRIBUTIONS (err u109))
(define-constant ERR_CYCLE_ENDED (err u110))

(define-data-var cycle-counter uint u0)

(define-map cycles
  uint
  {
    creator: principal,
    contribution-amount: uint,
    max-participants: uint,
    current-participants: uint,
    cycle-duration: uint,
    start-block: uint,
    status: (string-ascii 20),
    current-round: uint,
    total-rounds: uint
  }
)

(define-map cycle-participants
  { cycle-id: uint, participant: principal }
  {
    position: uint,
    has-received: bool,
    contributions-made: uint
  }
)

(define-map cycle-contributions
  { cycle-id: uint, round: uint, participant: principal }
  { amount: uint, block-height: uint }
)

(define-map participant-list
  { cycle-id: uint, position: uint }
  principal
)

(define-public (create-cycle (contribution-amount uint) (max-participants uint) (cycle-duration uint))
  (let
    (
      (cycle-id (+ (var-get cycle-counter) u1))
    )
    (map-set cycles cycle-id
      {
        creator: tx-sender,
        contribution-amount: contribution-amount,
        max-participants: max-participants,
        current-participants: u0,
        cycle-duration: cycle-duration,
        start-block: u0,
        status: "recruiting",
        current-round: u0,
        total-rounds: max-participants
      }
    )
    (var-set cycle-counter cycle-id)
    (ok cycle-id)
  )
)

(define-public (join-cycle (cycle-id uint))
  (let
    (
      (cycle-data (unwrap! (map-get? cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (current-participants (get current-participants cycle-data))
    )
    (asserts! (is-eq (get status cycle-data) "recruiting") ERR_CYCLE_STARTED)
    (asserts! (< current-participants (get max-participants cycle-data)) ERR_CYCLE_FULL)
    (asserts! (is-none (map-get? cycle-participants { cycle-id: cycle-id, participant: tx-sender })) ERR_ALREADY_JOINED)
    
    (map-set cycle-participants
      { cycle-id: cycle-id, participant: tx-sender }
      {
        position: current-participants,
        has-received: false,
        contributions-made: u0
      }
    )
    
    (map-set participant-list
      { cycle-id: cycle-id, position: current-participants }
      tx-sender
    )
    
    (map-set cycles cycle-id
      (merge cycle-data { current-participants: (+ current-participants u1) })
    )
    
    (if (is-eq (+ current-participants u1) (get max-participants cycle-data))
      (begin
        (map-set cycles cycle-id
          (merge cycle-data 
            { 
              current-participants: (+ current-participants u1),
              status: "active",
              start-block: stacks-block-height,
              current-round: u1
            }
          )
        )
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (contribute (cycle-id uint))
  (let
    (
      (cycle-data (unwrap! (map-get? cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (participant-data (unwrap! (map-get? cycle-participants { cycle-id: cycle-id, participant: tx-sender }) ERR_NOT_PARTICIPANT))
      (current-round (get current-round cycle-data))
    )
    (asserts! (is-eq (get status cycle-data) "active") ERR_CYCLE_NOT_ACTIVE)
    (asserts! (> current-round u0) ERR_CYCLE_NOT_ACTIVE)
    (asserts! (<= current-round (get total-rounds cycle-data)) ERR_CYCLE_ENDED)
    (asserts! (is-none (map-get? cycle-contributions { cycle-id: cycle-id, round: current-round, participant: tx-sender })) ERR_ALREADY_CONTRIBUTED)
    
    (try! (stx-transfer? (get contribution-amount cycle-data) tx-sender (as-contract tx-sender)))
    
    (map-set cycle-contributions
      { cycle-id: cycle-id, round: current-round, participant: tx-sender }
      { amount: (get contribution-amount cycle-data), block-height: stacks-block-height }
    )
    
    (map-set cycle-participants
      { cycle-id: cycle-id, participant: tx-sender }
      (merge participant-data { contributions-made: (+ (get contributions-made participant-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (claim-payout (cycle-id uint))
  (let
    (
      (cycle-data (unwrap! (map-get? cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (participant-data (unwrap! (map-get? cycle-participants { cycle-id: cycle-id, participant: tx-sender }) ERR_NOT_PARTICIPANT))
      (current-round (get current-round cycle-data))
      (recipient-position (- current-round u1))
      (expected-recipient (unwrap! (map-get? participant-list { cycle-id: cycle-id, position: recipient-position }) ERR_CYCLE_NOT_FOUND))
    )
    (asserts! (is-eq (get status cycle-data) "active") ERR_CYCLE_NOT_ACTIVE)
    (asserts! (is-eq tx-sender expected-recipient) ERR_NOT_RECIPIENT_TURN)
    (asserts! (not (get has-received participant-data)) ERR_NOT_RECIPIENT_TURN)
    (asserts! (>= (get-round-contributions cycle-id current-round) (get max-participants cycle-data)) ERR_INSUFFICIENT_CONTRIBUTIONS)
    
    (let
      (
        (payout-amount (* (get contribution-amount cycle-data) (get max-participants cycle-data)))
      )
      (try! (as-contract (stx-transfer? payout-amount tx-sender expected-recipient)))
      
      (map-set cycle-participants
        { cycle-id: cycle-id, participant: tx-sender }
        (merge participant-data { has-received: true })
      )
      
      (ok payout-amount)
    )
  )
)

(define-public (advance-round (cycle-id uint))
  (let
    (
      (cycle-data (unwrap! (map-get? cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (current-round (get current-round cycle-data))
    )
    (asserts! (is-eq (get status cycle-data) "active") ERR_CYCLE_NOT_ACTIVE)
    (asserts! (>= (- stacks-block-height (get start-block cycle-data)) (* current-round (get cycle-duration cycle-data))) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get-round-contributions cycle-id current-round) (get max-participants cycle-data)) ERR_INSUFFICIENT_CONTRIBUTIONS)
    
    (if (>= current-round (get total-rounds cycle-data))
      (begin
        (map-set cycles cycle-id (merge cycle-data { status: "completed" }))
        (ok "cycle-completed")
      )
      (begin
        (map-set cycles cycle-id (merge cycle-data { current-round: (+ current-round u1) }))
        (ok "round-advanced")
      )
    )
  )
)

(define-read-only (get-cycle (cycle-id uint))
  (map-get? cycles cycle-id)
)

(define-read-only (get-participant-info (cycle-id uint) (participant principal))
  (map-get? cycle-participants { cycle-id: cycle-id, participant: participant })
)

(define-read-only (get-round-contributions (cycle-id uint) (round uint))
  (fold check-contribution-exists (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) u0)
)

(define-read-only (get-current-recipient (cycle-id uint))
  (match (map-get? cycles cycle-id)
    cycle-data
      (let
        (
          (current-round (get current-round cycle-data))
          (recipient-position (- current-round u1))
        )
        (map-get? participant-list { cycle-id: cycle-id, position: recipient-position })
      )
    none
  )
)

(define-read-only (get-cycle-status (cycle-id uint))
  (match (map-get? cycles cycle-id)
    cycle-data (some (get status cycle-data))
    none
  )
)

(define-read-only (is-contribution-made (cycle-id uint) (round uint) (participant principal))
  (is-some (map-get? cycle-contributions { cycle-id: cycle-id, round: round, participant: participant }))
)

(define-read-only (get-total-cycles)
  (var-get cycle-counter)
)

(define-read-only (can-advance-round (cycle-id uint))
  (match (map-get? cycles cycle-id)
    cycle-data
      (let
        (
          (current-round (get current-round cycle-data))
          (blocks-passed (- stacks-block-height (get start-block cycle-data)))
          (required-blocks (* current-round (get cycle-duration cycle-data)))
        )
        (and 
          (is-eq (get status cycle-data) "active")
          (>= blocks-passed required-blocks)
          (>= (get-round-contributions cycle-id current-round) (get max-participants cycle-data))
        )
      )
    false
  )
)

(define-private (check-contribution-exists (position uint) (count uint))
  (match (map-get? participant-list { cycle-id: u0, position: position })
    participant
      (if (is-some (map-get? cycle-contributions { cycle-id: u0, round: u0, participant: participant }))
        (+ count u1)
        count
      )
    count
  )
)