;; Savings Goals and Milestone Tracker Contract
;; Personal financial goal tracking for Rotasave participants

;; Error constants
(define-constant err-not-authorized (err u300))
(define-constant err-goal-not-found (err u301))
(define-constant err-goal-already-exists (err u302))
(define-constant err-invalid-amount (err u303))
(define-constant err-goal-already-achieved (err u304))
(define-constant err-invalid-deadline (err u305))
(define-constant err-milestone-not-found (err u306))

;; Data variables
(define-data-var goal-id-nonce uint u0)
(define-data-var milestone-id-nonce uint u0)

;; Goal categories
(define-constant category-emergency u1)
(define-constant category-house u2)
(define-constant category-education u3)
(define-constant category-business u4)
(define-constant category-travel u5)
(define-constant category-general u6)

;; Goal status
(define-constant status-active u1)
(define-constant status-achieved u2)
(define-constant status-expired u3)
(define-constant status-paused u4)

;; Maps
(define-map user-goals
    {user: principal, goal-id: uint}
    {
        title: (string-ascii 100),
        description: (string-ascii 300),
        target-amount: uint,
        current-amount: uint,
        category: uint,
        deadline-block: uint,
        created-block: uint,
        status: uint,
        cycles-participated: uint,
        last-updated: uint
    }
)

(define-map user-goal-count
    principal
    uint
)

(define-map goal-milestones
    {user: principal, goal-id: uint, milestone-id: uint}
    {
        percentage: uint,
        achieved-block: uint,
        reward-claimed: bool
    }
)

(define-map user-savings-summary
    principal
    {
        total-saved: uint,
        goals-achieved: uint,
        active-goals: uint,
        total-cycles: uint,
        last-payout-block: uint
    }
)

(define-map goal-sharing
    {user: principal, goal-id: uint}
    {
        is-public: bool,
        supporters-count: uint,
        motivation-messages: uint
    }
)

;; Create a new savings goal
(define-public (create-savings-goal (title (string-ascii 100))
                                   (description (string-ascii 300))
                                   (target-amount uint)
                                   (category uint)
                                   (deadline-blocks uint))
    (let ((user-goal-id (default-to u0 (map-get? user-goal-count tx-sender)))
          (new-goal-id (+ user-goal-id u1))
          (deadline-block (+ stacks-block-height deadline-blocks)))
        (asserts! (> target-amount u0) err-invalid-amount)
        (asserts! (> deadline-blocks u0) err-invalid-deadline)
        (asserts! (and (>= category u1) (<= category u6)) err-not-authorized)
        
        (map-set user-goals {user: tx-sender, goal-id: new-goal-id}
            {
                title: title,
                description: description,
                target-amount: target-amount,
                current-amount: u0,
                category: category,
                deadline-block: deadline-block,
                created-block: stacks-block-height,
                status: status-active,
                cycles-participated: u0,
                last-updated: stacks-block-height
            }
        )
        
        (map-set user-goal-count tx-sender new-goal-id)
        
        ;; Update user summary
        (let ((summary (default-to 
                {total-saved: u0, goals-achieved: u0, active-goals: u0, total-cycles: u0, last-payout-block: u0}
                (map-get? user-savings-summary tx-sender))))
            (map-set user-savings-summary tx-sender
                (merge summary {active-goals: (+ (get active-goals summary) u1)})
            )
        )
        
        (ok new-goal-id)
    )
)

;; Update savings progress when user receives payout
(define-public (record-savings-progress (user principal) (amount uint) (cycle-id uint))
    (let ((user-goals-count (default-to u0 (map-get? user-goal-count user)))
          (summary (default-to 
            {total-saved: u0, goals-achieved: u0, active-goals: u0, total-cycles: u0, last-payout-block: u0}
            (map-get? user-savings-summary user))))
        ;; Update user summary
        (map-set user-savings-summary user
            {
                total-saved: (+ (get total-saved summary) amount),
                goals-achieved: (get goals-achieved summary),
                active-goals: (get active-goals summary),
                total-cycles: (+ (get total-cycles summary) u1),
                last-payout-block: stacks-block-height
            }
        )
        
;; Update progress for first goal only (simplified)
        (if (> user-goals-count u0)
            (update-goal-progress user u1 amount)
            (ok true)
        )
    )
)

;; Helper function to update individual goal progress
(define-private (update-goal-progress (user principal) (goal-id uint) (amount uint))
    (match (map-get? user-goals {user: user, goal-id: goal-id})
        goal-data
        (if (is-eq (get status goal-data) status-active)
            (let ((new-amount (+ (get current-amount goal-data) amount))
                  (target (get target-amount goal-data)))
                (map-set user-goals {user: user, goal-id: goal-id}
                    (merge goal-data 
                        {
                            current-amount: new-amount,
                            cycles-participated: (+ (get cycles-participated goal-data) u1),
                            last-updated: stacks-block-height,
                            status: (if (>= new-amount target) status-achieved (get status goal-data))
                        }
                    )
                )
                
                ;; Update milestone counter for tracking
                (var-set milestone-id-nonce (+ (var-get milestone-id-nonce) u1))
                
                ;; If goal achieved, update summary
                (if (>= new-amount target)
                    (let ((summary (unwrap-panic (map-get? user-savings-summary user))))
                        (map-set user-savings-summary user
                            (merge summary 
                                {
                                    goals-achieved: (+ (get goals-achieved summary) u1),
                                    active-goals: (- (get active-goals summary) u1)
                                }
                            )
                        )
                        (ok true)
                    )
                    (ok true)
                )
            )
            (ok true)
        )
        (ok true)
    )
)

;; Check and record milestone achievements (25%, 50%, 75%, 100%)
(define-private (check-milestone-achievement (user principal) (goal-id uint) (current-amount uint) (target-amount uint))
    (let ((progress-percentage (/ (* current-amount u100) target-amount)))
        (begin
            ;; Check 25% milestone
            (if (and (>= progress-percentage u25) 
                     (is-none (map-get? goal-milestones {user: user, goal-id: goal-id, milestone-id: u1})))
                (map-set goal-milestones {user: user, goal-id: goal-id, milestone-id: u1}
                    {
                        percentage: u25,
                        achieved-block: stacks-block-height,
                        reward-claimed: false
                    }
                )
                true
            )
            ;; Check 50% milestone
            (if (and (>= progress-percentage u50)
                     (is-none (map-get? goal-milestones {user: user, goal-id: goal-id, milestone-id: u2})))
                (map-set goal-milestones {user: user, goal-id: goal-id, milestone-id: u2}
                    {
                        percentage: u50,
                        achieved-block: stacks-block-height,
                        reward-claimed: false
                    }
                )
                true
            )
            ;; Check 75% milestone
            (if (and (>= progress-percentage u75)
                     (is-none (map-get? goal-milestones {user: user, goal-id: goal-id, milestone-id: u3})))
                (map-set goal-milestones {user: user, goal-id: goal-id, milestone-id: u3}
                    {
                        percentage: u75,
                        achieved-block: stacks-block-height,
                        reward-claimed: false
                    }
                )
                true
            )
            ;; Check 100% milestone
            (if (>= progress-percentage u100)
                (map-set goal-milestones {user: user, goal-id: goal-id, milestone-id: u4}
                    {
                        percentage: u100,
                        achieved-block: stacks-block-height,
                        reward-claimed: false
                    }
                )
                true
            )
            (ok true)
        )
    )
)

;; Award milestone achievement
(define-private (award-milestone (user principal) (goal-id uint) (milestone-id uint) (percentage uint))
    (let ((milestone-global-id (var-get milestone-id-nonce)))
        (map-set goal-milestones {user: user, goal-id: goal-id, milestone-id: milestone-id}
            {
                percentage: percentage,
                achieved-block: stacks-block-height,
                reward-claimed: false
            }
        )
        (var-set milestone-id-nonce (+ milestone-global-id u1))
        (ok milestone-id)
    )
)

;; Make goal public for community support
(define-public (share-goal (goal-id uint) (make-public bool))
    (match (map-get? user-goals {user: tx-sender, goal-id: goal-id})
        goal-data
        (begin
            (map-set goal-sharing {user: tx-sender, goal-id: goal-id}
                {
                    is-public: make-public,
                    supporters-count: u0,
                    motivation-messages: u0
                }
            )
            (ok true)
        )
        err-goal-not-found
    )
)

;; Pause or resume a goal
(define-public (update-goal-status (goal-id uint) (new-status uint))
    (match (map-get? user-goals {user: tx-sender, goal-id: goal-id})
        goal-data
        (begin
            (asserts! (and (>= new-status u1) (<= new-status u4)) err-not-authorized)
            (map-set user-goals {user: tx-sender, goal-id: goal-id}
                (merge goal-data 
                    {
                        status: new-status,
                        last-updated: stacks-block-height
                    }
                )
            )
            (ok true)
        )
        err-goal-not-found
    )
)

;; Read-only functions

;; Get user's goal information
(define-read-only (get-user-goal (user principal) (goal-id uint))
    (map-get? user-goals {user: user, goal-id: goal-id})
)

;; Get user's savings summary
(define-read-only (get-user-savings-summary (user principal))
    (default-to 
        {total-saved: u0, goals-achieved: u0, active-goals: u0, total-cycles: u0, last-payout-block: u0}
        (map-get? user-savings-summary user)
    )
)

;; Get goal progress percentage
(define-read-only (get-goal-progress (user principal) (goal-id uint))
    (match (map-get? user-goals {user: user, goal-id: goal-id})
        goal-data
        (let ((current (get current-amount goal-data))
              (target (get target-amount goal-data)))
            (if (> target u0)
                (ok (/ (* current u100) target))
                (ok u0)
            )
        )
        err-goal-not-found
    )
)

;; Get user's milestone achievements for a goal
(define-read-only (get-goal-milestones (user principal) (goal-id uint))
    (ok {
        milestone-25: (map-get? goal-milestones {user: user, goal-id: goal-id, milestone-id: u1}),
        milestone-50: (map-get? goal-milestones {user: user, goal-id: goal-id, milestone-id: u2}),
        milestone-75: (map-get? goal-milestones {user: user, goal-id: goal-id, milestone-id: u3}),
        milestone-100: (map-get? goal-milestones {user: user, goal-id: goal-id, milestone-id: u4})
    })
)

;; Get all user goals (up to 3 for simplicity)
(define-read-only (get-user-goals (user principal))
    (let ((goal-count (default-to u0 (map-get? user-goal-count user))))
        (ok {
            total-goals: goal-count,
            goal-1: (if (>= goal-count u1) (map-get? user-goals {user: user, goal-id: u1}) none),
            goal-2: (if (>= goal-count u2) (map-get? user-goals {user: user, goal-id: u2}) none),
            goal-3: (if (>= goal-count u3) (map-get? user-goals {user: user, goal-id: u3}) none)
        })
    )
)

;; Check if goal is expired based on deadline
(define-read-only (is-goal-expired (user principal) (goal-id uint))
    (match (map-get? user-goals {user: user, goal-id: goal-id})
        goal-data
        (ok (and 
                (> stacks-block-height (get deadline-block goal-data))
                (not (is-eq (get status goal-data) status-achieved))
            )
        )
        err-goal-not-found
    )
)

;; Get goal sharing information
(define-read-only (get-goal-sharing-info (user principal) (goal-id uint))
    (map-get? goal-sharing {user: user, goal-id: goal-id})
)

;; Calculate user's overall savings performance
(define-read-only (get-savings-performance (user principal))
    (let ((summary (get-user-savings-summary user))
          (goal-count (default-to u0 (map-get? user-goal-count user))))
        (ok {
            total-saved: (get total-saved summary),
            goals-achieved: (get goals-achieved summary),
            active-goals: (get active-goals summary),
            total-goals-created: goal-count,
            success-rate: (if (> goal-count u0) 
                            (/ (* (get goals-achieved summary) u100) goal-count)
                            u0),
            avg-saved-per-cycle: (if (> (get total-cycles summary) u0)
                                   (/ (get total-saved summary) (get total-cycles summary))
                                   u0)
        })
    )
)

;; Get total milestones achieved by user
(define-read-only (get-total-milestones-achieved (user principal))
    (let ((goal-count (default-to u0 (map-get? user-goal-count user))))
        (if (is-eq goal-count u0)
            (ok u0)
            (ok (+
                (if (is-some (map-get? goal-milestones {user: user, goal-id: u1, milestone-id: u1})) u1 u0)
                (+
                    (if (is-some (map-get? goal-milestones {user: user, goal-id: u1, milestone-id: u2})) u1 u0)
                    (+
                        (if (is-some (map-get? goal-milestones {user: user, goal-id: u1, milestone-id: u3})) u1 u0)
                        (if (is-some (map-get? goal-milestones {user: user, goal-id: u1, milestone-id: u4})) u1 u0)
                    )
                )
            ))
        )
    )
)

;; Get total active and completed goals counts
(define-read-only (get-global-goals-stats)
    (ok {
        total-goals-created: (var-get goal-id-nonce),
        total-milestones-awarded: (var-get milestone-id-nonce)
    })
)
