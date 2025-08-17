# 🔄 Rotasave - Rotating Savings Protocol (ROSCA)

A decentralized rotating savings and credit association (ROSCA) built on the Stacks blockchain using Clarity smart contracts.

## 🌟 What is Rotasave?

Rotasave enables groups of people to pool their money together in time-based savings cycles. Each participant contributes a fixed amount regularly, and in each round, one member receives the entire pool. This traditional savings method is now trustless and transparent on the blockchain!

## ✨ Features

- 🏗️ **Create Savings Cycles**: Set contribution amounts, participant limits, and cycle duration
- 👥 **Join Existing Cycles**: Participate in community savings groups
- 💰 **Automatic Payouts**: Receive the full pool when it's your turn
- ⏰ **Time-Based Rounds**: Cycles advance automatically based on block height
- 🔒 **Trustless**: No central authority needed - smart contract handles everything
- 📊 **Transparent**: All contributions and payouts are publicly verifiable

## 🚀 How It Works

1. **Create a Cycle**: Someone creates a new savings cycle with specific parameters
2. **Recruitment Phase**: Participants join until the cycle is full
3. **Active Phase**: Each round, all participants contribute the agreed amount
4. **Payout**: One participant receives the entire pool each round
5. **Rotation**: The process continues until everyone has received a payout

## 📋 Usage Instructions

### Creating a New Cycle

```clarity
(contract-call? .Rotasave create-cycle u1000000 u5 u144)
```
- `u1000000`: Contribution amount (1 STX in microSTX)
- `u5`: Maximum 5 participants
- `u144`: Round duration (144 blocks ≈ 1 day)

### Joining a Cycle

```clarity
(contract-call? .Rotasave join-cycle u1)
```

### Making Contributions

```clarity
(contract-call? .Rotasave contribute u1)
```

### Claiming Your Payout

```clarity
(contract-call? .Rotasave claim-payout u1)
```

### Advancing to Next Round

```clarity
(contract-call? .Rotasave advance-round u1)
```

## 🔍 Read-Only Functions

- `get-cycle`: Get cycle information
- `get-participant-info`: Check participant status
- `get-current-recipient`: See who should receive the current payout
- `get-cycle-status`: Check if cycle is recruiting, active, or completed
- `can-advance-round`: Check if round can be advanced

## 📊 Example Scenario

1. Alice creates a cycle: 5 participants, 1 STX per round, 1 day per round
2. Bob, Carol, Dave, and Eve join the cycle
3. Round 1: Everyone contributes 1 STX, Alice receives 5 STX
4. Round 2: Everyone contributes 1 STX, Bob receives 5 STX
5. This continues until everyone has received their payout

## ⚠️ Important Notes

- Participants must contribute in every round to maintain the cycle
- Payouts happen in the order participants joined
- Cycles automatically advance based on block height
- All STX amounts are in microSTX (1 STX = 1,000,000 microSTX)

## 🛡️ Security Features

- Only eligible recipients can claim payouts
- Contributions are locked in the contract until payout
- Automatic validation of all cycle rules
- Protection against double contributions and invalid claims

## 🎯 Perfect For

- 💼 **Community Savings Groups**
- 🏠 **Neighborhood Associations**  
- 👨‍👩‍👧‍👦 **Family Savings Plans**
- 🤝 **Friend Groups**
- 🌍 **Global Savings Communities**

Start your decentralized savings journey with Rotasave today! 🚀
```

