# On-chain Waste Management Reporting
A decentralized solution for transparent waste management reporting and tracking on the Stacks blockchain.

## 📋 Features

- Report illegal dumping with location, type, and proof
- Register and manage cleanup crews
- Track cleanup progress and completion
- Reputation system for reporters
- Transparent verification process

## 🔧 Contract Functions

### For Citizens
- `submit-report`: Submit a new waste dumping report
- `get-report`: View details of a specific report
- `get-user-stats`: Check reporter statistics

### For Cleanup Crews
- `register-cleanup-crew`: Register as an official cleanup crew
- `mark-cleanup-complete`: Mark assigned cleanup as completed

### For Administrators
- `assign-cleanup`: Assign cleanup crews to reports
- `verify-cleanup`: Verify completed cleanups
- `get-all-reports`: View reports within a range

## 🚀 Usage

1. Submit a report:
```clarity
(contract-call? .waste-management submit-report "Central Park" "plastic" u3 "ipfs://proof123")
```

2. Register as cleanup crew:
```clarity
(contract-call? .waste-management register-cleanup-crew "Green Team")
```

3. Check report status:
```clarity
(contract-call? .waste-management get-report u1)
```

## 🔐 Security

- Only contract owner can assign and verify cleanups
- Reputation system prevents abuse
- Proof required for all reports
```
