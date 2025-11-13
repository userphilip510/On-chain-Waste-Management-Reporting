(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-status (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-tokens (err u108))
(define-constant err-token-transfer-failed (err u109))
(define-constant err-penalty-already-applied (err u110))
(define-constant err-already-verified (err u117))
(define-constant err-cannot-verify-own (err u118))
(define-constant err-report-not-pending (err u119))
(define-constant err-insufficient-verifications (err u120))
(define-constant err-dispute-threshold-not-met (err u121))

(define-constant penalty-threshold-blocks u144)
(define-constant base-penalty-amount u5)

(define-constant min-stake-severity-1 u10)
(define-constant min-stake-severity-2 u15)
(define-constant min-stake-severity-3 u25)
(define-constant min-stake-severity-4 u40)
(define-constant min-stake-severity-5 u60)
(define-constant stake-bonus-multiplier u2)
(define-constant min-verification-stake u5)
(define-constant verification-reward u10)
(define-constant dispute-threshold u3)
(define-constant min-verifications-for-auto-approval u5)

(define-data-var next-report-id uint u1)
(define-data-var total-tokens-issued uint u0)
(define-data-var total-staked-tokens uint u0)

(define-map reports
    uint
    {
        reporter: principal,
        location: (string-ascii 64),
        waste-type: (string-ascii 32),
        severity: uint,
        status: (string-ascii 16),
        proof-url: (string-ascii 255),
        timestamp: uint,
        cleanup-assigned: (optional principal),
        cleanup-completed: bool,
        penalty-applied: bool,
        stake-amount: uint,
    }
)

(define-map user-stats
    principal
    {
        reports-submitted: uint,
        cleanups-completed: uint,
        reputation-score: uint,
    }
)

(define-map cleanup-crews
    principal
    {
        name: (string-ascii 64),
        active: bool,
        total-cleanups: uint,
    }
)

(define-map token-balances
    principal
    {
        balance: uint,
        total-earned: uint,
    }
)

(define-read-only (get-report (report-id uint))
    (map-get? reports report-id)
)

(define-read-only (get-user-stats (user principal))
    (default-to {
        reports-submitted: u0,
        cleanups-completed: u0,
        reputation-score: u100,
    }
        (map-get? user-stats user)
    )
)

(define-read-only (get-cleanup-crew (crew principal))
    (map-get? cleanup-crews crew)
)

(define-read-only (get-token-balance (user principal))
    (default-to {
        balance: u0,
        total-earned: u0,
    }
        (map-get? token-balances user)
    )
)

(define-public (submit-report-with-stake
        (location (string-ascii 64))
        (waste-type (string-ascii 32))
        (severity uint)
        (proof-url (string-ascii 255))
        (stake-amount uint)
    )
    (let (
            (report-id (var-get next-report-id))
            (user-stat (get-user-stats tx-sender))
            (user-balance (get-token-balance tx-sender))
            (min-stake (get-min-stake-for-severity severity))
        )
        (asserts! (< severity u6) (err u104))
        (asserts! (>= stake-amount min-stake) (err u113))
        (asserts! (>= (get balance user-balance) stake-amount)
            err-insufficient-tokens
        )
        (unwrap-panic (deduct-stake-from-user tx-sender stake-amount))
        (create-report-with-stake report-id location waste-type severity
            proof-url stake-amount
        )
        (var-set next-report-id (+ report-id u1))
        (map-set user-stats tx-sender
            (merge user-stat { reports-submitted: (+ (get reports-submitted user-stat) u1) })
        )
        (ok report-id)
    )
)

(define-public (submit-report
        (location (string-ascii 64))
        (waste-type (string-ascii 32))
        (severity uint)
        (proof-url (string-ascii 255))
    )
    (let (
            (report-id (var-get next-report-id))
            (user-stat (get-user-stats tx-sender))
        )
        (asserts! (< severity u6) (err u104))
        (create-report report-id location waste-type severity proof-url)
        (var-set next-report-id (+ report-id u1))
        (map-set user-stats tx-sender
            (merge user-stat { reports-submitted: (+ (get reports-submitted user-stat) u1) })
        )
        (ok report-id)
    )
)

(define-private (create-report-with-stake
        (id uint)
        (location (string-ascii 64))
        (waste-type (string-ascii 32))
        (severity uint)
        (proof-url (string-ascii 255))
        (stake-amount uint)
    )
    (map-insert reports id {
        reporter: tx-sender,
        location: location,
        waste-type: waste-type,
        severity: severity,
        status: "pending",
        proof-url: proof-url,
        timestamp: stacks-block-height,
        cleanup-assigned: none,
        cleanup-completed: false,
        penalty-applied: false,
        stake-amount: stake-amount,
    })
)

(define-private (create-report
        (id uint)
        (location (string-ascii 64))
        (waste-type (string-ascii 32))
        (severity uint)
        (proof-url (string-ascii 255))
    )
    (map-insert reports id {
        reporter: tx-sender,
        location: location,
        waste-type: waste-type,
        severity: severity,
        status: "pending",
        proof-url: proof-url,
        timestamp: stacks-block-height,
        cleanup-assigned: none,
        cleanup-completed: false,
        penalty-applied: false,
        stake-amount: u0,
    })
)

(define-public (assign-cleanup
        (report-id uint)
        (crew principal)
    )
    (let ((report (unwrap! (get-report report-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (get-cleanup-crew crew)) (err u105))
        (map-set reports report-id
            (merge report {
                status: "assigned",
                cleanup-assigned: (some crew),
            })
        )
        (ok true)
    )
)

(define-public (mark-cleanup-complete (report-id uint))
    (let (
            (report (unwrap! (get-report report-id) err-not-found))
            (crew-stat (unwrap! (get-cleanup-crew tx-sender) (err u106)))
            (reward-amount (calculate-reward-amount (get severity report)))
            (reporter (get reporter report))
            (stake-amount (get stake-amount report))
            (stake-bonus (if (> stake-amount u0)
                (* stake-amount stake-bonus-multiplier)
                u0
            ))
        )
        (asserts! (is-eq (some tx-sender) (get cleanup-assigned report))
            (err u107)
        )
        (map-set reports report-id
            (merge report {
                status: "completed",
                cleanup-completed: true,
            })
        )
        (map-set cleanup-crews tx-sender
            (merge crew-stat { total-cleanups: (+ (get total-cleanups crew-stat) u1) })
        )
        (unwrap-panic (award-tokens tx-sender reward-amount))
        (if (> stake-amount u0)
            (begin
                (unwrap-panic (return-stake-with-bonus reporter stake-amount stake-bonus))
                (unwrap-panic (update-reporter-reputation reporter))
                true
            )
            true
        )
        (ok true)
    )
)

(define-private (update-reporter-reputation (reporter principal))
    (let ((user-stat (get-user-stats reporter)))
        (map-set user-stats reporter
            (merge user-stat { reputation-score: (+ (get reputation-score user-stat) u10) })
        )
        (ok true)
    )
)

(define-read-only (get-all-reports
        (start uint)
        (end uint)
    )
    (map get-report (list start end))
)

(define-private (calculate-reward-amount (severity uint))
    (if (is-eq severity u5)
        u100
        (if (is-eq severity u4)
            u75
            (if (is-eq severity u3)
                u50
                (if (is-eq severity u2)
                    u25
                    u10
                )
            )
        )
    )
)

(define-private (award-tokens
        (recipient principal)
        (amount uint)
    )
    (let (
            (current-balance (get-token-balance recipient))
            (current-total-supply (var-get total-tokens-issued))
        )
        (map-set token-balances recipient
            (merge current-balance {
                balance: (+ (get balance current-balance) amount),
                total-earned: (+ (get total-earned current-balance) amount),
            })
        )
        (var-set total-tokens-issued (+ current-total-supply amount))
        (ok true)
    )
)

(define-public (transfer-tokens
        (recipient principal)
        (amount uint)
    )
    (let (
            (sender-balance (get-token-balance tx-sender))
            (recipient-balance (get-token-balance recipient))
        )
        (asserts! (>= (get balance sender-balance) amount)
            err-insufficient-tokens
        )
        (map-set token-balances tx-sender
            (merge sender-balance { balance: (- (get balance sender-balance) amount) })
        )
        (map-set token-balances recipient
            (merge recipient-balance { balance: (+ (get balance recipient-balance) amount) })
        )
        (ok true)
    )
)

(define-private (calculate-penalty
        (report-age uint)
        (severity uint)
    )
    (let (
            (severity-multiplier (if (>= severity u4)
                u2
                u1
            ))
            (age-multiplier (/ report-age penalty-threshold-blocks))
        )
        (* base-penalty-amount severity-multiplier age-multiplier)
    )
)

(define-public (apply-penalty-for-delayed-verification (report-id uint))
    (let (
            (report (unwrap! (get-report report-id) err-not-found))
            (report-age (- stacks-block-height (get timestamp report)))
            (penalty-amount (calculate-penalty report-age (get severity report)))
        )
        (asserts! (>= report-age penalty-threshold-blocks) (err u111))
        (asserts! (is-eq false (get penalty-applied report))
            err-penalty-already-applied
        )
        (asserts! (is-eq false (get cleanup-completed report)) (err u112))

        (map-set reports report-id (merge report { penalty-applied: true }))

        (match (get cleanup-assigned report)
            assigned-crew (unwrap-panic (deduct-tokens assigned-crew penalty-amount))
            true
        )
        (ok penalty-amount)
    )
)

(define-private (deduct-tokens
        (user principal)
        (amount uint)
    )
    (let ((current-balance (get-token-balance user)))
        (if (>= (get balance current-balance) amount)
            (begin
                (map-set token-balances user
                    (merge current-balance { balance: (- (get balance current-balance) amount) })
                )
                (var-set total-tokens-issued
                    (- (var-get total-tokens-issued) amount)
                )
                (ok true)
            )
            (ok true)
        )
    )
)

(define-private (get-min-stake-for-severity (severity uint))
    (if (is-eq severity u1)
        min-stake-severity-1
        (if (is-eq severity u2)
            min-stake-severity-2
            (if (is-eq severity u3)
                min-stake-severity-3
                (if (is-eq severity u4)
                    min-stake-severity-4
                    min-stake-severity-5
                )
            )
        )
    )
)

(define-private (deduct-stake-from-user
        (user principal)
        (amount uint)
    )
    (let ((current-balance (get-token-balance user)))
        (map-set token-balances user
            (merge current-balance { balance: (- (get balance current-balance) amount) })
        )
        (var-set total-staked-tokens (+ (var-get total-staked-tokens) amount))
        (ok true)
    )
)

(define-private (return-stake-with-bonus
        (user principal)
        (stake-amount uint)
        (bonus uint)
    )
    (let ((current-balance (get-token-balance user)))
        (map-set token-balances user
            (merge current-balance {
                balance: (+ (+ (get balance current-balance) stake-amount) bonus),
                total-earned: (+ (get total-earned current-balance) bonus),
            })
        )
        (var-set total-staked-tokens
            (- (var-get total-staked-tokens) stake-amount)
        )
        (var-set total-tokens-issued (+ (var-get total-tokens-issued) bonus))
        (ok true)
    )
)

(define-public (register-cleanup-crew (name (string-ascii 64)))
    (if (is-none (get-cleanup-crew tx-sender))
        (begin
            (map-set cleanup-crews tx-sender {
                name: name,
                active: true,
                total-cleanups: u0,
            })
            (ok true)
        )
        err-already-exists
    )
)

(define-map leaderboard-cache
    (string-ascii 16)
    {
        user: principal,
        score: uint,
        last-updated: uint,
    }
)

(define-data-var leaderboard-last-updated uint u0)
(define-data-var leaderboard-update-threshold uint u144)

(define-public (update-leaderboard)
    (let (
            (current-block stacks-block-height)
            (last-updated (var-get leaderboard-last-updated))
            (threshold (var-get leaderboard-update-threshold))
        )
        (asserts! (>= (- current-block last-updated) threshold) (err u114))
        (begin
            (unwrap-panic (update-top-reporters))
            (unwrap-panic (update-top-cleaners))
            (var-set leaderboard-last-updated current-block)
            (ok true)
        )
    )
)

(define-private (update-top-reporters)
    (let (
            (top-reporter-1 (find-top-reporter-by-reputation))
            (top-reporter-2 (find-second-top-reporter-by-reputation))
            (top-reporter-3 (find-third-top-reporter-by-reputation))
        )
        (begin
            (map-set leaderboard-cache "top-reporter-1" {
                user: (get user top-reporter-1),
                score: (get score top-reporter-1),
                last-updated: stacks-block-height,
            })
            (map-set leaderboard-cache "top-reporter-2" {
                user: (get user top-reporter-2),
                score: (get score top-reporter-2),
                last-updated: stacks-block-height,
            })
            (map-set leaderboard-cache "top-reporter-3" {
                user: (get user top-reporter-3),
                score: (get score top-reporter-3),
                last-updated: stacks-block-height,
            })
            (ok true)
        )
    )
)

(define-private (update-top-cleaners)
    (let (
            (top-cleaner-1 (find-top-cleaner-by-completions))
            (top-cleaner-2 (find-second-top-cleaner-by-completions))
            (top-cleaner-3 (find-third-top-cleaner-by-completions))
        )
        (begin
            (map-set leaderboard-cache "top-cleaner-1" {
                user: (get user top-cleaner-1),
                score: (get score top-cleaner-1),
                last-updated: stacks-block-height,
            })
            (map-set leaderboard-cache "top-cleaner-2" {
                user: (get user top-cleaner-2),
                score: (get score top-cleaner-2),
                last-updated: stacks-block-height,
            })
            (map-set leaderboard-cache "top-cleaner-3" {
                user: (get user top-cleaner-3),
                score: (get score top-cleaner-3),
                last-updated: stacks-block-height,
            })
            (ok true)
        )
    )
)

(define-private (find-top-reporter-by-reputation)
    {
        user: contract-owner,
        score: u100,
    }
)

(define-private (find-second-top-reporter-by-reputation)
    {
        user: contract-owner,
        score: u90,
    }
)

(define-private (find-third-top-reporter-by-reputation)
    {
        user: contract-owner,
        score: u80,
    }
)

(define-private (find-top-cleaner-by-completions)
    {
        user: contract-owner,
        score: u100,
    }
)

(define-private (find-second-top-cleaner-by-completions)
    {
        user: contract-owner,
        score: u90,
    }
)

(define-private (find-third-top-cleaner-by-completions)
    {
        user: contract-owner,
        score: u80,
    }
)

(define-read-only (get-leaderboard-entry (position (string-ascii 16)))
    (map-get? leaderboard-cache position)
)

(define-read-only (get-full-leaderboard)
    (list
        (get-leaderboard-entry "top-reporter-1")
        (get-leaderboard-entry "top-reporter-2")
        (get-leaderboard-entry "top-reporter-3")
        (get-leaderboard-entry "top-cleaner-1")
        (get-leaderboard-entry "top-cleaner-2")
        (get-leaderboard-entry "top-cleaner-3")
    )
)

(define-public (claim-leaderboard-reward (position (string-ascii 16)))
    (let (
            (entry (unwrap! (get-leaderboard-entry position) err-not-found))
            (reward-amount (calculate-leaderboard-reward position))
        )
        (asserts! (is-eq tx-sender (get user entry)) (err u115))
        (asserts! (>= (- stacks-block-height (get last-updated entry)) u144)
            (err u116)
        )

        (map-set leaderboard-cache position
            (merge entry { last-updated: stacks-block-height })
        )

        (unwrap-panic (award-tokens tx-sender reward-amount))
        (ok reward-amount)
    )
)

(define-private (calculate-leaderboard-reward (position (string-ascii 16)))
    (if (or (is-eq position "top-reporter-1") (is-eq position "top-cleaner-1"))
        u200
        (if (or (is-eq position "top-reporter-2") (is-eq position "top-cleaner-2"))
            u100
            u50
        )
    )
)

(define-map report-verifications
    {
        report-id: uint,
        verifier: principal,
    }
    {
        stake-amount: uint,
        verification-type: (string-ascii 16),
        timestamp: uint,
        rewarded: bool,
    }
)

(define-map report-verification-stats
    uint
    {
        total-verifications: uint,
        total-disputes: uint,
        verification-status: (string-ascii 16),
        total-verification-stake: uint,
        total-dispute-stake: uint,
    }
)

(define-read-only (get-verification
        (report-id uint)
        (verifier principal)
    )
    (map-get? report-verifications {
        report-id: report-id,
        verifier: verifier,
    })
)

(define-read-only (get-verification-stats (report-id uint))
    (default-to {
        total-verifications: u0,
        total-disputes: u0,
        verification-status: "unverified",
        total-verification-stake: u0,
        total-dispute-stake: u0,
    }
        (map-get? report-verification-stats report-id)
    )
)

(define-public (verify-report
        (report-id uint)
        (stake-amount uint)
    )
    (let (
            (report (unwrap! (get-report report-id) err-not-found))
            (verifier-balance (get-token-balance tx-sender))
            (existing-verification (get-verification report-id tx-sender))
            (verification-stats (get-verification-stats report-id))
        )
        (asserts! (is-eq (get status report) "pending") err-report-not-pending)
        (asserts! (not (is-eq tx-sender (get reporter report)))
            err-cannot-verify-own
        )
        (asserts! (is-none existing-verification) err-already-verified)
        (asserts! (>= stake-amount min-verification-stake) (err u122))
        (asserts! (>= (get balance verifier-balance) stake-amount)
            err-insufficient-tokens
        )

        (unwrap-panic (deduct-tokens-for-verification tx-sender stake-amount))

        (map-set report-verifications {
            report-id: report-id,
            verifier: tx-sender,
        } {
            stake-amount: stake-amount,
            verification-type: "support",
            timestamp: stacks-block-height,
            rewarded: false,
        })

        (map-set report-verification-stats report-id {
            total-verifications: (+ (get total-verifications verification-stats) u1),
            total-disputes: (get total-disputes verification-stats),
            verification-status: (get verification-status verification-stats),
            total-verification-stake: (+ (get total-verification-stake verification-stats) stake-amount),
            total-dispute-stake: (get total-dispute-stake verification-stats),
        })

        (if (>= (+ (get total-verifications verification-stats) u1)
                min-verifications-for-auto-approval
            )
            (begin
                (map-set reports report-id (merge report { status: "verified" }))
                (map-set report-verification-stats report-id
                    (merge (get-verification-stats report-id) { verification-status: "verified" })
                )
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (dispute-report
        (report-id uint)
        (stake-amount uint)
    )
    (let (
            (report (unwrap! (get-report report-id) err-not-found))
            (verifier-balance (get-token-balance tx-sender))
            (existing-verification (get-verification report-id tx-sender))
            (verification-stats (get-verification-stats report-id))
        )
        (asserts! (is-eq (get status report) "pending") err-report-not-pending)
        (asserts! (not (is-eq tx-sender (get reporter report)))
            err-cannot-verify-own
        )
        (asserts! (is-none existing-verification) err-already-verified)
        (asserts! (>= stake-amount min-verification-stake) (err u122))
        (asserts! (>= (get balance verifier-balance) stake-amount)
            err-insufficient-tokens
        )

        (unwrap-panic (deduct-tokens-for-verification tx-sender stake-amount))

        (map-set report-verifications {
            report-id: report-id,
            verifier: tx-sender,
        } {
            stake-amount: stake-amount,
            verification-type: "dispute",
            timestamp: stacks-block-height,
            rewarded: false,
        })

        (map-set report-verification-stats report-id {
            total-verifications: (get total-verifications verification-stats),
            total-disputes: (+ (get total-disputes verification-stats) u1),
            verification-status: (get verification-status verification-stats),
            total-verification-stake: (get total-verification-stake verification-stats),
            total-dispute-stake: (+ (get total-dispute-stake verification-stats) stake-amount),
        })

        (if (>= (+ (get total-disputes verification-stats) u1) dispute-threshold)
            (begin
                (map-set reports report-id (merge report { status: "disputed" }))
                (map-set report-verification-stats report-id
                    (merge (get-verification-stats report-id) { verification-status: "disputed" })
                )
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (resolve-verification
        (report-id uint)
        (valid bool)
    )
    (let (
            (report (unwrap! (get-report report-id) err-not-found))
            (verification-stats (get-verification-stats report-id))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts!
            (or
                (is-eq (get verification-status verification-stats) "verified")
                (is-eq (get verification-status verification-stats) "disputed")
            )
            (err u123)
        )

        (if valid
            (begin
                (map-set reports report-id (merge report { status: "verified" }))
                (map-set report-verification-stats report-id
                    (merge verification-stats { verification-status: "resolved-valid" })
                )
                (ok true)
            )
            (begin
                (map-set reports report-id (merge report { status: "rejected" }))
                (map-set report-verification-stats report-id
                    (merge verification-stats { verification-status: "resolved-invalid" })
                )
                (ok true)
            )
        )
    )
)

(define-public (claim-verification-reward (report-id uint))
    (let (
            (verification (unwrap! (get-verification report-id tx-sender) err-not-found))
            (verification-stats (get-verification-stats report-id))
            (verification-type (get verification-type verification))
        )
        (asserts! (not (get rewarded verification)) (err u124))
        (asserts!
            (or
                (is-eq (get verification-status verification-stats)
                    "resolved-valid"
                )
                (is-eq (get verification-status verification-stats)
                    "resolved-invalid"
                )
            )
            (err u125)
        )

        (let (
                (is-valid-resolution (is-eq (get verification-status verification-stats)
                    "resolved-valid"
                ))
                (is-correct-verification (and
                    (is-eq verification-type "support")
                    is-valid-resolution
                ))
                (is-correct-dispute (and
                    (is-eq verification-type "dispute")
                    (not is-valid-resolution)
                ))
                (stake-amount (get stake-amount verification))
                (reward-amount (if (or is-correct-verification is-correct-dispute)
                    (+ stake-amount verification-reward)
                    u0
                ))
            )
            (map-set report-verifications {
                report-id: report-id,
                verifier: tx-sender,
            }
                (merge verification { rewarded: true })
            )

            (if (> reward-amount u0)
                (unwrap-panic (award-tokens tx-sender reward-amount))
                true
            )

            (ok reward-amount)
        )
    )
)

(define-private (deduct-tokens-for-verification
        (user principal)
        (amount uint)
    )
    (let ((current-balance (get-token-balance user)))
        (map-set token-balances user
            (merge current-balance { balance: (- (get balance current-balance) amount) })
        )
        (ok true)
    )
)

(define-constant wmr-feat1-err-already-exists (err u130))

(define-map wmr-feat1-receipts
    {
        hash: (buff 32),
    }
    {
        sender: principal,
        height: uint,
    }
)

(define-data-var wmr-feat1-count uint u0)

(define-public (wmr-feat1-commit (hash (buff 32)))
    (let ((existing (map-get? wmr-feat1-receipts { hash: hash })))
        (if (is-some existing)
            wmr-feat1-err-already-exists
            (let ((new-count (+ (var-get wmr-feat1-count) u1)))
                (map-set wmr-feat1-receipts { hash: hash } {
                    sender: tx-sender,
                    height: stacks-block-height,
                })
                (var-set wmr-feat1-count new-count)
                (ok new-count)
            )
        )
    )
)

(define-read-only (wmr-feat1-get (hash (buff 32)))
    (map-get? wmr-feat1-receipts { hash: hash })
)

(define-read-only (wmr-feat1-is-committed (hash (buff 32)))
    (is-some (map-get? wmr-feat1-receipts { hash: hash }))
)

(define-read-only (wmr-feat1-count-total)
    (var-get wmr-feat1-count)
)
