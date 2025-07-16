(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-status (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-tokens (err u108))
(define-constant err-token-transfer-failed (err u109))

(define-data-var next-report-id uint u1)
(define-data-var total-tokens-issued uint u0)

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
