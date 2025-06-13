(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-status (err u102))
(define-constant err-already-exists (err u103))

(define-data-var next-report-id uint u1)

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
