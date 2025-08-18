;; ComplianceLogger Smart Contract
;; Logs compliance events for audit and traceability

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_INPUT (err u101))
(define-constant ERR_EVENT_NOT_FOUND (err u102))

;; Data Variables
(define-data-var event-counter uint u0)
(define-data-var contract-owner principal tx-sender)

;; Data Maps
(define-map compliance-events
  { event-id: uint }
  {
    invoice-id: (string-ascii 64),
    event-type: (string-ascii 32),
    actor: principal,
    timestamp: uint,
    old-status: (optional (string-ascii 32)),
    new-status: (optional (string-ascii 32)),
    document-hash: (optional (buff 32)),
    metadata: (string-ascii 256),
    block-height: uint
  }
)

(define-map authorized-actors
  { actor: principal }
  { authorized: bool }
)

;; Authorization Functions
(define-public (authorize-actor (actor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ok (map-set authorized-actors { actor: actor } { authorized: true }))
  )
)

(define-public (revoke-actor (actor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ok (map-set authorized-actors { actor: actor } { authorized: false }))
  )
)

;; Helper Functions
(define-private (is-authorized (actor principal))
  (or 
    (is-eq actor (var-get contract-owner))
    (default-to false (get authorized (map-get? authorized-actors { actor: actor })))
  )
)

(define-private (increment-counter)
  (let ((current-counter (var-get event-counter)))
    (var-set event-counter (+ current-counter u1))
    (+ current-counter u1)
  )
)

;; Core Logging Functions
(define-public (log-compliance-event 
  (invoice-id (string-ascii 64))
  (event-type (string-ascii 32))
  (old-status (optional (string-ascii 32)))
  (new-status (optional (string-ascii 32)))
  (document-hash (optional (buff 32)))
  (metadata (string-ascii 256))
)
  (let ((event-id (increment-counter)))
    (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> (len invoice-id) u0) ERR_INVALID_INPUT)
    (asserts! (> (len event-type) u0) ERR_INVALID_INPUT)
    
    (map-set compliance-events
      { event-id: event-id }
      {
        invoice-id: invoice-id,
        event-type: event-type,
        actor: tx-sender,
        timestamp: block-height,
        old-status: old-status,
        new-status: new-status,
        document-hash: document-hash,
        metadata: metadata,
        block-height: block-height
      }
    )
    
    (print {
      event: "compliance-logged",
      event-id: event-id,
      invoice-id: invoice-id,
      event-type: event-type,
      actor: tx-sender
    })
    
    (ok event-id)
  )
)

;; Specialized Logging Functions
(define-public (log-invoice-creation 
  (invoice-id (string-ascii 64))
  (document-hash (buff 32))
  (metadata (string-ascii 256))
)
  (log-compliance-event 
    invoice-id 
    "INVOICE_CREATED" 
    none 
    (some "DRAFT") 
    (some document-hash) 
    metadata
  )
)

(define-public (log-status-change 
  (invoice-id (string-ascii 64))
  (old-status (string-ascii 32))
  (new-status (string-ascii 32))
  (metadata (string-ascii 256))
)
  (log-compliance-event 
    invoice-id 
    "STATUS_CHANGED" 
    (some old-status) 
    (some new-status) 
    none 
    metadata
  )
)

(define-public (log-payment-received 
  (invoice-id (string-ascii 64))
  (amount-str (string-ascii 32))
  (metadata (string-ascii 200)) ;; Reduced from 256 to 200 to account for concatenation
)
  (log-compliance-event 
    invoice-id 
    "PAYMENT_RECEIVED" 
    (some "PENDING") 
    (some "PAID") 
    none 
    (concat "Amount: " (concat amount-str (concat " | " metadata))) ;; This creates: 8 + 32 + 3 + 200 = 243 chars (under 256 limit)
  )
)

(define-public (log-document-update 
  (invoice-id (string-ascii 64))
  (document-hash (buff 32))
  (metadata (string-ascii 256))
)
  (log-compliance-event 
    invoice-id 
    "DOCUMENT_UPDATED" 
    none 
    none 
    (some document-hash) 
    metadata
  )
)

;; Read-Only Functions for Auditing
(define-read-only (get-compliance-event (event-id uint))
  (map-get? compliance-events { event-id: event-id })
)

(define-read-only (get-event-counter)
  (var-get event-counter)
)

(define-read-only (is-actor-authorized (actor principal))
  (is-authorized actor)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Batch Query Functions
(define-read-only (get-events-batch (start-id uint) (count uint))
  (let ((end-id (+ start-id count)))
    (map get-compliance-event (list start-id (+ start-id u1) (+ start-id u2) (+ start-id u3) (+ start-id u4)))
  )
)

;; Transfer Ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)
