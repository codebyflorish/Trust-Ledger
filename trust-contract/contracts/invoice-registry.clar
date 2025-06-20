;; InvoiceRegistry Smart Contract
;; Purpose: Main invoice storage and lifecycle management

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVOICE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INVOICE_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_TRANSITION (err u105))

;; Invoice Status Constants
(define-constant STATUS_PENDING u0)
(define-constant STATUS_APPROVED u1)
(define-constant STATUS_PAID u2)
(define-constant STATUS_DISPUTED u3)
(define-constant STATUS_CANCELLED u4)

;; Data Variables
(define-data-var invoice-counter uint u0)

;; Data Maps
(define-map invoices
  { invoice-id: uint }
  {
    issuer: principal,
    recipient: principal,
    amount: uint,
    currency: (string-ascii 10),
    description: (string-utf8 500),
    due-date: uint,
    created-at: uint,
    updated-at: uint,
    status: uint,
    metadata-hash: (buff 32),
    payment-hash: (optional (buff 32))
  }
)

;; Invoice permissions - who can modify specific invoices
(define-map invoice-permissions
  { invoice-id: uint, user: principal }
  { can-approve: bool, can-pay: bool, can-dispute: bool }
)

;; Status transition history
(define-map status-history
  { invoice-id: uint, sequence: uint }
  {
    from-status: uint,
    to-status: uint,
    changed-by: principal,
    timestamp: uint,
    reason: (optional (string-utf8 200))
  }
)

;; Track sequence numbers for status history
(define-map invoice-history-counter
  { invoice-id: uint }
  { counter: uint }
)

;; Helper Functions

;; Check if status is valid
(define-private (is-valid-status (status uint))
  (or 
    (is-eq status STATUS_PENDING)
    (is-eq status STATUS_APPROVED)
    (is-eq status STATUS_PAID)
    (is-eq status STATUS_DISPUTED)
    (is-eq status STATUS_CANCELLED)
  )
)

;; ;; Check if status transition is valid
;; (define-private (is-valid-transition (from-status uint) (to-status uint))
;;   (match (tuple (from from-status) (to to-status))
;;     ;; From PENDING
;;     { from: STATUS_PENDING, to: STATUS_APPROVED } true
;;     { from: STATUS_PENDING, to: STATUS_DISPUTED } true
;;     { from: STATUS_PENDING, to: STATUS_CANCELLED } true
    
;;     ;; From APPROVED
;;     { from: STATUS_APPROVED, to: STATUS_PAID } true
;;     { from: STATUS_APPROVED, to: STATUS_DISPUTED } true
;;     { from: STATUS_APPROVED, to: STATUS_CANCELLED } true
    
;;     ;; From DISPUTED
;;     { from: STATUS_DISPUTED, to: STATUS_APPROVED } true
;;     { from: STATUS_DISPUTED, to: STATUS_CANCELLED } true
    
;;     ;; From PAID - final state, no transitions allowed
    
;;     ;; From CANCELLED - final state, no transitions allowed
    
;;     ;; Default case
;;     false
;;   )
;; )

;; Get next invoice ID
(define-private (get-next-invoice-id)
  (let ((current-id (var-get invoice-counter)))
    (var-set invoice-counter (+ current-id u1))
    (+ current-id u1)
  )
)

;; Record status change in history
(define-private (record-status-change 
  (invoice-id uint) 
  (from-status uint) 
  (to-status uint) 
  (reason (optional (string-utf8 200))))
  (let (
    (current-counter (default-to u0 (get counter (map-get? invoice-history-counter { invoice-id: invoice-id }))))
    (new-counter (+ current-counter u1))
  )
    (map-set invoice-history-counter 
      { invoice-id: invoice-id } 
      { counter: new-counter })
    (map-set status-history
      { invoice-id: invoice-id, sequence: new-counter }
      {
        from-status: from-status,
        to-status: to-status,
        changed-by: tx-sender,
        timestamp: block-height,
        reason: reason
      }
    )
  )
)

;; Public Functions

;; Create a new invoice
(define-public (create-invoice
  (recipient principal)
  (amount uint)
  (currency (string-ascii 10))
  (description (string-utf8 500))
  (due-date uint)
  (metadata-hash (buff 32)))
  (let (
    (invoice-id (get-next-invoice-id))
    (current-time block-height)
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set invoices
      { invoice-id: invoice-id }
      {
        issuer: tx-sender,
        recipient: recipient,
        amount: amount,
        currency: currency,
        description: description,
        due-date: due-date,
        created-at: current-time,
        updated-at: current-time,
        status: STATUS_PENDING,
        metadata-hash: metadata-hash,
        payment-hash: none
      }
    )
    
    ;; Set default permissions
    (map-set invoice-permissions
      { invoice-id: invoice-id, user: recipient }
      { can-approve: true, can-pay: true, can-dispute: true }
    )
    
    ;; Record initial status
    (record-status-change invoice-id STATUS_PENDING STATUS_PENDING none)
    
    ;; Emit creation event
    (print {
      event: "invoice-created",
      invoice-id: invoice-id,
      issuer: tx-sender,
      recipient: recipient,
      amount: amount,
      currency: currency,
      due-date: due-date,
      timestamp: current-time
    })
    
    (ok invoice-id)
  )
)

;; ;; Update invoice status
;; (define-public (update-invoice-status 
;;   (invoice-id uint) 
;;   (new-status uint) 
;;   (reason (optional (string-utf8 200))))
;;   (let (
;;     (invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
;;     (current-status (get status invoice-data))
;;     (issuer (get issuer invoice-data))
;;     (recipient (get recipient invoice-data))
;;   )
;;     (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)
;;     (asserts! (is-valid-transition current-status new-status) ERR_INVALID_TRANSITION)
    
;;     ;; Check permissions based on the new status
;;     (asserts! (or
;;       (is-eq tx-sender issuer)
;;       (is-eq tx-sender recipient)
;;       (and 
;;         (is-eq new-status STATUS_APPROVED)
;;         (default-to false (get can-approve (map-get? invoice-permissions { invoice-id: invoice-id, user: tx-sender })))
;;       )
;;       (and 
;;         (is-eq new-status STATUS_DISPUTED)
;;         (default-to false (get can-dispute (map-get? invoice-permissions { invoice-id: invoice-id, user: tx-sender })))
;;       )
;;     ) ERR_UNAUTHORIZED)
    
;;     ;; Update invoice
;;     (map-set invoices
;;       { invoice-id: invoice-id }
;;       (merge invoice-data { 
;;         status: new-status, 
;;         updated-at: block-height 
;;       })
;;     )
    
;;     ;; Record status change
;;     (record-status-change invoice-id current-status new-status reason)
    
;;     ;; Emit status change event
;;     (print {
;;       event: "status-changed",
;;       invoice-id: invoice-id,
;;       from-status: current-status,
;;       to-status: new-status,
;;       changed-by: tx-sender,
;;       reason: reason,
;;       timestamp: block-height
;;     })
    
;;     (ok true)
;;   )
;; )

;; Mark invoice as paid with payment hash
(define-public (mark-as-paid (invoice-id uint) (payment-hash (buff 32)))
  (let (
    (invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    (current-status (get status invoice-data))
  )
    (asserts! (is-eq current-status STATUS_APPROVED) ERR_INVALID_TRANSITION)
    (asserts! (or 
      (is-eq tx-sender (get recipient invoice-data))
      (is-eq tx-sender (get issuer invoice-data))
    ) ERR_UNAUTHORIZED)
    
    ;; Update invoice with payment hash
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { 
        status: STATUS_PAID, 
        updated-at: block-height,
        payment-hash: (some payment-hash)
      })
    )
    
    ;; Record status change
    (record-status-change invoice-id current-status STATUS_PAID (some u"Payment completed"))
    
    ;; Emit payment event
    (print {
      event: "invoice-paid",
      invoice-id: invoice-id,
      payment-hash: payment-hash,
      paid-by: tx-sender,
      amount: (get amount invoice-data),
      timestamp: block-height
    })
    
    (ok true)
  )
)

;; Update invoice metadata hash
(define-public (update-metadata-hash (invoice-id uint) (new-hash (buff 32)))
  (let (
    (invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get issuer invoice-data)) ERR_UNAUTHORIZED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { 
        metadata-hash: new-hash, 
        updated-at: block-height 
      })
    )
    
    ;; Emit metadata update event
    (print {
      event: "metadata-updated",
      invoice-id: invoice-id,
      new-hash: new-hash,
      updated-by: tx-sender,
      timestamp: block-height
    })
    
    (ok true)
  )
)

;; Grant permissions to a user for a specific invoice
(define-public (grant-permissions 
  (invoice-id uint) 
  (user principal) 
  (can-approve bool) 
  (can-pay bool) 
  (can-dispute bool))
  (let (
    (invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get issuer invoice-data)) ERR_UNAUTHORIZED)
    
    (map-set invoice-permissions
      { invoice-id: invoice-id, user: user }
      { can-approve: can-approve, can-pay: can-pay, can-dispute: can-dispute }
    )
    
    (print {
      event: "permissions-granted",
      invoice-id: invoice-id,
      user: user,
      permissions: { can-approve: can-approve, can-pay: can-pay, can-dispute: can-dispute },
      granted-by: tx-sender,
      timestamp: block-height
    })
    
    (ok true)
  )
)

;; Read-only Functions

;; Get invoice by ID
(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices { invoice-id: invoice-id })
)

;; Get invoice status
(define-read-only (get-invoice-status (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice-data (ok (get status invoice-data))
    ERR_INVOICE_NOT_FOUND
  )
)

;; Get user permissions for an invoice
(define-read-only (get-user-permissions (invoice-id uint) (user principal))
  (map-get? invoice-permissions { invoice-id: invoice-id, user: user })
)

;; Get status history for an invoice
(define-read-only (get-status-history (invoice-id uint) (sequence uint))
  (map-get? status-history { invoice-id: invoice-id, sequence: sequence })
)

;; Get current invoice counter
(define-read-only (get-invoice-counter)
  (var-get invoice-counter)
)

;; Check if user can perform action on invoice
(define-read-only (can-user-approve (invoice-id uint) (user principal))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice-data 
      (or 
        (is-eq user (get issuer invoice-data))
        (is-eq user (get recipient invoice-data))
        (default-to false (get can-approve (map-get? invoice-permissions { invoice-id: invoice-id, user: user })))
      )
    false
  )
)

;; Get invoices by issuer (helper for off-chain indexing)
(define-read-only (get-invoice-metadata (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice-data (ok {
      issuer: (get issuer invoice-data),
      recipient: (get recipient invoice-data),
      status: (get status invoice-data),
      created-at: (get created-at invoice-data),
      updated-at: (get updated-at invoice-data),
      metadata-hash: (get metadata-hash invoice-data)
    })
    ERR_INVOICE_NOT_FOUND
  )
)