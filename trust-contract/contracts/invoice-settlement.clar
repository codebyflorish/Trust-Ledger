;; InvoiceSettlement Contract
;; Handles invoice payments and token transfers with security measures

;; SIP-010 Trait Definition
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)


;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVOICE_NOT_FOUND (err u101))
(define-constant ERR_INVOICE_ALREADY_PAID (err u102))
(define-constant ERR_INSUFFICIENT_AMOUNT (err u103))
(define-constant ERR_INVALID_TOKEN (err u104))
(define-constant ERR_TRANSFER_FAILED (err u105))
(define-constant ERR_INVALID_RECIPIENT (err u106))
(define-constant ERR_INVOICE_EXPIRED (err u107))
(define-constant ERR_SELF_PAYMENT (err u108))

;; Data Variables
(define-data-var next-invoice-id uint u1)
(define-data-var contract-paused bool false)

;; Data Maps
(define-map invoices
  { invoice-id: uint }
  {
    issuer: principal,
    recipient: principal,
    amount: uint,
    token-contract: (optional principal),
    description: (string-ascii 256),
    due-date: uint,
    created-at: uint,
    paid-at: (optional uint),
    status: (string-ascii 20)
  }
)

(define-map settlements
  { settlement-id: uint }
  {
    invoice-id: uint,
    payer: principal,
    amount-paid: uint,
    token-used: (optional principal),
    settled-at: uint,
    tx-hash: (buff 32)
  }
)

(define-map authorized-tokens
  { token-contract: principal }
  { enabled: bool }
)

(define-map user-balances
  { user: principal, token: (optional principal) }
  { balance: uint }
)

;; Private Functions

;; Validate token contract
(define-private (is-valid-token (token-contract (optional principal)))
  (match token-contract
    some-token (default-to false (get enabled (map-get? authorized-tokens { token-contract: some-token })))
    true ;; STX is always valid
  )
)

;; Check if invoice exists and is valid
(define-private (validate-invoice (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    some-invoice (ok some-invoice)
    (err ERR_INVOICE_NOT_FOUND)
  )
)

;; Check if invoice is payable
(define-private (is-invoice-payable (invoice-data { issuer: principal, recipient: principal, amount: uint, token-contract: (optional principal), description: (string-ascii 256), due-date: uint, created-at: uint, paid-at: (optional uint), status: (string-ascii 20) }))
  (and
    (is-eq (get status invoice-data) "pending")
    (is-none (get paid-at invoice-data))
    (>= block-height (get due-date invoice-data))
  )
)

;; Transfer STX with validation
(define-private (transfer-stx (amount uint) (sender principal) (recipient principal))
    (stx-transfer? amount sender recipient)
)


;; Transfer SIP-010 tokens with validation
(define-private (transfer-sip010 (amount uint) (sender principal) (recipient principal) (token-contract <sip-010-trait>))
    (contract-call? token-contract transfer amount sender recipient none)
)

;; Generate settlement receipt hash using deterministic numeric combination
(define-private (generate-settlement-hash (invoice-id uint) (payer principal) (amount uint))
  (hash160 (+ 
    (* invoice-id u1000000)
    (+ amount block-height)
  ))
)


;; Public Functions

;; Create a new invoice
(define-public (create-invoice 
  (recipient principal) 
  (amount uint) 
  (token-contract (optional principal)) 
  (description (string-ascii 256))
  (due-date uint)
)
  (let (
    (invoice-id (var-get next-invoice-id))
    (current-time block-height)
  )
    ;; Validation checks
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INSUFFICIENT_AMOUNT)
    (asserts! (not (is-eq tx-sender recipient)) ERR_SELF_PAYMENT)
    (asserts! (> due-date current-time) ERR_INVOICE_EXPIRED)
    (asserts! (is-valid-token token-contract) ERR_INVALID_TOKEN)
    
    ;; Create invoice
    (map-set invoices
      { invoice-id: invoice-id }
      {
        issuer: tx-sender,
        recipient: recipient,
        amount: amount,
        token-contract: token-contract,
        description: description,
        due-date: due-date,
        created-at: current-time,
        paid-at: none,
        status: "pending"
      }
    )
    
    ;; Increment invoice ID
    (var-set next-invoice-id (+ invoice-id u1))
    
    ;; Emit event
    (print {
      event: "invoice-created",
      invoice-id: invoice-id,
      issuer: tx-sender,
      recipient: recipient,
      amount: amount,
      token-contract: token-contract,
      due-date: due-date
    })
    
    (ok invoice-id)
  )
)

;;; Pay an invoice with STX
(define-public (pay-invoice-stx (invoice-id uint))
  (let (
    (invoice-data (unwrap! (validate-invoice invoice-id) (err u400)))
    (amount (get amount invoice-data))
    (recipient (get recipient invoice-data))
    (settlement-id (var-get next-invoice-id))
  )
    ;; Validation checks
    (asserts! (not (var-get contract-paused)) (err u404))
    (asserts! (is-none (get token-contract invoice-data)) (err u405))
    (asserts! (is-invoice-payable invoice-data) (err u406))
    (asserts! (not (is-eq tx-sender (get issuer invoice-data))) (err u407))
    
    ;; Transfer STX
    (unwrap! (stx-transfer? amount tx-sender recipient) (err u408))
    
    ;; Update next settlement ID
    (var-set next-invoice-id (+ settlement-id u1))
    
    ;; Update invoice status
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data {
        paid-at: (some block-height),
        status: "paid"
      })
    )
    
    ;; Create settlement record
    (map-set settlements
      { settlement-id: settlement-id }
      {
        invoice-id: invoice-id,
        payer: tx-sender,
        amount-paid: amount,
        token-used: none,
        settled-at: block-height,
        tx-hash: (generate-settlement-hash invoice-id tx-sender amount)
      }
    )
    
    ;; Emit settlement event
    (print {
      event: "invoice-settled",
      invoice-id: invoice-id,
      settlement-id: settlement-id,
      payer: tx-sender,
      recipient: recipient,
      amount: amount,
      token-type: "STX",
      settled-at: block-height
    })
    
    (ok settlement-id)
  )
)


;; Pay an invoice with SIP-010 tokens
(define-public (pay-invoice-token (invoice-id uint) (token-contract <sip-010-trait>))
  (let (
    (invoice-data (unwrap! (validate-invoice invoice-id) (err u400)))
    (amount (get amount invoice-data))
    (recipient (get recipient invoice-data))
    (expected-token (get token-contract invoice-data))
    (settlement-id (var-get next-invoice-id))
  )
    ;; Validation checks
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-some expected-token) ERR_INVALID_TOKEN)
    (asserts! (is-invoice-payable invoice-data) ERR_INVOICE_ALREADY_PAID)
    (asserts! (not (is-eq tx-sender (get issuer invoice-data))) ERR_SELF_PAYMENT)
    
    ;; Transfer tokens
    (unwrap! (transfer-sip010 amount tx-sender recipient token-contract) (err u408))
    
    ;; Update next settlement ID
    (var-set next-invoice-id (+ settlement-id u1))
    
    ;; Update invoice status
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data {
        paid-at: (some block-height),
        status: "paid"
      })
    )
    
    ;; Create settlement record
    (map-set settlements
      { settlement-id: settlement-id }
      {
        invoice-id: invoice-id,
        payer: tx-sender,
        amount-paid: amount,
        token-used: none,
        settled-at: block-height,
        tx-hash: (generate-settlement-hash invoice-id tx-sender amount)
      }
    )
    
    ;; Emit settlement event
    (print {
      event: "invoice-settled",
      invoice-id: invoice-id,
      settlement-id: settlement-id,
      payer: tx-sender,
      recipient: recipient,
      amount: amount,
      token-type: "SIP010",
      token-contract: token-contract,
      settled-at: block-height
    })
    
    (ok settlement-id)
  )
)

;; Cancel an invoice (only by issuer)
(define-public (cancel-invoice (invoice-id uint))
  (let (
    (invoice-data (unwrap! (validate-invoice invoice-id) (err u400)))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get issuer invoice-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status invoice-data) "pending") ERR_INVOICE_ALREADY_PAID)
    
    ;; Update invoice status
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: "cancelled" })
    )
    
    ;; Emit event
    (print {
      event: "invoice-cancelled",
      invoice-id: invoice-id,
      issuer: tx-sender
    })
    
    (ok true)
  )
)
;; Admin Functions

;; Add authorized token (only contract owner)
(define-public (add-authorized-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-tokens
      { token-contract: token-contract }
      { enabled: true }
    )
    (print { event: "token-authorized", token-contract: token-contract })
    (ok true)
  )
)

;; Remove authorized token (only contract owner)
(define-public (remove-authorized-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-tokens
      { token-contract: token-contract }
      { enabled: false }
    )
    (print { event: "token-deauthorized", token-contract: token-contract })
    (ok true)
  )
)

;; Pause/unpause contract (only contract owner)
(define-public (set-contract-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused paused)
    (print { event: "contract-paused", paused: paused })
    (ok true)
  )
)

;; Read-only Functions

;; Get invoice details
(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices { invoice-id: invoice-id })
)

;; Get settlement details
(define-read-only (get-settlement (settlement-id uint))
  (map-get? settlements { settlement-id: settlement-id })
)

;; Check if token is authorized
(define-read-only (is-token-authorized (token-contract principal))
  (default-to false (get enabled (map-get? authorized-tokens { token-contract: token-contract })))
)

;; Get contract status
(define-read-only (get-contract-status)
  {
    paused: (var-get contract-paused),
    next-invoice-id: (var-get next-invoice-id),
    owner: CONTRACT_OWNER
  }
)

;; Check invoice payment status
(define-read-only (is-invoice-paid (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    some-invoice (is-eq (get status some-invoice) "paid")
    false
  )
)

;; Get invoices by issuer (limited to prevent DoS)
(define-read-only (get-user-invoice-count (user principal))
  (let (
    (current-id (var-get next-invoice-id))
  )
    ;; This is a simplified version - in production, you'd want a more efficient indexing system
    (fold check-user-invoices (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) { user: user, count: u0 })
  )
)

;; Helper function for counting user invoices
(define-private (check-user-invoices (invoice-id uint) (acc { user: principal, count: uint }))
  (match (map-get? invoices { invoice-id: invoice-id })
    some-invoice (if (is-eq (get issuer some-invoice) (get user acc))
                    { user: (get user acc), count: (+ (get count acc) u1) }
                    acc)
    acc
  )
)
