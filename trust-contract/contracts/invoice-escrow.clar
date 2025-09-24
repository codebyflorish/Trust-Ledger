;; Invoice Escrow System
;; A Clarity smart contract that implements role-based access control
;; with escrow functionality for secure invoice payments

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-DOES-NOT-EXIST (err u102))
(define-constant ERR-INVALID-ROLE (err u103))
(define-constant ERR-INVALID-STATUS (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-ESCROW-NOT-FOUND (err u106))
(define-constant ERR-ESCROW-ALREADY-EXISTS (err u107))
(define-constant ERR-TIMEOUT-NOT-REACHED (err u108))
(define-constant ERR-DISPUTE-ACTIVE (err u109))
(define-constant ERR-INVALID-AMOUNT (err u110))

;; Role definitions
(define-constant ROLE-NONE u0)
(define-constant ROLE-ISSUER u1)
(define-constant ROLE-VERIFIER u2)
(define-constant ROLE-ADMIN u3)

;; Invoice status
(define-constant STATUS-PENDING u0)
(define-constant STATUS-VERIFIED u1)
(define-constant STATUS-REJECTED u2)
(define-constant STATUS-APPROVED u3)
(define-constant STATUS-ESCROWED u4)
(define-constant STATUS-PAID u5)
(define-constant STATUS-COMPLETED u6)
(define-constant STATUS-DISPUTED u7)

;; Escrow timeout (in blocks) - approximately 24 hours assuming 10-minute blocks
(define-constant ESCROW-TIMEOUT-BLOCKS u144)

;; Data maps
(define-map roles { address: principal } { role: uint })

(define-map invoices 
  { id: uint } 
  { 
    issuer: principal,
    recipient: principal,
    amount: uint,
    description: (string-utf8 256),
    created-at: uint,
    status: uint,
    verifier: (optional principal)
  }
)

(define-map escrows
  { invoice-id: uint }
  {
    payer: principal,
    amount: uint,
    deposited-at: uint,
    timeout-block: uint,
    issuer-confirmed: bool,
    recipient-confirmed: bool,
    dispute-raised: bool,
    dispute-resolver: (optional principal)
  }
)

(define-map disputes
  { invoice-id: uint }
  {
    raised-by: principal,
    raised-at: uint,
    reason: (string-utf8 256),
    resolved: bool,
    resolution: (optional (string-utf8 256))
  }
)

;; Contract owner (initial admin)
(define-data-var contract-owner principal tx-sender)

;; Invoice counter
(define-data-var invoice-counter uint u0)

;; Helper functions
(define-private (is-authorized (address principal) (required-role uint))
  (let ((user-role (default-to ROLE-NONE (get role (map-get? roles { address: address })))))
    (or 
      (is-eq address (var-get contract-owner))
      (and (>= user-role required-role) (<= user-role ROLE-ADMIN))
    )
  )
)

(define-private (is-admin (address principal))
  (is-authorized address ROLE-ADMIN)
)

(define-private (is-verifier (address principal))
  (is-authorized address ROLE-VERIFIER)
)

(define-private (is-issuer (address principal))
  (is-authorized address ROLE-ISSUER)
)

(define-private (is-invoice-party (invoice-id uint) (address principal))
  (match (map-get? invoices { id: invoice-id })
    invoice (or 
              (is-eq address (get issuer invoice))
              (is-eq address (get recipient invoice))
            )
    false
  )
)

;; Public functions

;; Role management (admin only)
(define-public (assign-role (address principal) (new-role uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-role ROLE-NONE) (<= new-role ROLE-ADMIN)) ERR-INVALID-ROLE)
    (ok (map-set roles { address: address } { role: new-role }))
  )
)

(define-public (revoke-role (address principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? roles { address: address })) ERR-DOES-NOT-EXIST)
    (ok (map-delete roles { address: address }))
  )
)

;; Invoice management

;; Create invoice (issuer only)
(define-public (create-invoice (recipient principal) (amount uint) (description (string-utf8 256)))
  (let ((invoice-id (var-get invoice-counter)))
    (begin
      (asserts! (is-issuer tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (map-set invoices 
        { id: invoice-id } 
        { 
          issuer: tx-sender,
          recipient: recipient,
          amount: amount,
          description: description,
          created-at: block-height,
          status: STATUS-PENDING,
          verifier: none
        }
      )
      (var-set invoice-counter (+ invoice-id u1))
      (ok invoice-id)
    )
  )
)

;; Verify invoice (verifier only)
(define-public (verify-invoice (invoice-id uint))
  (let ((invoice (map-get? invoices { id: invoice-id })))
    (begin
      (asserts! (is-verifier tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-eq (get status (unwrap-panic invoice)) STATUS-PENDING) ERR-INVALID-STATUS)
      (ok (map-set invoices 
        { id: invoice-id } 
        (merge (unwrap-panic invoice) 
          { 
            status: STATUS-VERIFIED,
            verifier: (some tx-sender)
          }
        )
      ))
    )
  )
)

;; Approve invoice (recipient only)
(define-public (approve-invoice (invoice-id uint))
  (let ((invoice (map-get? invoices { id: invoice-id })))
    (begin
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-eq tx-sender (get recipient (unwrap-panic invoice))) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get status (unwrap-panic invoice)) STATUS-VERIFIED) ERR-INVALID-STATUS)
      (ok (map-set invoices 
        { id: invoice-id } 
        (merge (unwrap-panic invoice) { status: STATUS-APPROVED })
      ))
    )
  )
)

;; Escrow functions

;; Deposit funds to escrow (payer - usually the recipient)
(define-public (deposit-to-escrow (invoice-id uint))
  (let (
    (invoice (map-get? invoices { id: invoice-id }))
    (amount (get amount (unwrap! invoice ERR-DOES-NOT-EXIST)))
  )
    (begin
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-eq (get status (unwrap-panic invoice)) STATUS-APPROVED) ERR-INVALID-STATUS)
      (asserts! (is-none (map-get? escrows { invoice-id: invoice-id })) ERR-ESCROW-ALREADY-EXISTS)
      
      ;; Transfer STX to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Create escrow record
      (map-set escrows
        { invoice-id: invoice-id }
        {
          payer: tx-sender,
          amount: amount,
          deposited-at: block-height,
          timeout-block: (+ block-height ESCROW-TIMEOUT-BLOCKS),
          issuer-confirmed: false,
          recipient-confirmed: false,
          dispute-raised: false,
          dispute-resolver: none
        }
      )
      
      ;; Update invoice status
      (map-set invoices 
        { id: invoice-id } 
        (merge (unwrap-panic invoice) { status: STATUS-ESCROWED })
      )
      
      (ok true)
    )
  )
)

;; Confirm completion by issuer
(define-public (confirm-completion-issuer (invoice-id uint))
  (let (
    (invoice (map-get? invoices { id: invoice-id }))
    (escrow (map-get? escrows { invoice-id: invoice-id }))
  )
    (begin
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-some escrow) ERR-ESCROW-NOT-FOUND)
      (asserts! (is-eq tx-sender (get issuer (unwrap-panic invoice))) ERR-NOT-AUTHORIZED)
      (asserts! (not (get dispute-raised (unwrap-panic escrow))) ERR-DISPUTE-ACTIVE)
      
      (map-set escrows
        { invoice-id: invoice-id }
        (merge (unwrap-panic escrow) { issuer-confirmed: true })
      )
      
      (ok true)
    )
  )
)

;; Confirm completion by recipient
(define-public (confirm-completion-recipient (invoice-id uint))
  (let (
    (invoice (map-get? invoices { id: invoice-id }))
    (escrow (map-get? escrows { invoice-id: invoice-id }))
  )
    (begin
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-some escrow) ERR-ESCROW-NOT-FOUND)
      (asserts! (is-eq tx-sender (get recipient (unwrap-panic invoice))) ERR-NOT-AUTHORIZED)
      (asserts! (not (get dispute-raised (unwrap-panic escrow))) ERR-DISPUTE-ACTIVE)
      
      (map-set escrows
        { invoice-id: invoice-id }
        (merge (unwrap-panic escrow) { recipient-confirmed: true })
      )
      
      (ok true)
    )
  )
)

;; Release escrow funds (when both parties confirm or timeout reached)
(define-public (release-escrow (invoice-id uint))
  (let (
    (invoice (map-get? invoices { id: invoice-id }))
    (escrow (map-get? escrows { invoice-id: invoice-id }))
    (escrow-data (unwrap! escrow ERR-ESCROW-NOT-FOUND))
    (amount (get amount escrow-data))
    (issuer (get issuer (unwrap! invoice ERR-DOES-NOT-EXIST)))
    (both-confirmed (and (get issuer-confirmed escrow-data) (get recipient-confirmed escrow-data)))
    (timeout-reached (>= block-height (get timeout-block escrow-data)))
  )
    (begin
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-some escrow) ERR-ESCROW-NOT-FOUND)
      (asserts! (not (get dispute-raised escrow-data)) ERR-DISPUTE-ACTIVE)
      (asserts! (or both-confirmed timeout-reached) ERR-TIMEOUT-NOT-REACHED)
      
      ;; Transfer funds to issuer
      (try! (as-contract (stx-transfer? amount tx-sender issuer)))
      
      ;; Update invoice status
      (map-set invoices 
        { id: invoice-id } 
        (merge (unwrap-panic invoice) { status: STATUS-COMPLETED })
      )
      
      ;; Remove escrow record
      (map-delete escrows { invoice-id: invoice-id })
      
      (ok true)
    )
  )
)

;; Dispute functions

;; Raise dispute
(define-public (raise-dispute (invoice-id uint) (reason (string-utf8 256)))
  (let (
    (invoice (map-get? invoices { id: invoice-id }))
    (escrow (map-get? escrows { invoice-id: invoice-id }))
  )
    (begin
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-some escrow) ERR-ESCROW-NOT-FOUND)
      (asserts! (is-invoice-party invoice-id tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (not (get dispute-raised (unwrap-panic escrow))) ERR-DISPUTE-ACTIVE)
      
      ;; Mark dispute in escrow
      (map-set escrows
        { invoice-id: invoice-id }
        (merge (unwrap-panic escrow) { dispute-raised: true })
      )
      
      ;; Create dispute record
      (map-set disputes
        { invoice-id: invoice-id }
        {
          raised-by: tx-sender,
          raised-at: block-height,
          reason: reason,
          resolved: false,
          resolution: none
        }
      )
      
      ;; Update invoice status
      (map-set invoices 
        { id: invoice-id } 
        (merge (unwrap-panic invoice) { status: STATUS-DISPUTED })
      )
      
      (ok true)
    )
  )
)

;; Resolve dispute (admin only)
(define-public (resolve-dispute (invoice-id uint) (release-to-issuer bool) (resolution (string-utf8 256)))
  (let (
    (invoice (map-get? invoices { id: invoice-id }))
    (escrow (map-get? escrows { invoice-id: invoice-id }))
    (dispute (map-get? disputes { invoice-id: invoice-id }))
    (escrow-data (unwrap! escrow ERR-ESCROW-NOT-FOUND))
    (amount (get amount escrow-data))
    (issuer (get issuer (unwrap! invoice ERR-DOES-NOT-EXIST)))
    (payer (get payer escrow-data))
  )
    (begin
      (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-some escrow) ERR-ESCROW-NOT-FOUND)
      (asserts! (is-some dispute) ERR-DOES-NOT-EXIST)
      (asserts! (get dispute-raised escrow-data) ERR-DISPUTE-ACTIVE)
      
      ;; Transfer funds based on resolution
      (if release-to-issuer
        (try! (as-contract (stx-transfer? amount tx-sender issuer)))
        (try! (as-contract (stx-transfer? amount tx-sender payer)))
      )
      
      ;; Update dispute record
      (map-set disputes
        { invoice-id: invoice-id }
        (merge (unwrap-panic dispute) 
          { 
            resolved: true,
            resolution: (some resolution)
          }
        )
      )
      
      ;; Update invoice status
      (map-set invoices 
        { id: invoice-id } 
        (merge (unwrap-panic invoice) 
          { status: (if release-to-issuer STATUS-COMPLETED STATUS-REJECTED) }
        )
      )
      
      ;; Remove escrow record
      (map-delete escrows { invoice-id: invoice-id })
      
      (ok true)
    )
  )
)

;; Read-only functions

;; Get user role
(define-read-only (get-role (address principal))
  (default-to ROLE-NONE (get role (map-get? roles { address: address })))
)

;; Get invoice details
(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices { id: invoice-id })
)

;; Get escrow details
(define-read-only (get-escrow (invoice-id uint))
  (map-get? escrows { invoice-id: invoice-id })
)

;; Get dispute details
(define-read-only (get-dispute (invoice-id uint))
  (map-get? disputes { invoice-id: invoice-id })
)

;; Check if escrow can be released
(define-read-only (can-release-escrow (invoice-id uint))
  (match (map-get? escrows { invoice-id: invoice-id })
    escrow (let (
      (both-confirmed (and (get issuer-confirmed escrow) (get recipient-confirmed escrow)))
      (timeout-reached (>= block-height (get timeout-block escrow)))
      (no-dispute (not (get dispute-raised escrow)))
    )
      (and no-dispute (or both-confirmed timeout-reached))
    )
    false
  )
)

;; Check authorization
(define-read-only (check-authorization (address principal) (required-role uint))
  (is-authorized address required-role)
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Initialize contract - set contract owner as admin
(begin
  (map-set roles { address: tx-sender } { role: ROLE-ADMIN })
)
