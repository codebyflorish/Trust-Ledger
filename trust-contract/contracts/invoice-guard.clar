;; InvoiceGuard - Role-Based Access Control for Invoice Management
;; A Clarity smart contract that implements a flexible permission system
;; with separate roles for issuers, verifiers, and administrators

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-DOES-NOT-EXIST (err u102))
(define-constant ERR-INVALID-ROLE (err u103))
(define-constant ERR-INVALID-STATUS (err u104))

;; Role definitions
(define-constant ROLE-NONE u0)
(define-constant ROLE-ISSUER u1)
(define-constant ROLE-VERIFIER u2)
(define-constant ROLE-ADMIN u3)

;; Invoice status
(define-constant STATUS-PENDING u0)
(define-constant STATUS-VERIFIED u1)
(define-constant STATUS-REJECTED u2)

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

;; Reject invoice (verifier only)
(define-public (reject-invoice (invoice-id uint))
  (let ((invoice (map-get? invoices { id: invoice-id })))
    (begin
      (asserts! (is-verifier tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (is-some invoice) ERR-DOES-NOT-EXIST)
      (asserts! (is-eq (get status (unwrap-panic invoice)) STATUS-PENDING) ERR-INVALID-STATUS)
      (ok (map-set invoices 
        { id: invoice-id } 
        (merge (unwrap-panic invoice) 
          { 
            status: STATUS-REJECTED,
            verifier: (some tx-sender)
          }
        )
      ))
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

;; Check if user is authorized for a specific role
(define-read-only (check-authorization (address principal) (required-role uint))
  (is-authorized address required-role)
)

;; Initialize contract - set contract owner as admin
(begin
  (map-set roles { address: tx-sender } { role: ROLE-ADMIN })
)
