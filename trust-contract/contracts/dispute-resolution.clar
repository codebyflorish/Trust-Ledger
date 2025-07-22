;; DisputeResolution Smart Contract
;; Manages disputes and resolution workflows for invoices

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_DISPUTE_NOT_FOUND (err u404))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_STATUS (err u400))
(define-constant ERR_VOTING_CLOSED (err u403))
(define-constant ERR_ALREADY_VOTED (err u402))
(define-constant ERR_INSUFFICIENT_STAKE (err u405))

;; Dispute statuses
(define-constant STATUS_OPEN u1)
(define-constant STATUS_IN_ARBITRATION u2)
(define-constant STATUS_RESOLVED u3)
(define-constant STATUS_REJECTED u4)

;; Data Variables
(define-data-var dispute-counter uint u0)
(define-data-var arbitration-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var voting-period uint u144) ;; ~24 hours in blocks

;; Data Maps
(define-map disputes
  { dispute-id: uint }
  {
    invoice-id: uint,
    complainant: principal,
    respondent: principal,
    reason: (string-ascii 500),
    amount-disputed: uint,
    status: uint,
    created-at: uint,
    resolved-at: (optional uint),
    resolution: (optional (string-ascii 500)),
    arbitrator: (optional principal)
  }
)

(define-map dispute-votes
  { dispute-id: uint, voter: principal }
  {
    vote: bool, ;; true for complainant, false for respondent
    stake: uint,
    voted-at: uint
  }
)

(define-map dispute-vote-summary
  { dispute-id: uint }
  {
    total-votes: uint,
    complainant-votes: uint,
    respondent-votes: uint,
    total-stake: uint,
    complainant-stake: uint,
    respondent-stake: uint,
    voting-ends-at: uint
  }
)

(define-map arbitrators
  { arbitrator: principal }
  {
    active: bool,
    cases-handled: uint,
    reputation-score: uint
  }
)

(define-map invoice-disputes
  { invoice-id: uint }
  { dispute-id: uint }
)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (is-dispute-party (dispute-id uint))
  (match (map-get? disputes { dispute-id: dispute-id })
    dispute-data (or 
      (is-eq tx-sender (get complainant dispute-data))
      (is-eq tx-sender (get respondent dispute-data))
    )
    false
  )
)

(define-private (is-arbitrator (address principal))
  (match (map-get? arbitrators { arbitrator: address })
    arbitrator-data (get active arbitrator-data)
    false
  )
)

;; Public Functions

;; File a new dispute
(define-public (file-dispute 
  (invoice-id uint)
  (respondent principal)
  (reason (string-ascii 500))
  (amount-disputed uint)
)
  (let (
    (dispute-id (+ (var-get dispute-counter) u1))
    (current-block block-height)
  )
    ;; Check if dispute already exists for this invoice
    (asserts! (is-none (map-get? invoice-disputes { invoice-id: invoice-id })) ERR_DISPUTE_ALREADY_EXISTS)
    
    ;; Create dispute record
    (map-set disputes
      { dispute-id: dispute-id }
      {
        invoice-id: invoice-id,
        complainant: tx-sender,
        respondent: respondent,
        reason: reason,
        amount-disputed: amount-disputed,
        status: STATUS_OPEN,
        created-at: current-block,
        resolved-at: none,
        resolution: none,
        arbitrator: none
      }
    )
    
    ;; Link invoice to dispute
    (map-set invoice-disputes
      { invoice-id: invoice-id }
      { dispute-id: dispute-id }
    )
    
    ;; Update counter
    (var-set dispute-counter dispute-id)
    
    ;; Emit event
    (print {
      event: "dispute-filed",
      dispute-id: dispute-id,
      invoice-id: invoice-id,
      complainant: tx-sender,
      respondent: respondent,
      amount: amount-disputed
    })
    
    (ok dispute-id)
  )
)

;; Assign arbitrator to dispute
(define-public (assign-arbitrator (dispute-id uint) (arbitrator principal))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
  )
    ;; Only contract owner or dispute parties can assign arbitrator
    (asserts! (or (is-contract-owner) (is-dispute-party dispute-id)) ERR_UNAUTHORIZED)
    
    ;; Check if arbitrator is registered and active
    (asserts! (is-arbitrator arbitrator) ERR_UNAUTHORIZED)
    
    ;; Update dispute with arbitrator
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        arbitrator: (some arbitrator),
        status: STATUS_IN_ARBITRATION
      })
    )
    
    (print {
      event: "arbitrator-assigned",
      dispute-id: dispute-id,
      arbitrator: arbitrator
    })
    
    (ok true)
  )
)

;; Start community voting on dispute
(define-public (start-voting (dispute-id uint))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    (voting-end (+ block-height (var-get voting-period)))
  )
    ;; Only dispute parties can start voting
    (asserts! (is-dispute-party dispute-id) ERR_UNAUTHORIZED)
    
    ;; Dispute must be open
    (asserts! (is-eq (get status dispute-data) STATUS_OPEN) ERR_INVALID_STATUS)
    
    ;; Initialize vote summary
    (map-set dispute-vote-summary
      { dispute-id: dispute-id }
      {
        total-votes: u0,
        complainant-votes: u0,
        respondent-votes: u0,
        total-stake: u0,
        complainant-stake: u0,
        respondent-stake: u0,
        voting-ends-at: voting-end
      }
    )
    
    ;; Update dispute status
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data { status: STATUS_IN_ARBITRATION })
    )
    
    (print {
      event: "voting-started",
      dispute-id: dispute-id,
      voting-ends-at: voting-end
    })
    
    (ok true)
  )
)

;; Vote on dispute (community arbitration)
(define-public (vote-on-dispute (dispute-id uint) (vote-for-complainant bool) (stake-amount uint))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    (vote-summary (unwrap! (map-get? dispute-vote-summary { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
  )
    ;; Check if voting is still open
    (asserts! (< block-height (get voting-ends-at vote-summary)) ERR_VOTING_CLOSED)
    
    ;; Check if user hasn't voted already
    (asserts! (is-none (map-get? dispute-votes { dispute-id: dispute-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    
    ;; Minimum stake requirement
    (asserts! (>= stake-amount u100000) ERR_INSUFFICIENT_STAKE) ;; 0.1 STX minimum
    
    ;; Transfer stake to contract (simplified - in production would use escrow)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Record vote
    (map-set dispute-votes
      { dispute-id: dispute-id, voter: tx-sender }
      {
        vote: vote-for-complainant,
        stake: stake-amount,
        voted-at: block-height
      }
    )
    
    ;; Update vote summary
    (map-set dispute-vote-summary
      { dispute-id: dispute-id }
      {
        total-votes: (+ (get total-votes vote-summary) u1),
        complainant-votes: (if vote-for-complainant 
          (+ (get complainant-votes vote-summary) u1)
          (get complainant-votes vote-summary)
        ),
        respondent-votes: (if vote-for-complainant
          (get respondent-votes vote-summary)
          (+ (get respondent-votes vote-summary) u1)
        ),
        total-stake: (+ (get total-stake vote-summary) stake-amount),
        complainant-stake: (if vote-for-complainant
          (+ (get complainant-stake vote-summary) stake-amount)
          (get complainant-stake vote-summary)
        ),
        respondent-stake: (if vote-for-complainant
          (get respondent-stake vote-summary)
          (+ (get respondent-stake vote-summary) stake-amount)
        ),
        voting-ends-at: (get voting-ends-at vote-summary)
      }
    )
    
    (print {
      event: "vote-cast",
      dispute-id: dispute-id,
      voter: tx-sender,
      vote: vote-for-complainant,
      stake: stake-amount
    })
    
    (ok true)
  )
)

;; Resolve dispute (by arbitrator or after voting)
(define-public (resolve-dispute 
  (dispute-id uint) 
  (resolution (string-ascii 500))
  (favor-complainant bool)
)
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
  )
    ;; Check authorization
    (asserts! (or 
      (is-contract-owner)
      (match (get arbitrator dispute-data)
        arbitrator (is-eq tx-sender arbitrator)
        false
      )
    ) ERR_UNAUTHORIZED)
    
    ;; Update dispute status
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        status: (if favor-complainant STATUS_RESOLVED STATUS_REJECTED),
        resolved-at: (some block-height),
        resolution: (some resolution)
      })
    )
    
    ;; Update arbitrator stats if applicable
    (match (get arbitrator dispute-data)
      arbitrator (update-arbitrator-stats arbitrator)
      true
    )
    
    (print {
      event: "dispute-resolved",
      dispute-id: dispute-id,
      resolution: resolution,
      favor-complainant: favor-complainant,
      resolved-by: tx-sender
    })
    
    (ok true)
  )
)

;; Finalize voting and auto-resolve
(define-public (finalize-voting (dispute-id uint))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    (vote-summary (unwrap! (map-get? dispute-vote-summary { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
  )
    ;; Check if voting period has ended
    (asserts! (>= block-height (get voting-ends-at vote-summary)) ERR_VOTING_CLOSED)
    
    ;; Determine winner based on stake-weighted votes
    (let (
      (complainant-wins (> (get complainant-stake vote-summary) (get respondent-stake vote-summary)))
    )
      ;; Resolve dispute based on voting outcome
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data {
          status: (if complainant-wins STATUS_RESOLVED STATUS_REJECTED),
          resolved-at: (some block-height),
          resolution: (some "Resolved by community voting")
        })
      )
      
      (print {
        event: "voting-finalized",
        dispute-id: dispute-id,
        complainant-wins: complainant-wins,
        total-votes: (get total-votes vote-summary),
        total-stake: (get total-stake vote-summary)
      })
      
      (ok complainant-wins)
    )
  )
)

;; Register as arbitrator
(define-public (register-arbitrator)
  (begin
    ;; Pay registration fee
    (try! (stx-transfer? (var-get arbitration-fee) tx-sender CONTRACT_OWNER))
    
    ;; Register arbitrator
    (map-set arbitrators
      { arbitrator: tx-sender }
      {
        active: true,
        cases-handled: u0,
        reputation-score: u100
      }
    )
    
    (print {
      event: "arbitrator-registered",
      arbitrator: tx-sender
    })
    
    (ok true)
  )
)

;; Private helper functions
(define-private (update-arbitrator-stats (arbitrator principal))
  (match (map-get? arbitrators { arbitrator: arbitrator })
    arbitrator-data (map-set arbitrators
      { arbitrator: arbitrator }
      (merge arbitrator-data {
        cases-handled: (+ (get cases-handled arbitrator-data) u1)
      })
    )
    false
  )
)

;; Read-only functions

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-dispute-by-invoice (invoice-id uint))
  (match (map-get? invoice-disputes { invoice-id: invoice-id })
    invoice-dispute (map-get? disputes { dispute-id: (get dispute-id invoice-dispute) })
    none
  )
)

(define-read-only (get-vote-summary (dispute-id uint))
  (map-get? dispute-vote-summary { dispute-id: dispute-id })
)

(define-read-only (get-user-vote (dispute-id uint) (voter principal))
  (map-get? dispute-votes { dispute-id: dispute-id, voter: voter })
)

(define-read-only (get-arbitrator-info (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-dispute-count)
  (var-get dispute-counter)
)

;; Admin functions
(define-public (set-arbitration-fee (new-fee uint))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (var-set arbitration-fee new-fee)
    (ok true)
  )
)

(define-public (set-voting-period (new-period uint))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (deactivate-arbitrator (arbitrator principal))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (match (map-get? arbitrators { arbitrator: arbitrator })
      arbitrator-data (map-set arbitrators
        { arbitrator: arbitrator }
        (merge arbitrator-data { active: false })
      )
      false
    )
    (ok true)
  )
)