;; title: GigGuard - Freelancer Payment Protection Protocol
;; version: 1.0.0
;; summary: Smart contract escrow system for gig worker payments with milestone-based releases
;; description: Provides secure payment escrow, dispute resolution, and automated milestone releases with reputation tracking

;; traits

;; token definitions
(define-fungible-token GUARD-POINTS)

;; constants
(define-constant contract-owner tx-sender)
(define-constant ERR-UNAUTHORIZED (err u1000))
(define-constant ERR-INVALID-AMOUNT (err u1001))
(define-constant ERR-INVALID-MILESTONE (err u1002))
(define-constant ERR-INVALID-JOB (err u1003))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1004))
(define-constant ERR-DISPUTE-ACTIVE (err u1005))
(define-constant ERR-INVALID-PAYMENT-STATE (err u1006))
(define-constant ERR-ARBITRATOR-REQUIRED (err u1007))
(define-constant ERR-ALREADY-RELEASED (err u1008))
(define-constant ERR-INVALID-REPUTATION (err u1009))

(define-constant DISPUTE-WINDOW u604800) ;; 7 days in blocks
(define-constant MIN-ARBITRATOR-STAKE u1000000000) ;; 1 STX in microSTX
(define-constant ARBITRATOR-REWARD-PERCENT u500) ;; 5% in basis points (500/10000)
(define-constant MIN-REPUTATION-FOR-INSTANT-PAYMENT u50)

;; data vars
(define-data-var next-job-id uint u0)
(define-data-var next-dispute-id uint u0)
(define-data-var next-arbitrator-id uint u0)
(define-data-var total-escrow uint u0)
(define-data-var platform-fee-balance uint u0)
(define-data-var arbitration-fee-percent uint u300) ;; 3% in basis points

;; data maps

;; Job structure: Tracks freelance work and escrow details
(define-map jobs
  { job-id: uint }
  {
    client: principal,
    freelancer: principal,
    total-amount: uint,
    released-amount: uint,
    currency: (string-ascii 10),
    created-at: uint,
    status: (string-ascii 20), ;; "active", "completed", "disputed", "cancelled"
    milestones-count: uint,
    description: (string-ascii 256)
  }
)

;; Milestone structure: Tracks payment milestones
(define-map milestones
  { job-id: uint, milestone-id: uint }
  {
    amount: uint,
    description: (string-ascii 256),
    deadline: uint,
    proof-hash: (optional (buff 32)),
    status: (string-ascii 20), ;; "pending", "submitted", "approved", "released"
  }
)

;; Dispute structure: Tracks disputes between parties
(define-map disputes
  { dispute-id: uint }
  {
    job-id: uint,
    plaintiff: principal,
    defendant: principal,
    amount-at-stake: uint,
    created-at: uint,
    resolved-at: (optional uint),
    arbitrator: (optional principal),
    ruling: (optional (string-ascii 20)), ;; "plaintiff-wins", "defendant-wins", "split"
    arbitrator-reward: uint
  }
)

;; Arbitrator structure: Tracks arbitrator stakes and participation
(define-map arbitrators
  { arbitrator: principal }
  {
    staked-amount: uint,
    disputes-resolved: uint,
    reputation-score: uint,
    active: bool,
    joined-at: uint
  }
)

;; Reputation scores: Tracks user reputation for instant payment eligibility
(define-map reputation
  { principal: principal }
  {
    score: uint,
    completed-jobs: uint,
    disputes-won: uint,
    disputes-lost: uint,
    total-volume: uint,
    last-updated: uint
  }
)

;; Escrow balances: Tracks held funds per job
(define-map escrow-balances
  { job-id: uint }
  { amount: uint, released: uint }
)

;; Tax withholding records: Tracks taxes per freelancer
(define-map tax-withholding
  { freelancer: principal }
  {
    withheld_amount: uint,
    reporting_year: uint,
    status-1099: bool
  }
)

;; Multi-currency rates: Simple exchange rates against base currency (STX)
(define-map currency-rates
  { currency: (string-ascii 10) }
  { rate: uint } ;; rate relative to base (e.g., 1 USD = rate microSTX)
)

;; public functions

;; Create new escrow job with milestones
(define-public (create-job
  (freelancer principal)
  (total-amount uint)
  (currency (string-ascii 10))
  (milestones-count uint)
  (description (string-ascii 256))
)
  (let (
    (job-id (var-get next-job-id))
    (milestone-amount (/ total-amount milestones-count))
  )
    (asserts! (> total-amount u0) (err u1001))
    (asserts! (> milestones-count u0) (err u1002))
    
    (map-set jobs
      { job-id: job-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        total-amount: total-amount,
        released-amount: u0,
        currency: currency,
        created-at: stacks-block-height,
        status: "active",
        milestones-count: milestones-count,
        description: description
      }
    )
    
    (map-set escrow-balances
      { job-id: job-id }
      { amount: u0, released: u0 }
    )
    
    (var-set next-job-id (+ job-id u1))
    (ok job-id)
  )
)

;; Deposit funds into escrow for a job
(define-public (deposit-escrow (job-id uint) (amount uint))
  (let (
    (job (unwrap! (map-get? jobs { job-id: job-id }) (err u1003))) ;; ERR-INVALID-JOB
    (current-escrow (unwrap! (map-get? escrow-balances { job-id: job-id }) (err u1003))) ;; ERR-INVALID-JOB
  )
    (asserts! (is-eq (get client job) tx-sender) (err u1000)) ;; ERR-UNAUTHORIZED
    (asserts! (> amount u0) (err u1001)) ;; ERR-INVALID-AMOUNT
    (asserts! (<= (+ (get amount current-escrow) amount) (get total-amount job)) (err u1004)) ;; ERR-INSUFFICIENT-FUNDS
    
    ;; Update escrow balance
    (map-set escrow-balances
      { job-id: job-id }
      { 
        amount: (+ (get amount current-escrow) amount), 
        released: (get released current-escrow) 
      }
    )
    
    ;; Update total escrow tracking
    (var-set total-escrow (+ (var-get total-escrow) amount))
    
    (ok true)
  )
)

;; Submit proof of work for a milestone
(define-public (submit-milestone-proof
  (job-id uint)
  (milestone-id uint)
  (proof-hash (buff 32))
)
  (let (
    (job (unwrap! (map-get? jobs { job-id: job-id }) (err u1003))) ;; ERR-INVALID-JOB
    (milestone (unwrap! (map-get? milestones { job-id: job-id, milestone-id: milestone-id }) (err u1002))) ;; ERR-INVALID-MILESTONE
  )
    (asserts! (is-eq (get freelancer job) tx-sender) (err u1000)) ;; ERR-UNAUTHORIZED
    (asserts! (is-eq (get status milestone) "pending") (err u1006)) ;; ERR-INVALID-PAYMENT-STATE
    (asserts! (< stacks-block-height (get deadline milestone)) (err u1002)) ;; ERR-INVALID-MILESTONE
    
    ;; Update milestone with proof
    (map-set milestones
      { job-id: job-id, milestone-id: milestone-id }
      (merge milestone { 
        proof-hash: (some proof-hash), 
        status: "submitted" 
      })
    )
    
    (ok true)
  )
)

;; Approve milestone and release payment
(define-public (approve-milestone
  (job-id uint)
  (milestone-id uint)
)
  (let (
    (job (unwrap! (map-get? jobs { job-id: job-id }) (err u1003))) ;; ERR-INVALID-JOB
    (milestone (unwrap! (map-get? milestones { job-id: job-id, milestone-id: milestone-id }) (err u1002))) ;; ERR-INVALID-MILESTONE
    (escrow (unwrap! (map-get? escrow-balances { job-id: job-id }) (err u1003))) ;; ERR-INVALID-JOB
    (milestone-amount (get amount milestone))
  )
    (asserts! (is-eq (get client job) tx-sender) (err u1000)) ;; ERR-UNAUTHORIZED
    (asserts! (is-eq (get status milestone) "submitted") (err u1006)) ;; ERR-INVALID-PAYMENT-STATE
    (asserts! (>= (- (get amount escrow) (get released escrow)) milestone-amount) (err u1004)) ;; ERR-INSUFFICIENT-FUNDS
    
    ;; Update milestone status
    (map-set milestones
      { job-id: job-id, milestone-id: milestone-id }
      (merge milestone { status: "released" })
    )
    
    ;; Update escrow released amount
    (map-set escrow-balances
      { job-id: job-id }
      { 
        amount: (get amount escrow), 
        released: (+ (get released escrow) milestone-amount) 
      }
    )
    
    ;; Update reputation
    (unwrap-panic (update-reputation (get freelancer job) true u1))
    
    (ok true)
  )
)

;; Register as arbitrator with staking
(define-public (register-arbitrator (stake-amount uint))
  (let
    (
      (arbitrator-id (var-get next-arbitrator-id))
    )
    (asserts! (>= stake-amount MIN-ARBITRATOR-STAKE) ERR-INSUFFICIENT-FUNDS)
    
    ;; Check if already registered
    (if (is-some (map-get? arbitrators { arbitrator: tx-sender }))
      (err u1010) ;; Already registered
      (begin
        (map-set arbitrators
          { arbitrator: tx-sender }
          {
            staked-amount: stake-amount,
            disputes-resolved: u0,
            reputation-score: u100,
            active: true,
            joined-at: stacks-block-height
          }
        )
        (var-set next-arbitrator-id (+ arbitrator-id u1))
        ;; Mint arbitrator reputation token
        (try! (ft-mint? GUARD-POINTS stake-amount tx-sender))
        (ok true)
      )
    )
  )
)

;; Initiate dispute resolution
(define-public (create-dispute
  (job-id uint)
  (amount-at-stake uint)
  (defendant principal)
)
  (let
    (
      (dispute-id (var-get next-dispute-id))
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-INVALID-JOB))
    )
    (asserts! (> amount-at-stake u0) ERR-INVALID-AMOUNT)
    
    ;; Verify plaintiff or defendant is caller
    (asserts!
      (or (is-eq tx-sender (get client job)) (is-eq tx-sender (get freelancer job)))
      ERR-UNAUTHORIZED
    )
    
    ;; Create dispute record
    (map-set disputes
      { dispute-id: dispute-id }
      {
        job-id: job-id,
        plaintiff: tx-sender,
        defendant: defendant,
        amount-at-stake: amount-at-stake,
        created-at: stacks-block-height,
        resolved-at: none,
        arbitrator: none,
        ruling: none,
        arbitrator-reward: u0
      }
    )
    
    ;; Update job status
    (map-set jobs
      { job-id: job-id }
      (merge job { status: "disputed" })
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

;; Arbitrator accepts dispute and provides ruling
(define-public (resolve-dispute
  (dispute-id uint)
  (ruling (string-ascii 20))
)
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-INVALID-JOB))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR-ARBITRATOR-REQUIRED))
      (fee-amount (/ (get amount-at-stake dispute) (var-get arbitration-fee-percent)))
    )
    (asserts! (get active arbitrator-data) ERR-UNAUTHORIZED)
    (asserts! (is-none (get resolved-at dispute)) ERR-ALREADY-RELEASED)
    (asserts! (or (is-eq ruling "plaintiff-wins") (or (is-eq ruling "defendant-wins") (is-eq ruling "split"))) ERR-INVALID-PAYMENT-STATE)
    
    ;; Update dispute with resolution
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute
        {
          resolved-at: (some stacks-block-height),
          arbitrator: (some tx-sender),
          ruling: (some ruling),
          arbitrator-reward: fee-amount
        }
      )
    )
    
    ;; Update arbitrator stats
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data { disputes-resolved: (+ (get disputes-resolved arbitrator-data) u1) })
    )
    
    (ok true)
  )
)

;; Withdraw funds after milestone approval or dispute resolution
(define-public (withdraw-funds (job-id uint) (amount uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-INVALID-JOB))
      (escrow (unwrap! (map-get? escrow-balances { job-id: job-id }) ERR-INVALID-JOB))
    )
    (asserts! (is-eq (get freelancer job) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount (get released escrow)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Update escrow
    (map-set escrow-balances
      { job-id: job-id }
      { amount: (- (get amount escrow) amount), released: (- (get released escrow) amount) }
    )
    
    ;; Deduct from total escrow
    (var-set total-escrow (- (var-get total-escrow) amount))
    
    ;; Apply tax withholding (simplified: 10%)
    (let ((tax-amount (/ amount u10)))
      (unwrap-panic (update-tax-withholding tx-sender tax-amount))
    )
    
    ;; In real implementation: (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
    
    (ok true)
  )
)

;; Claim instant payment if reputation qualifies
(define-public (claim-instant-payment (job-id uint) (amount uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-INVALID-JOB))
      (user-rep (unwrap! (map-get? reputation { principal: (get freelancer job) }) ERR-INVALID-REPUTATION))
    )
    (asserts! (is-eq (get freelancer job) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (>= (get score user-rep) MIN-REPUTATION-FOR-INSTANT-PAYMENT) ERR-INVALID-REPUTATION)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Update released amount immediately
    (let ((escrow (unwrap! (map-get? escrow-balances { job-id: job-id }) ERR-INVALID-JOB)))
      (map-set escrow-balances
        { job-id: job-id }
        { amount: (get amount escrow), released: (+ (get released escrow) amount) }
      )
    )
    
    (ok true)
  )
)

;; read only functions

;; Get job details
(define-read-only (get-job (job-id uint))
  (map-get? jobs { job-id: job-id })
)

;; Get milestone details
(define-read-only (get-milestone (job-id uint) (milestone-id uint))
  (map-get? milestones { job-id: job-id, milestone-id: milestone-id })
)

;; Get dispute details
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

;; Get arbitrator details
(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

;; Get user reputation
(define-read-only (get-user-reputation (user principal))
  (map-get? reputation { principal: user })
)

;; Get escrow balance
(define-read-only (get-escrow-balance (job-id uint))
  (map-get? escrow-balances { job-id: job-id })
)

;; Get total platform escrow
(define-read-only (get-total-escrow)
  (ok (var-get total-escrow))
)

;; Get platform fee balance
(define-read-only (get-platform-fees)
  (ok (var-get platform-fee-balance))
)

;; Get currency rate
(define-read-only (get-currency-rate (currency (string-ascii 10)))
  (map-get? currency-rates { currency: currency })
)

;; private functions

;; Update user reputation score
(define-private (update-reputation (user principal) (completed bool) (jobs-delta uint))
  (let
    (
      (current-rep (default-to
        { score: u25, completed-jobs: u0, disputes-won: u0, disputes-lost: u0, total-volume: u0, last-updated: u0 }
        (map-get? reputation { principal: user })
      ))
      (score-increase (if completed u5 u0))
    )
    (map-set reputation
      { principal: user }
      {
        score: (+ (get score current-rep) score-increase),
        completed-jobs: (+ (get completed-jobs current-rep) jobs-delta),
        disputes-won: (get disputes-won current-rep),
        disputes-lost: (get disputes-lost current-rep),
        total-volume: (get total-volume current-rep),
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Track tax withholding
(define-private (update-tax-withholding (freelancer principal) (amount uint))
  (let
    (
      (current-tax (default-to
        { withheld_amount: u0, reporting_year: u2025,  status-1099: false }
        (map-get? tax-withholding { freelancer: freelancer })
      ))
    )
    (map-set tax-withholding
      { freelancer: freelancer }
      {
        withheld_amount: (+  (get withheld_amount current-tax) amount),
        reporting_year:  (get reporting_year current-tax),
        status-1099: true
      }
    )
    (ok true)
  )
)