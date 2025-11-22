;; CreateCove Labs - Decentralized Creative Economy Platform
;; A comprehensive blockchain solution for digital art provenance, IP management, and perpetual royalties

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-already-exists (err u202))
(define-constant err-unauthorized (err u203))
(define-constant err-invalid-price (err u204))
(define-constant err-invalid-royalty (err u205))
(define-constant err-artwork-not-for-sale (err u206))
(define-constant err-insufficient-payment (err u207))

;; Maximum royalty percentage (20%)
(define-constant max-royalty-percentage u2000)

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% platform fee
(define-data-var next-artwork-id uint u1)
(define-data-var next-license-id uint u1)

;; Data Maps

;; Artwork registry: immutable provenance tracking
(define-map artworks
  uint ;; artwork-id
  {
    creator: principal,
    title: (string-utf8 256),
    ipfs-hash: (string-ascii 128), ;; IPFS content identifier
    metadata-uri: (string-ascii 256),
    creation-timestamp: uint,
    edition-size: uint,
    current-edition: uint,
    royalty-percentage: uint, ;; Basis points (100 = 1%)
    is-active: bool
  }
)

;; Ownership records: tracks current owners and provenance chain
(define-map artwork-ownership
  uint ;; artwork-id
  {
    current-owner: principal,
    purchase-price: uint,
    purchase-timestamp: uint,
    previous-owner: (optional principal),
    total-transfers: uint
  }
)

;; Marketplace listings: artworks available for sale
(define-map marketplace-listings
  uint ;; artwork-id
  {
    seller: principal,
    price: uint,
    is-for-sale: bool,
    listed-at: uint
  }
)

;; Perpetual licensing agreements: ongoing royalty tracking
(define-map licensing-agreements
  uint ;; license-id
  {
    artwork-id: uint,
    licensor: principal,
    licensee: principal,
    license-type: (string-ascii 64), ;; commercial, personal, derivative
    royalty-rate: uint,
    payment-frequency: uint, ;; blocks between payments
    total-paid: uint,
    is-active: bool,
    created-at: uint,
    last-payment: uint
  }
)

;; Creator earnings: accumulated royalties and sales
(define-map creator-earnings
  principal
  {
    total-primary-sales: uint,
    total-royalties: uint,
    total-artworks: uint,
    withdrawal-available: uint
  }
)

;; Collector profiles: verification and collection stats
(define-map collector-profiles
  principal
  {
    total-collected: uint,
    total-spent: uint,
    verified-collector: bool,
    joined-at: uint
  }
)

;; Provenance chain: complete ownership history
(define-map provenance-records
  { artwork-id: uint, transfer-index: uint }
  {
    from-owner: principal,
    to-owner: principal,
    sale-price: uint,
    timestamp: uint,
    transaction-hash: (buff 32)
  }
)

;; Read-only functions

(define-read-only (get-artwork (artwork-id uint))
  (map-get? artworks artwork-id)
)

(define-read-only (get-artwork-owner (artwork-id uint))
  (map-get? artwork-ownership artwork-id)
)

(define-read-only (get-marketplace-listing (artwork-id uint))
  (map-get? marketplace-listings artwork-id)
)

(define-read-only (get-licensing-agreement (license-id uint))
  (map-get? licensing-agreements license-id)
)

(define-read-only (get-creator-earnings (creator principal))
  (map-get? creator-earnings creator)
)

(define-read-only (get-collector-profile (collector principal))
  (map-get? collector-profiles collector)
)

(define-read-only (get-provenance-record (artwork-id uint) (transfer-index uint))
  (map-get? provenance-records { artwork-id: artwork-id, transfer-index: transfer-index })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-read-only (calculate-royalty (sale-price uint) (royalty-percentage uint))
  (/ (* sale-price royalty-percentage) u10000)
)

(define-read-only (calculate-platform-fee (sale-price uint))
  (/ (* sale-price (var-get platform-fee-percentage)) u10000)
)

;; Public functions

;; Mint new artwork with IPFS integration
(define-public (mint-artwork 
    (title (string-utf8 256))
    (ipfs-hash (string-ascii 128))
    (metadata-uri (string-ascii 256))
    (edition-size uint)
    (royalty-percentage uint))
  (let
    (
      (artwork-id (var-get next-artwork-id))
      (caller tx-sender)
    )
    (asserts! (<= royalty-percentage max-royalty-percentage) err-invalid-royalty)
    (asserts! (> edition-size u0) err-invalid-price)
    
    ;; Create artwork record
    (map-set artworks artwork-id {
      creator: caller,
      title: title,
      ipfs-hash: ipfs-hash,
      metadata-uri: metadata-uri,
      creation-timestamp: block-height,
      edition-size: edition-size,
      current-edition: u1,
      royalty-percentage: royalty-percentage,
      is-active: true
    })
    
    ;; Set initial ownership
    (map-set artwork-ownership artwork-id {
      current-owner: caller,
      purchase-price: u0,
      purchase-timestamp: block-height,
      previous-owner: none,
      total-transfers: u0
    })
    
    ;; Initialize or update creator earnings
    (match (map-get? creator-earnings caller)
      existing-earnings
        (map-set creator-earnings caller
          (merge existing-earnings {
            total-artworks: (+ (get total-artworks existing-earnings) u1)
          })
        )
      (map-set creator-earnings caller {
        total-primary-sales: u0,
        total-royalties: u0,
        total-artworks: u1,
        withdrawal-available: u0
      })
    )
    
    ;; Increment artwork counter
    (var-set next-artwork-id (+ artwork-id u1))
    
    (ok artwork-id)
  )
)

;; List artwork for sale on marketplace
(define-public (list-artwork-for-sale (artwork-id uint) (price uint))
  (let
    (
      (caller tx-sender)
      (ownership (unwrap! (map-get? artwork-ownership artwork-id) err-not-found))
      (artwork (unwrap! (map-get? artworks artwork-id) err-not-found))
    )
    (asserts! (is-eq caller (get current-owner ownership)) err-unauthorized)
    (asserts! (> price u0) err-invalid-price)
    (asserts! (get is-active artwork) err-not-found)
    
    (ok (map-set marketplace-listings artwork-id {
      seller: caller,
      price: price,
      is-for-sale: true,
      listed-at: block-height
    }))
  )
)

;; Remove artwork from marketplace
(define-public (delist-artwork (artwork-id uint))
  (let
    (
      (caller tx-sender)
      (listing (unwrap! (map-get? marketplace-listings artwork-id) err-not-found))
    )
    (asserts! (is-eq caller (get seller listing)) err-unauthorized)
    
    (ok (map-set marketplace-listings artwork-id
      (merge listing { is-for-sale: false })
    ))
  )
)

;; Purchase artwork with automatic royalty distribution
(define-public (purchase-artwork (artwork-id uint) (tx-hash (buff 32)))
  (let
    (
      (caller tx-sender)
      (listing (unwrap! (map-get? marketplace-listings artwork-id) err-not-found))
      (artwork (unwrap! (map-get? artworks artwork-id) err-not-found))
      (ownership (unwrap! (map-get? artwork-ownership artwork-id) err-not-found))
      (sale-price (get price listing))
      (creator (get creator artwork))
      (seller (get seller listing))
      (royalty-amount (calculate-royalty sale-price (get royalty-percentage artwork)))
      (platform-fee (calculate-platform-fee sale-price))
      (seller-proceeds (- (- sale-price royalty-amount) platform-fee))
    )
    (asserts! (get is-for-sale listing) err-artwork-not-for-sale)
    (asserts! (not (is-eq caller seller)) err-unauthorized)
    
    ;; Transfer payment to seller
    (try! (stx-transfer? seller-proceeds caller seller))
    
    ;; Transfer royalty to creator (if not primary sale)
    (if (not (is-eq seller creator))
      (begin
        (try! (stx-transfer? royalty-amount caller creator))
        
        ;; Update creator royalty earnings
        (match (map-get? creator-earnings creator)
          existing-earnings
            (map-set creator-earnings creator
              (merge existing-earnings {
                total-royalties: (+ (get total-royalties existing-earnings) royalty-amount),
                withdrawal-available: (+ (get withdrawal-available existing-earnings) royalty-amount)
              })
            )
          true
        )
      )
      ;; Primary sale - update creator primary sales
      (match (map-get? creator-earnings creator)
        existing-earnings
          (map-set creator-earnings creator
            (merge existing-earnings {
              total-primary-sales: (+ (get total-primary-sales existing-earnings) sale-price),
              withdrawal-available: (+ (get withdrawal-available existing-earnings) seller-proceeds)
            })
          )
        true
      )
    )
    
    ;; Transfer platform fee
    (try! (stx-transfer? platform-fee caller contract-owner))
    
    ;; Update ownership record
    (map-set artwork-ownership artwork-id {
      current-owner: caller,
      purchase-price: sale-price,
      purchase-timestamp: block-height,
      previous-owner: (some seller),
      total-transfers: (+ (get total-transfers ownership) u1)
    })
    
    ;; Record provenance
    (map-set provenance-records
      { artwork-id: artwork-id, transfer-index: (get total-transfers ownership) }
      {
        from-owner: seller,
        to-owner: caller,
        sale-price: sale-price,
        timestamp: block-height,
        transaction-hash: tx-hash
      }
    )
    
    ;; Update marketplace listing
    (map-set marketplace-listings artwork-id
      (merge listing { is-for-sale: false })
    )
    
    ;; Update or create collector profile
    (match (map-get? collector-profiles caller)
      existing-profile
        (map-set collector-profiles caller
          (merge existing-profile {
            total-collected: (+ (get total-collected existing-profile) u1),
            total-spent: (+ (get total-spent existing-profile) sale-price)
          })
        )
      (map-set collector-profiles caller {
        total-collected: u1,
        total-spent: sale-price,
        verified-collector: false,
        joined-at: block-height
      })
    )
    
    (ok true)
  )
)

;; Create perpetual licensing agreement
(define-public (create-license-agreement
    (artwork-id uint)
    (licensee principal)
    (license-type (string-ascii 64))
    (royalty-rate uint)
    (payment-frequency uint))
  (let
    (
      (caller tx-sender)
      (artwork (unwrap! (map-get? artworks artwork-id) err-not-found))
      (ownership (unwrap! (map-get? artwork-ownership artwork-id) err-not-found))
      (license-id (var-get next-license-id))
    )
    (asserts! (is-eq caller (get current-owner ownership)) err-unauthorized)
    (asserts! (<= royalty-rate max-royalty-percentage) err-invalid-royalty)
    
    (map-set licensing-agreements license-id {
      artwork-id: artwork-id,
      licensor: caller,
      licensee: licensee,
      license-type: license-type,
      royalty-rate: royalty-rate,
      payment-frequency: payment-frequency,
      total-paid: u0,
      is-active: true,
      created-at: block-height,
      last-payment: block-height
    })
    
    (var-set next-license-id (+ license-id u1))
    
    (ok license-id)
  )
)

;; Process licensing royalty payment
(define-public (pay-license-royalty (license-id uint) (payment-amount uint))
  (let
    (
      (caller tx-sender)
      (license (unwrap! (map-get? licensing-agreements license-id) err-not-found))
      (artwork (unwrap! (map-get? artworks (get artwork-id license)) err-not-found))
      (licensor (get licensor license))
    )
    (asserts! (is-eq caller (get licensee license)) err-unauthorized)
    (asserts! (get is-active license) err-unauthorized)
    (asserts! (> payment-amount u0) err-invalid-price)
    
    ;; Transfer royalty payment
    (try! (stx-transfer? payment-amount caller licensor))
    
    ;; Update license agreement
    (map-set licensing-agreements license-id
      (merge license {
        total-paid: (+ (get total-paid license) payment-amount),
        last-payment: block-height
      })
    )
    
    ;; Update creator earnings
    (match (map-get? creator-earnings (get creator artwork))
      existing-earnings
        (map-set creator-earnings (get creator artwork)
          (merge existing-earnings {
            total-royalties: (+ (get total-royalties existing-earnings) payment-amount),
            withdrawal-available: (+ (get withdrawal-available existing-earnings) payment-amount)
          })
        )
      true
    )
    
    (ok true)
  )
)

;; Verify collector status (admin function)
(define-public (verify-collector (collector principal))
  (let
    (
      (profile (unwrap! (map-get? collector-profiles collector) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (ok (map-set collector-profiles collector
      (merge profile { verified-collector: true })
    ))
  )
)

;; Withdraw creator earnings
(define-public (withdraw-earnings (amount uint))
  (let
    (
      (caller tx-sender)
      (earnings (unwrap! (map-get? creator-earnings caller) err-not-found))
    )
    (asserts! (>= (get withdrawal-available earnings) amount) err-insufficient-payment)
    
    (try! (as-contract (stx-transfer? amount tx-sender caller)))
    
    (ok (map-set creator-earnings caller
      (merge earnings {
        withdrawal-available: (- (get withdrawal-available earnings) amount)
      })
    ))
  )
)

;; Update artwork metadata URI
(define-public (update-metadata-uri (artwork-id uint) (new-metadata-uri (string-ascii 256)))
  (let
    (
      (caller tx-sender)
      (artwork (unwrap! (map-get? artworks artwork-id) err-not-found))
    )
    (asserts! (is-eq caller (get creator artwork)) err-unauthorized)
    
    (ok (map-set artworks artwork-id
      (merge artwork { metadata-uri: new-metadata-uri })
    ))
  )
)

;; Admin functions

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-royalty) ;; Max 10% fee
    (ok (var-set platform-fee-percentage new-fee))
  )
)

;; Initialize contract
(begin
  (print "CreateCove Labs contract initialized")
)