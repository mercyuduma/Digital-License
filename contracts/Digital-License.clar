;; contracts/digital-license.clar

(define-constant ERR_NOT_AUTHORITY u100)
(define-constant ERR_LICENSE_EXISTS u101)
(define-constant ERR_LICENSE_NOT_FOUND u102)
(define-constant ERR_NOT_LICENSE_OWNER u103)
(define-constant ERR_LICENSE_ALREADY_ACTIVATED u104)

(define-data-var license-counter uint u0)
(define-map licenses uint 
    (tuple (owner principal) (price uint) (activated bool)))
(define-map earnings principal uint)

(define-public (create-license (license-id uint) (price uint))
    (begin
        ;; Ensure only the contract deployer (authority) can create licenses
        (asserts! (is-eq tx-sender (as-contract tx-sender)) (err ERR_NOT_AUTHORITY))
        ;; Ensure the license doesn't already exist
        (asserts! (is-none (map-get? licenses license-id)) (err ERR_LICENSE_EXISTS))
        ;; Ensure price is valid (non-negative)
        (asserts! (>= price u0) (err u107))
        ;; Store the license with the initial state
        (map-set licenses license-id { owner: tx-sender, price: price, activated: false })
        ;; Increment license counter
        (var-set license-counter (+ (var-get license-counter) u1))
        ;; Verify the license was created successfully
        (match (map-get? licenses license-id)
            license-data (ok license-id)
            (err ERR_LICENSE_NOT_FOUND)
        )
    )
)

(define-public (acquire-license (license-id uint))
    (let (
        (license (map-get? licenses license-id))
    )
        (begin
            ;; Ensure the license exists
            (asserts! (is-some license) (err ERR_LICENSE_NOT_FOUND))
            ;; Unwrap the license data
            (let (
                (license-data (unwrap-panic license))
                (license-price (get price license-data))
                (license-owner (get owner license-data))
                (license-activated (get activated license-data))
            )
                ;; Ensure the license is not already activated
                (asserts! (not license-activated) (err ERR_LICENSE_ALREADY_ACTIVATED))
                ;; Ensure the buyer pays enough funds
                (match (stx-transfer? license-price tx-sender license-owner)
                    success
                    (begin
                        ;; Transfer the license to the buyer
                        (map-set licenses license-id { owner: tx-sender, price: license-price, activated: true })
                        ;; Update earnings for authority
                        (let ((current-earnings (default-to u0 (map-get? earnings license-owner))))
                            (map-set earnings license-owner (+ current-earnings license-price))
                        )
                        (ok license-id)
                    )
                    error (err u106)
                )
            )
        )
    )
)

(define-public (transfer-license (license-id uint) (recipient principal))
    (let (
        (license (map-get? licenses license-id))
    )
        (begin
            ;; Ensure the license exists
            (asserts! (is-some license) (err ERR_LICENSE_NOT_FOUND))
            ;; Unwrap the license data
            (let (
                (license-data (unwrap-panic license))
                (license-owner (get owner license-data))
            )
                ;; Ensure the caller owns the license
                (asserts! (is-eq license-owner tx-sender) (err ERR_NOT_LICENSE_OWNER))
                ;; Ensure recipient is not null
                (asserts! (is-some (some recipient)) (err u108))
                ;; Transfer license ownership
                (map-set licenses license-id { owner: recipient, price: (get price license-data), activated: true })
                (ok recipient)
            )
        )
    )
)

(define-public (withdraw-earnings)
    (let (
        (earned (default-to u0 (map-get? earnings tx-sender)))
    )
        (begin
            ;; Ensure the user has earnings to withdraw
            (asserts! (> earned u0) (err u105))
            ;; Transfer funds to the user
            (match (stx-transfer? earned (as-contract tx-sender) tx-sender)
                success (begin
                    ;; Reset earnings to zero only if transfer succeeded
                    (map-delete earnings tx-sender)
                    (ok earned)
                )
                error (err u106)
            )
        )
    )
)

