(impl-trait .sip009-nft-trait.sip009-nft-trait)


;; constant for the contract deployer and two error codes.
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-id-failure (err u102))

;;NFT definition
(define-non-fungible-token stacksies uint)


;; counter variable each time a new NFT is minted
(define-data-var token-id-nonce uint u0)


;; get-last-token-id
(define-read-only (get-last-token-id)
	(ok (var-get token-id-nonce))
)


;;return a link to metadata for the specified NFT
(define-read-only (get-token-uri (token-id uint))
	(ok none)
)


;; wrap the built-in nft-get-owner?.
(define-read-only (get-owner (token-id uint))
	(ok (nft-get-owner? stacksies token-id))
)

;; The transfer function
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
	(begin
		(asserts! (is-eq tx-sender sender) err-not-token-owner)
		(nft-transfer? stacksies token-id sender recipient)
	)
)


;; Mint


(define-public (mint (recipient principal))
	(let ((token-id (+ (var-get token-id-nonce) u1)))
		(asserts! (is-eq tx-sender contract-owner) err-owner-only)
		(try! (nft-mint? stacksies token-id recipient))
		(asserts! (var-set token-id-nonce token-id) err-token-id-failure)
		(ok token-id)
	)
)


