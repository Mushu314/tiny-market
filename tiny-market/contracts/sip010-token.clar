(impl-trait .sip010-ft-trait.sip010-ft-trait)


;; token definition, a constant for the contract deployer, and two error codes
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))

;; No maximum supply!
(define-fungible-token amazing-coin)

;; The transfer function
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
	(begin
		(asserts! (is-eq tx-sender sender) err-owner-only)
		(try! (ft-transfer? amazing-coin amount sender recipient))
		(match memo to-print (print to-print) 0x)
		(ok true)
	)
)

;; returns a human-readable name for our token.
(define-read-only (get-name)
	(ok "Amazing Coin")
)

;; returns a human-readable symbol for our token

(define-read-only (get-symbol)
	(ok "AC")
)

;; he value returned by this function is purely for display reasons

(define-read-only (get-decimals)
	(ok u6)
)

;; wrap the built-in function that retrieves the balance.

(define-read-only (get-balance (who principal))
	(ok (ft-get-balance amazing-coin who))
)

;; function returns the total supply of our custom token.
(define-read-only (get-total-supply)
	(ok (ft-get-supply amazing-coin))
)

;; URI  Our practice fungible token does not have a website so we can return none.

(define-read-only (get-token-uri)
	(ok none)
)


;; 




(define-public (mint (amount uint) (recipient principal))
	(begin
		(asserts! (is-eq tx-sender contract-owner) err-owner-only)
		(ft-mint? amazing-coin amount recipient)
	)
)