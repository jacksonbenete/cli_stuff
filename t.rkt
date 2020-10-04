#lang racket/base
(require racket/string)
(require charterm)
(require racket/list)
(require test-engine/racket-tests)

(define dice-result 0)

; Number -> Number
; roll 1dy
(define (dice y) (+ 1 (random y)))

; String -> List
; roll xdy dices and return a list of results
(define (roll x y)
  (define result-list '())
  (map (lambda (i) (cons (dice y) result-list)) (range x)))

; Roll 2d6 dice.
(define (roll-save)
  ;; (clear)
  ;; (newline 1)
  (define roll-list '())
  (set! roll-list (roll 2 6))
  (set! dice-result (apply + roll-list))
  ;; (set! dice-result (+ (random 6) (random 6) 1))
  ;; (define res (number->string dice-result))
  )

(check-range (dice 6) 1 6)
;; (check-expect (roll 2 6) list)
;; (check

;; (test)