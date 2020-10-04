#lang racket/base
(require charterm)

(define cl 0) ; current-line
(define cc 0) ; current-column

(define (echo str)
  (charterm-display str))

(define (main)
  (with-charterm
    (clear)
    (editor)))

(define (clear)
  (charterm-clear-screen)
  (red-layout))

(define (print-inverse-line start last-column)
  (cond
    [(< start last-column)
     (echo " ")
     (print-inverse-line (add1 start) last-column)]))

;; (define (print-ruler start last-column)
  ;; (define colspan 4)
  ;; (define colspan+1 (add1 colspan))
  ;; (cond
    ;; [(= start 16)
     ;; (charterm-cursor (- start colspan) 2)
     ;; (echo (string-append "+" (number->string (- start 8))))
     ;; (print-ruler (add1 start) last-column)
     ;; (charterm-cursor start 2)
     ;; (print-ruler (add1 start) last-column)]
    ;; [(and (> start 8)  (< start last-column) (= (remainder start 8) 0))
     ;; (charterm-cursor (- start colspan+1) 2)
     ;; (echo (string-append "+" (number->string (- start 8))))
     ;; (charterm-cursor start 2)
     ;; (print-ruler (add1 start) last-column)]
    ;; [(< start last-column)
     ;; (echo " ")
     ;; (print-ruler (add1 start) last-column)]))


(define (print-ruler start last-column)
  (define colspan 4)
  (define colspan+1 (add1 colspan))
  (cond
    [(= start 16)
     (charterm-cursor (- start colspan) 2)
     (echo (string-append "+" (number->string (- start 8))))
     (print-ruler (add1 start) last-column)
     (charterm-cursor start 2)
     (print-ruler (add1 start) last-column)]
    [(and (> start 8)  (< start last-column) (= (remainder start 8) 0))
     (charterm-cursor (- start colspan+1) 2)
     (echo (string-append "+" (number->string (- start 8))))
     (charterm-cursor start 2)
     (print-ruler (add1 start) last-column)]
    [(and (> start 4) (< start last-column))
     (echo " ")
     (print-ruler (add1 start) last-column)]
    [(< start last-column) (charterm-inverse)
          (echo " ")
          (charterm-normal)
          (print-ruler (add1 start) last-column)]))

(define (red-layout)
  ;; Title
  (charterm-inverse)
  (print-inverse-line 1 86)
  (charterm-cursor 6 1)
  (echo "red")
  (charterm-cursor 12 1)
  (echo "> ")
  (charterm-normal)
  ;; Ruler
  (charterm-cursor 1 2)
  (print-ruler 1 96))

(define (line-numbering cl)
  (charterm-cursor 1 cl)
  (charterm-inverse)
  (echo "    ")
  (define cl-str (number->string (- cl 2))) ; first line needs to be `1`, not `3`
  (charterm-cursor (- 5 (string-length cl-str)) cl)
  (echo cl-str)
  (charterm-normal)
  (set! cc 6)
  (charterm-cursor cc cl))

(define (editor)
  (set! cl 3)
  (line-numbering cl)
  (wait-for-key))

(define (quit)
  (charterm-newline)
  (charterm-display "C-x")
  (charterm-newline)
  (close-charterm))


(define (wait-for-key)
  (let ((key (charterm-read-key)))
    (cond
      [(equal? key 'return) (set! cl (add1 cl)) (line-numbering cl) (wait-for-key)]
      [(equal? key 'ctrl-x) (quit)]
      [else (echo key) (wait-for-key)])))

(main)