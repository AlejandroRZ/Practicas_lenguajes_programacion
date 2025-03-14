
#lang racket

;; decremental? :: number number number number -> boolean
(define (decremental? a b c d)
  (and (> a b) (> b c) (> c d))); Devolvemos true si a es mayor a b, b es mayor a c y c es mayor a d de otra forma refresamos f

(decremental? 1 2 3 4)

(decremental? 657 4 3 1)

(decremental? 56 3 45 2)
