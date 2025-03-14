#lang racket

;;Definir la función multiplica que dada una lista, multiplica todos los elementos contenidos en la misma.
(define (Multiplica ls)
  (cond
    [(empty? ls) 1] 
    [else (* (car ls) (Multiplica (cdr ls)))]))