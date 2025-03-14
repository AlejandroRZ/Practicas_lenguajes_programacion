#lang racket

;Ejercicio 6: Funcion que dado un numero devuelve 1 usando la conjetura de collatz
(define (collatz n) 
    (aux-collatz n '() ))

;Funcion auxiliar para la funcion collatz 
 (define (aux-collatz x lst)
    (cond
          [(= x 1) (reverse (cons x lst))]
          [(even? x) (aux-collatz (/ x 2) (cons x lst))]
          [else (aux-collatz (+ (* x 3) 1) (cons x lst))]
          ))
