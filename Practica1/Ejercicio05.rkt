#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ejercicio 05: Definir el predicado pares? que dada una lista,
;;indica si todos los elementos contenidos en ésta son pares.

(define (pares? ls)
  (if(empty? ls) true
     (if(verifica (car ls)) (pares? (cdr ls)) false)))

;;Función auxiliar que se encarga de hacer las operaciones matemáticas y verifica que un número
;;es par o no dependiendo si el modulo entre ese número y 2 es igual a cero
(define(verifica n)
  (= (modulo n 2) 0))