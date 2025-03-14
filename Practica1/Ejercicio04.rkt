#lang racket

;;Ejercicio 4: Definir la función area-heron que calcula el área de un triángulo a
;;partir de la medida de cada uno de sus lados. La formula de Herón es la siguiente:

;Definción de la función
(define (area-heron a b c)
  (sqrt(* (S a b c) (- (S a b c) a) (- (S a b c) b) (- (S a b c) c))))

;Definición de la constante S que se utiliza en la fórmula que se pasa en el PDF
(define(S a b c)
  (/ (+ a b c) 2))
