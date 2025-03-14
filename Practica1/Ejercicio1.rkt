#lang racket

;; area-total :: number number -> number

(define (area-cono g d)
  ;Definimos el radio, dividiendo el diametro entre 2
  (let ([r (/ d 2)])
    ;Multiplicamos pi * radio * la generatriz del cono
    (+ (* pi r g)
       ;sumamos el resultado anterior mas radio al cuadrado por pi
       (* pi (expt r 2))))) 