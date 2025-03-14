#lang racket

;Ejercicio 8: Funcion que dada una lista de numeros enteros regresa la
;representacion binaria en cadena de cada uno de los numeros 
(define (binarios lista)
  (cond
        [(null? lista) '()]
        [else (cons (number->string (car lista) 2) (binarios (cdr lista)) )]
        )) 

