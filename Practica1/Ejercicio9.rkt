#lang racket
;Ejercicio 9: Funcion que dada una lista de numeros, regresa la misma lista
;con unicamente los numeros primos
(define (primos lst)
  (filter es-primo? lst))

;Funcion que determina si un numero es primo o no
(define (es-primo? n)
  (cond
        [(<= n 1)  #f] ;CASO CASE 1
        [(= n 2)   #t] ;CASO BASE 2
        [(even? n) #f]  
        [else (let bucle ((d 3))        
                (cond
                      [(> d (sqrt n))       #t]
                      [(zero? (modulo n d)) #f]
                      [else (bucle (+ d 2))]
                      ))]
        ))