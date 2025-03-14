#lang plai
(require "datatypes.rkt")


;Ejercicio 4a); Dada una lista y un elemento, devuelve True si el elemento
;se encuentra en la lista o False en caso contrario.
(define (contains-List? elem Lst)
   (type-case List Lst
       [Empty () #f]
       [Cons (cabeza cola)
            (if (equal? elem cabeza)
                #t
               (contains-List? elem cola))]))


;Ejercicio 4b): Dada una lista, devuelve su longitud.
(define (length-List Lst)
  (type-case List Lst
    [Empty () 0]
    [Cons (cabeza cola)(+ 1 (length-List cola))]))


;Ejercicio 4c): Dada una lista y un elemento. Construye una nueva lista
;donde el elemento es la nueva cabeza de la lista. 
(define (add-List elem Lst)
         (Cons elem Lst))