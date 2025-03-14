#lang plai
;; Importando las definiciones de los tipos de datos
(require "datatypes.rkt")

;; Recuerda realizar en orden ejercicios correspondientes a la práctica
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

;Ejercicio 4d): Dada una lista y un elemento, elimina la primera aparicion del elemento en la lista
;devuelve la lista actualizada
(define (delete-List elem Lst)
  (type-case List Lst
    [Empty () Empty]
    [Cons(cabeza cola)
         (if (equal? elem cabeza)
             cola
             (Cons cabeza (delete-List elem cola)))]))

;Ejercico 4e) : Dada una funcion y una lista, aplicamos la funcion a la lista
; devolvemos una nueva lista con los resultados aplicados
(define (map-List fun Lst)
  (type-case List Lst
    [Empty () Empty]
    [Cons (cabeza cola)
          (Cons ( fun cabeza) (map-List fun cola))]))

;Ejercico 4f) : Dada una funcion y una lista devolvemos la lista
; donde el predicado es verdadero
(define (filter-List pred Lst)
  (type-case List Lst
    [Empty () Empty]
    [Cons (cabeza cola)
           (if (pred cabeza)
               (Cons cabeza (filter-List pred cola))
               (filter-List pred cola))]))