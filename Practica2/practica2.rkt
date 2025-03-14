#lang plai
;; Importando las definiciones de los tipos de datos
(require "datatypes.rkt")
(require "03-07-23.rkt")
;; Recuerda realizar en orden ejercicios correspondientes a la práctica


;Ejercicio1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|a) filter: Esta función recibe un predicado y una lista (es de esperar que el predicado sea aplicable a los elementos de la lista), recorre la lista pasada como argumento y verifica uno a uno, si los elementos en ella cumplen con el predicado. Retorna una sublista de la original que contiene a aquellos elementos que si cumplen con el predicado.
Su firma es (filter pred lst) → list?
Ejemplos:
>(filter mayuscula? '(A b c d e F G))
	'(A F G)

>(filter entero? '(3 3.14 2.96 0.5 60 761 0 1.1 4.7))
'(3 60 761 0)

b) foldr: Se trata de una función de plegado, recibe un procedimiento o función, un valor/estructura de inicio o semilla y un conjunto de listas a recorrer. Recorre el conjunto de listas de derecha a izquierda y le aplica a cada tupla de valores el procedimiento pasado como parámetro, acumula los resultados del procedimiento sobre el valor de inicio a cada paso. Al final, retorna el que fuera un valor de inicio pero con todos los resultados acumulados sobre él. 
su firma es (foldr proc init lst ...+) → any/c, inicialmente la función se manda a llamar con el primer elemento en cada lista y el argumento final es init. En las llamadas posteriores el último argumento es el valor devuelto por proc durante la llamada anterior de la función.
Ejemplos:
>(foldr + 1 '(1 1 1 1 1) )
 6
>(foldr cons '() '(1 2 3 4))
'(1 2 3 4)

c) foldl: Funciona de manera similar a foldr, recorre un conjunto de listas sobre las que aplica un procedimiento y acumula los resultados sobre un argumento. La diferencia principal radica en que foldl hace el recorrido de izquierda a derecha y por lo tanto la acumulación podría diferir.
Su firma es (foldl proc init lst ...+) → any/c, al igual que en foldr el argumento init tiene un valor inicial y luego la función se manda a llamar con el resultado de la aplicación anterior de proc a cada paso.
Ejemplos:
>(foldl  + 0 '(5 6 7 8))
26

> (foldl cons '() '(1 2 3 4))
'(4 3 2 1)|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;Ejercicio2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Función que verifica si un determinado elemento de tipo númerico es parte de
;;la estructura de datos pasada como primer parámetro (está contenido en alguno de
;;sus nodos).
(define (contains? struct element)
  (cond
   [(pila? struct)
          (cond
            [(Void? (pila-nodos struct)) #f]
            [(nodo? (pila-nodos struct)) (or (equal? element (nodo-elem (pila-nodos struct)))
                                             (contains? (pila (nodo-siguiente (pila-nodos struct))) element))]
           )
   ]
   [(cola? struct)
         (cond
            [(Void? (cola-nodos struct)) #f]
            [(nodo? (cola-nodos struct)) (or (equal? element (nodo-elem (cola-nodos struct)))
                                             (contains? (cola (nodo-siguiente (cola-nodos struct))) element))]
          )
   ]
   [(binary-search-tree? struct)   ;;Añadimos opcionalmente la verificación para árboles binarios de búsqueda. 
         (cond
            [(VoidTree? struct) #f]
            [(BST? struct) (or
                            (equal? element (BST-elem struct))
                            (contains? (BST-lst struct) element)
                            (contains? (BST-rst struct) element)
                            )]
         )    
   ]
  )  
)

;;Función que devuelve el elemento en el tope de la estructura pasada como primer
;;parámetro, en el caso de la pila se trata del último en ser apilado y en el caso
;;de la cola se trata del primero en ser añadido a la cola.
(define (mira struct)
      (cond
        [(pila? struct)
         (cond
           [(Void? (pila-nodos struct)) "Nada"]
           [(nodo? (pila-nodos struct))
            (if (Void? (nodo-siguiente (pila-nodos struct)))
                (nodo-elem (pila-nodos struct))
                (mira (pila (nodo-siguiente (pila-nodos struct))))
             )
           ]
         )         
        ]
        [(cola? struct)
         (cond
           [(Void? (cola-nodos struct)) "Nada"]
           [(nodo? (cola-nodos struct)) (nodo-elem (cola-nodos struct))]
         )        
        ]
      )
 )
;;Función que devuelve un entero que nos indica el número de elementos contenidos en la
;;estructura de datos.
(define (size struct)
  (cond
   [(pila? struct)
    (cond
      [(Void? (pila-nodos struct)) 0]
      [(nodo? (pila-nodos struct)) (+ 1 (size (pila (nodo-siguiente (pila-nodos struct)))))]
     )
   ]
   [(cola? struct)
    (cond
      [(Void? (cola-nodos struct)) 0]
      [(nodo? (cola-nodos struct)) (+ 1 (size (cola (nodo-siguiente (cola-nodos struct)))))]
     )
    ]
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Ejercicio3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a)Define la función (delete-bst bst elem) que dado un BST y un elemento, lo elimina del BST y entrega
;como resultado el BST producto de realizar dicha operación.

;;Función que elimina el elemento del árbol, regresa una cadena en caso de que el elemento no esté en el árbol.
(define (delete-bst bst elem)
 (cond 
    [(search-in-bst bst elem) (auxiliar-delete-bst bst elem)]
    [else "El elemento no está en el árbol"]))

;;FUnción encargada de localizar el elemento a eliminar
(define (auxiliar-delete-bst bst elem)
  (type-case binary-search-tree bst
    [VoidTree () ""]
    [BST (e l r)
         (cond
           [(eq? e elem) (revisar-nodo bst)]                                ;; si ya encontré a mi elemento, entonces reviso si la cantidad de hijos que tiene
           [(< elem e) (BST e (auxiliar-delete-bst l elem) r) ]             ;; si el elemento a buscar es menor, entonces hago recursión en mi subarbol izquierdo 
           [else (BST e l (auxiliar-delete-bst r elem))])]))

;;Función que revisa el caso que es el elemento a eliminar (con o sin hijos).
(define (revisar-nodo bst)
 (type-case binary-search-tree bst
    [VoidTree () ""]
    [BST (e l r)
         (cond               
           [(esHoja? bst)(VoidTree)]                     ;Caso en que sea hoja
           [(hijoIzq? bst)(eq? (hijoDer? bst) #f) l]     ;Caso en que tenga sólo hijo izquierdo
           [(hijoDer? bst)(eq? (hijoIzq? bst) #f) r]     ;Caso en que tenga sólo hijo derecho
           [(hijoDer? bst)(hijoIzq? bst) (BST (numMaxI l) (delete-bst l (numMaxI l)) r)])]))

;;Encuentra el hijo mayor del subarbol izquierdo 
(define (numMaxI bst)
 (type-case binary-search-tree bst
    [VoidTree () empty]
    [BST (e l r)
         (if (VoidTree? r) e (numMaxI r))]))


;;Función que te dice si tiene hijo izquierdo o no, regres true en caso de tenerlo y false en cualquier otro caso
(define (hijoIzq? bst)
  (type-case binary-search-tree bst
    [VoidTree () #f]
    [BST (e l r)
         (if (VoidTree? l)#f #t)]))

;;Función que te dice si tiene hijo izquierdo o no, regres true en caso de tenerlo y false en cualquier otro caso
(define (hijoDer? bst)
  (type-case binary-search-tree bst
    [VoidTree () #f]
    [BST (e l r)
         (if (VoidTree? r)#f #t)]))

;;Función que te dice si tiene hijos o no, regresa true en caso de no tener hijos y false en cualquier otro caso
(define (esHoja? bst)
  (type-case binary-search-tree bst
    [VoidTree () #f]
    [BST (e l r)
         (if (VoidTree? l)
             (if(VoidTree? r) #t #f)
             #f)]))

;b) Define la función (count-leaves-bst bst) que dado un BST, devuelve el número de hojas de éste. Re-
;cuerda que una hoja es aquel nodo que tanto su subárbol izquierdo como derecho son void.

;función que cuenta si son hojas o no

(define (count-leaves-bst bst)
  (auxiliar-count bst ))

(define (auxiliar-count bst)
  (type-case binary-search-tree bst
    [VoidTree () 0]
    [BST (e l r)
         (cond
           [(hijoDer? bst)(hijoIzq? bst)(auxiliar-count r)(auxiliar-count l)]
           [(esHoja? bst)(+ 1)]                                                                     ;Caso en que sea hoja
           [(hijoIzq? bst)(eq? (hijoDer? bst) #f) (auxiliar-count l)]                               ;Caso en que tenga sólo hijo izquierdo
           [(hijoDer? bst)(eq? (hijoIzq? bst) #f) (auxiliar-count r)])]))                           ;Caso en que tenga sólo hijo derecho 


;c) Define la función (map-bst fun bst) tal que da como resultado el BST que tiene como elementos al los
;elementos del árbol pasado como parámetro después de haberles aplicado fun a cada uno de ellos.

;; definiendo map sobre BST
;; map-bst :: binary-search-tree -> binary-search-tree
(define (map-bst fun bst)
  (type-case binary-search-tree bst
    [VoidTree () (VoidTree)]
    [BST (e l r) (BST (fun e) (map-bst fun l) (map-bst fun r))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Ejercicio4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;