#lang plai
;; Importando las definiciones de los tipos de datos
(require "datatypes.rkt")
#|Ejercicio 1)
a) filter: Esta función recibe un predicado y una lista (es de esperar que el predicado sea aplicable a los elementos de la lista), recorre la lista pasada como argumento y verifica uno a uno, si los elementos en ella cumplen con el predicado. Retorna una sublista de la original que contiene a aquellos elementos que si cumplen con el predicado.
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
'(4 3 2 1)

|#

#|Función que verifica si un determinado elemento de tipo númerico es parte de
la estructura de datos pasada como primer parámetro (está contenido en alguno de
sus nodos).|#
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

#|Función que devuelve el elemento en el tope de la estructura pasada como primer
parámetro, en el caso de la pila se trata del último en ser apilado y en el caso
de la cola se trata del primero en ser añadido a la cola.|#
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
#|Función que devuelve un entero que nos indica el número de elementos contenidos en la
estructura de datos.|#
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


