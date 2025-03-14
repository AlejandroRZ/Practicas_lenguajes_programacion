#lang plai
;; Definición de los tipos de datos pilas y colas. Para esto, utilizamos un solo ADT mediante el cual modelaremos
;; el comportamiento de cada una de las instancias correspondientes.

;; Definición de tipo de dato Nodo, necesario para definir pilas-y-colas
(define-type Nodo
  [Void] ;; Nodo vacío
  [nodo (elem number?) (siguiente Nodo?)]) ;; Nodo con un elemento y referencia al siguiente nodo en la estructura

;;Definiendo pilas y colas
(define-type pilas-y-colas
  [pila (nodos Nodo?)]  ;; Definiendo pila
  [cola (nodos Nodo?)]) ;; Definiendo cola

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Definición del tipo de dato abstracto "arboles binarios de busqueda"
;; podemos insertar elementos en el arbol de manera "ordenada" de tal forma que yo pueda
;; acceder a ellos en tiempo del orden de O(log n), donde n es el numero de elementos en el arbol

; Definicion del tipo de dato abstracto 
(define-type binary-search-tree
  [VoidTree] ;; Arbol vacío
  [BST (elem number?) (lst binary-search-tree?) (rst binary-search-tree?)]) ;;Arbol-nodo no vacío, y subárboles izquierdo y derecho


;;predicado acepta cualquier tipo y devuelve true
(define (any? x) #t)

;Tipo de dato abstracto para listas
(define-type List
      [Empty] ;;lista vacia
      [Cons (cabeza any?) (cola List?)]) ;; Cabeza y cola de la lista
