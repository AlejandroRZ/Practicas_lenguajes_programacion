#lang plai
;Practica 04 de laboratorio
;Clase: Lenguajes de programación.
;Alumnos: Javier Alejandro Rivera Zavala, Gabriela López Diego, Juan Daniel San Martín Macias, Abraham Jimenéz Reyes.

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Función correspondiente al algoritmo de sustitución
(define (subst expr idx value)
  (type-case WBAE expr
    [id (i) (if (symbol=? i idx) value expr)]
    [num (n) expr]
    [op (f args) (op f (map (lambda (w) (subst w idx value)) args))]
    [bool (b) expr]
    [chaR (c) expr]
    [strinG (s) expr]
    [lst (l) ( (map(lambda (x) (subst x idx value)) l))] 
    [with (bindings body)
          (let ([new-bindings (map (lambda (b) (subst-value-binding b idx value)) bindings)])
            (with new-bindings (subst body idx value)))]
    [with* (bindings body)
          (let ([new-bindings (map (lambda (b) (subst-value-binding b idx value)) bindings)])
            (with* new-bindings (subst body idx value)))]))


;Funcion que hace la sustitucion en el value de un binding
(define (subst-value-binding bind sub-id val)
  (type-case Binding bind
    [binding (id value) (binding id (subst value sub-id val))]))


;Funcion que nos ayuda a cambiar una expresion de tipo with* a tipo with's anidados 
(define (withAnidados expr)
  (type-case WBAE expr
    [id (i) expr]
    [num (n) expr]
    [op (f args) (op f (map withAnidados args))]
    [bool (b) expr]
    [chaR (c) expr]
    [strinG (s) expr]
    [lst (l) (lst (map withAnidados l))]
    [with (bindings body) ""]
    [with* (bindings body)
      (let* ([first-binding (car bindings)] [rest-bindings (cdr bindings)] [inner-with (if (null? rest-bindings) body (withAnidados (with* rest-bindings body)))])
         (with (list first-binding) inner-with))]))


;; Toma un árbol de sintáxis abstraca del lenguaje WBAE.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
;Este es el ejercicio 5
(define (interp expr) 
  (type-case WBAE expr 
    [id (i) (error 'interp "Variable libre")]
    [num (n) (num n)]
    [op (f args) (if (equal? f anD)  (anD args) 
                     (if (equal? f oR)  (oR args)
                         (num ( apply f (map (lambda (elemento) (match elemento [(num n) n])) args)))))]
    [bool (b) (bool b) ]
    [chaR (c) (chaR c) ]
    [strinG (s) (strinG s) ]
    [lst (l) (lst (map interp l))]
    [with (bindings body)
          (let* ([new-bindings (map (lambda (b) (interp-value-binding b)) bindings)]
                 [new-body (apply-subst-bindings new-bindings body)])
                 (interp new-body))]
     [with* (bindings body) (interp (interp (withAnidados (with* bindings body))))]))


;Funcion que interpreta el valor asociado de un binding 
(define (interp-value-binding bind)
  (type-case Binding bind
    [binding (id value) (binding id (interp value))]))

;Funcion que aplica de forma recursiva subst (con los bindings que tengamos en la lista dada) en body
(define (apply-subst-bindings bind-list body)
  (cond
    [(empty? bind-list) body]
    [else (let ([bind (car bind-list)])
       (apply-subst-bindings (cdr bind-list) (subst body (binding-id bind) (binding-value bind))))]))

;Funcion que obtiene el identificador de un binding
(define (binding-id bind)
  (type-case Binding bind
    [binding (id value) id]))

;Funcion que obtiene el valor asociado de un binding
(define (binding-value bind)
  (type-case Binding bind
    [binding (id value) value]))


;-----------------Pruebas-----------------------
;(interp (parse 'foo))
#|
(interp (parse 12345))
(interp (parse true))
(interp (parse "Jisoo"))
(interp (parse #\B))
(interp (parse '(+ 1 2 3 4 5))) ;Devuelve 15
(interp (parse '(- 1 2 3 4 5))) ;Devuelve -13
(interp (parse '(* 1 2 3 4 5))) ;Devuelve 120
(interp (parse '(/ 1 2 3 4 5))) ;Devuelve 1/120
(interp (op anD (list (bool #t)(bool #t) (bool #t)(bool #t))))
(interp (op oR (list (bool #f) (bool #f)(bool #f)(bool #f))))
(interp (oR (list (bool #f) (bool #t)(bool #f))))
(interp (parse '(1 2 3)))
(interp (parse '(1 #\C 3)))
(subst (parse '{with ([x 2] [y 3]) (+ x y)}) 'x (num 7))
(interp (parse '{with {{a 2} {b 7}} {+ a b}}))
(interp (parse '{with* { {a 2} {b {+ a a}} } b} ))
(interp (parse '{with ([x 2] [y 3]) (+ x y)}))
|#



