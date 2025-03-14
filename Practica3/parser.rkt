#lang plai
;Practica 03 de laboratorio
;Clase: Lenguajes de programación.
;Alumnos: Javier Alejandro Rivera Zavala, Gabriela López Diego, Juan Daniel San Martín Macias, Abraham Jimenéz Reyes.

(require "grammars.rkt")

;; Parse :: s-expression -> WBAE
(define (parse sexp)
  (cond
     [(symbol? sexp)(id sexp)]
     [(number? sexp)(num sexp)]
     [(boolean? sexp) (bool sexp)]
     [(char? sexp)(chaR sexp)]
     [(string? sexp)(strinG sexp)]
     [(and (list? sexp) (equal? '+ (car sexp)) (esmayor1? (cdr sexp)))
      (op + (map parse (cdr sexp)))]
     [(and (list? sexp) (equal? 'or (car sexp)))
      (op oR (map parse (rest sexp)))]
     [(and (list? sexp) (equal? 'and (car sexp)))
      (op anD (map parse (rest sexp)))]
     [(and (list? sexp) (equal? 'with (car sexp)))
     (with (parse2 (second sexp)) (if (and(verificar-lista (third sexp)) (auxlist1 (third sexp)))
                                      (parse (eval (car (third sexp))) )
                                      (parse (third sexp))) )]
     [(and (list? sexp) (equal? 'with* (car sexp)))
     (with* (parse2 (second sexp))
           (cond
             [(and (verificar-lista (third sexp)) (auxlist1 (third sexp))) (parse (eval(car (third sexp))))]
             [(and (verificar-lista (third sexp)) (auxlist2 (third sexp))) (parse (car (third sexp)))]
             [(parse (third sexp))]))]
     [(list? sexp) (if (todos-simbolos? sexp) (map parse sexp) (lst(map parse sexp))) ]))

;Funcion auxiliar para parse. Dada una lista, la transforma en una lista de tipo binding. En caso de que el id
;tenga mas de un valor asociado, devolvemos error. 
(define (parse2 lst)
  (if (null? lst) '()
      (let ((cabeza (car lst)) (cola (parse2 (cdr lst))))
        (if (and (list? cabeza) (isBind? cabeza))
            (cons (binding (car cabeza) (parse (cadr cabeza))) cola) cola))))

;Funcion auxiliar para parse2. Determina si el elemento dado es de tipo binding.
(define (isBind? lista)
  (if (and (symbol? (first lista)) (or (symbol? (second lista))(number? (second lista))) (equal? (length lista) 2))
      #t
      (error (format "Binding ~a mal formado" lista))))

;Funcion auxiliar para parse2. Determina dada una lista si es de longitud 2.
(define (length-2? lista)(if(equal? (length lista) 2) #t (error "El procedimiento espera dos argumentos")))

;Funcion que determina si se recibe una lista dentro de otra lista. Devuelve T si ocurre, en otro caso F. 
(define (verificar-lista lista)
  (and (list? lista) (not (null? lista)) (list? (car lista))))

;Funcion que determina si dada una lista, todos sus elementos son simbolos. 
(define (todos-simbolos? lst)
  (andmap symbol? lst))

;Funcion que recibe una lista dentro de otra lista, devuelve true si la cabeza de la cabeza de la lista recibada 
; es 'list en otro caso, false.  
(define (auxlist1 lista)
  (if (eq? (car (car lista)) 'list) #t #f))

;Funcion que recibe una lista dentro de otra lista, devuelve true si la cabeza de la cabeza de la lista recibada 
; es no 'list en otro caso, false.
(define (auxlist2 lista)
  (if (not(eq? (car (car lista)) 'list)) #t #f))

;Funcion que dada una lista, determina si contiene mas de 1 elementos contenidos. En caso contrario, devuelve error. 
(define (esmayor1? lista) (if (> (length lista) 1) #t (error "El procedimiento espera al menos dos argumentos")))

                    