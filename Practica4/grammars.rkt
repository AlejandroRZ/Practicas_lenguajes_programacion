#lang plai
;Practica 04 de laboratorio
;Clase: Lenguajes de programación.
;Alumnos: Javier Alejandro Rivera Zavala, Gabriela López Diego, Juan Daniel San Martín Macias, Abraham Jimenéz Reyes.

;; Definición del lenguaje WAE
;; Definición tipo Binding
(define-type Binding
  [binding (id symbol?) (value WBAE?)])

;; Tipo WBAE
(define-type WBAE
  [id (i symbol?)]
  [num (n number?)]
  [op (f procedure?) (args (listof WBAE?))]
  [bool (b boolean?)]
  [chaR (c char?)]
  [strinG (s string?)]
  [lst (l (listof WBAE?))]
  [with (bindings (listof binding?)) (body WBAE?)]
  [with* (bindings (listof binding?)) (body WBAE?)])

;;Definición de la función and
;Ejercicio 3 - Práctica 4.
#|Función anD modificada en su aridad, para analizar
el valor de verdad de una proposición, que corresponde a una
conjunción con múltiples operandos lógicos, desde cero hasta n.
Retorna bool true si y sólo si todos los operandos se evaluan a bool true|#
(define (anD ls)
  (if (andmap (lambda (x) (equal? x (bool true))) ls)
      (bool true)
      (bool false)))

#|Función oR modificada en su aridad, para analizar
el valor de verdad de una proposición, que corresponde a una
disyunción con múltiples operandos lógicos, desde cero hasta n.
Retorna bool false si y sólo si todos los operandos se evaluan a bool false|#
(define (oR ls)
  (if (ormap (lambda (x) (equal? x (bool true))) ls)
      (bool true)
      (bool false)))