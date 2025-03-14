#lang plai
;Practica 03 de laboratorio
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
(define (anD)
  void)

;;Definición de la función or
(define (oR)
  void)
