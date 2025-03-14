#lang racket
#|
Practica 1
Clase: Lenguajes de programación.
Profesora: Karla Ramírez Pulido.
Alumnos: Javier Alejandro Rivera Zavala, Gabriela López Diego,
Juan Daniel San Martín Macias, Abraham Jimenéz.
Profesores adjuntos: Alan Martínez y Ricardo Desales.
|#

#|
Función que toma un símbolo y un entero n>0, anida el símbolo dentro de n-1 listas.
entierra :: quote number -> (lisoftype (listoftype ... (listoftype quote)))
|#
(define (entierra a n)
  (if(< n 0)
     (error "No es una entrada apropiada")
     (if (= n 0)
         a
         (cons (entierra a (- n 1)) empty)
      )
  )
)

#|
Función que toma una lista de tripletas y nos indica que tipo
de triángulo definen las magnitudes ellas, si no existen triángulos de
tales dimensiones nos lo indica.
son-triangulos :: (listoftype (listoftype number)) -> (listoftype string)
|#
(define (son-triangulos lst)
  (map (lambda (x) (tipo-de-triangulo x)) lst)
)

#|
Función auxiliar que toma una tripleta y nos indica que tipo
de triángulo definen las magnitudes en ella, si no existe un triángulo de
tales dimensiones nos lo indica. Es conveniente separar la "manipulación" de las tripletas
de la iteración sobre las mismas, para así facilitar el acceso a los datos de estas últimas
y mejorar la legilibildiad del código.
tipo-de-trianuglo :: (listoftype number) -> string
|#
(define (tipo-de-triangulo ls)
  (if (= 3 (length ls))
      (if (or (negative? (first ls)) (or (negative? (second ls)) (negative? (third ls))))
          "No es el caso"
          (if (or (<= (+ (first ls) (second ls)) (third ls))
                  (or (<= (+ (third ls) (second ls)) (first ls)) (<= (+ (first ls) (third ls)) (second ls))))
              "No es el caso"
              (if (and (= (first ls) (second ls)) (= (second ls) (third ls)))
                  "Equilatero"
                  (if (or (= (first ls) (second ls)) (or (= (second ls) (third ls)) (= (first ls) (third ls))))
                      "Isóceles"
                      "Escaleno"
                  )
              )
          )
     )
     "No es el caso" 
  )
)
