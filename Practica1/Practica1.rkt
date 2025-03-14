#lang racket

#|Practica 01 de laboratorio
Clase: Lenguajes de programación.
Alumnos: Javier Alejandro Rivera Zavala, Gabriela López Diego, Juan Daniel San Martín Macias, Abraham Jimenéz Reyes.
Profesora: Karla Ramírez Pulido.
Ayudante de Laboratorio. Ricardo Desales.
Ayudante de teoria. Alan Alexis Martínez López|#


;Ejercicio 1: [Funcion que dada la generatriz y diametro
;de la base de un cono circular recto calcula su area total]
(define (area-total g d)
  ;Definimos el radio, dividiendo el diametro entre 2
  (let ([r (/ d 2)])
    ;Multiplicamos pi * radio * la generatriz del cono
    (+ (* pi r g)
       ;sumamos el resultado anterior mas radio al cuadrado por pi
       (* pi (expt r 2))))) 


;Ejercicio 2: [Funcion que dados cuatro numeros determina
;si se encuentran ordenados de forma decreciente]
(define (decremental? a b c d)
  (and (> a b) (> b c) (> c d)))


;Ejercicio 3: [Funcion que dada una lista,
;multiplica todos los elementos contenidos en ella]
(define (multiplica ls)
  (cond
    [(empty? ls) 1] 
    [else (* (car ls) (multiplica (cdr ls)))]))


;Ejercicio 4: [Funcion que utiliza la formula de Heron para calcular el 
;area de un triangulo a partir de la medida de cada uno de sus lados]
(define (area-heron a b c)
  (sqrt(* (S a b c) (- (S a b c) a) (- (S a b c) b) (- (S a b c) c))))
;[Funcion auxiliar para el Ejercicio 4: Calcula el semiperimetro del triangulo]
(define(S a b c)
  (/ (+ a b c) 2))


;Ejercicio 5: [Funcion que dada una lista indica si
;todos sus elementos contenidos son pares]
(define (pares? ls)
  (if(empty? ls) #t
     (if(verifica (car ls)) (pares? (cdr ls)) #f)))
;;[Funcion auxiliar para el ejercicio 5: Indica si n es par ya que devuelve #t si n mod 2 es 0]
(define(verifica n)
  (= (modulo n 2) 0))


;Ejercicio 6: [Funcion que dado un numero devuelve 1 usando la conjetura de collatz]
(define (collatz n) 
    (aux-collatz n '() ))
;[Funcion auxiliar para el ejercicio 6: Recibe un numero y una lista]
; Si x = 1 -> concatena 1 con el resto el resto de la lista y la devuelve de forma invertida (caso base)
; Si x = par -> hace una llamada recursiva de la funcion con x = x/2 y la lista resultante
; de concatenar x con resto de la lista
; En otro caso (x es impar) hace una llamada recursiva de la funcion con x = 3x+1
; y la lista resultante de concatenar x con el resto de la lista 
 (define (aux-collatz x lst)
    (cond
          [(= x 1) (reverse (cons x lst))]
          [(even? x) (aux-collatz (/ x 2) (cons x lst))]
          [else (aux-collatz (+ (* x 3) 1) (cons x lst))]
          ))


;Ejercicio 7: [Funcion que dado un numero, construye una cadena que dibuja un rombo con dicho numero de digitos]
(define (rombo x)
  (if(> x 10) "Error: el numero debe ser menor o igual a 10"
     (if(= x 1) "0"
        (if(= x 2) " 0\n101\n 0"
           (if(= x 3) "  0\n 101 \n21012\n 101\n  0"
              (if(= x 4) "   0\n  101\n 21012\n3210123\n 21012\n  101\n   0"
                 (if(= x 5) "    0\n   101\n  21012\n 3210123\n432101234\n 3210123\n  21012\n   101\n    0"
                    (if(= x 6) "     0\n    101\n   21012\n  3210123\n 432101234\n54321012345\n 432101234\n  3210123\n   21012\n    101\n     0"
                       (if(= x 7) "      0\n     101\n    21012\n   3210123\n  432101234\n 54321012345\n6543210123456\n 54321012345\n  432101234\n   3210123\n    21012\n     101\n      0"
                          (if(= x 8) "       0\n      101\n     21012\n    3210123\n   432101234\n  54321012345\n 6543210123456\n765432101234567\n 6543210123456\n  54321012345\n   432101234\n    3210123\n     21012\n      101\n       0"
                             (if(= x 9) "        0\n       101\n      21012\n     3210123\n    432101234\n   54321012345\n  6543210123456\n 765432101234567\n87654321012345678\n 765432101234567\n  6543210123456\n   54321012345\n    432101234\n     3210123\n      21012\n       101\n        0\n"
                                (if(= x 10) "         0\n        101\n       21012\n      3210123\n     432101234\n    54321012345\n   6543210123456\n  765432101234567\n 87654321012345678\n9876543210123456789\n 87654321012345678\n  765432101234567\n   6543210123456\n    54321012345\n     432101234\n      3210123\n       21012\n        101\n         0"

"Error: el numero debe ser mayor o igual a 1"))))))))))))





;Ejercicio 8: [Funcion que dada una lista de numeros enteros regresa la
;representacion binaria en cadena de cada uno de los numeros]
(define (binarios lista)
  (cond
        [(null? lista) '()]
        [else (cons (number->string (car lista) 2) (binarios (cdr lista)) )]
        )) 


;;Ejercicio 9: [Funcion que dada una lista de numeros, regresa la misma lista
;con unicamente los numeros primos]
(define (primos lst)
  (filter es-primo? lst))
;[Funcion auxiliar para el ejercicio 9: Devuelve #t si un numero es primo]
(define (es-primo? n)
  (cond
        [(<= n 1)  #f] ;CASO CASE 1
        [(= n 2)   #t] ;CASO BASE 2
        [(even? n) #f]  
        [else (let bucle ((d 3))        
                (cond
                      [(> d (sqrt n))       #t]
                      [(zero? (modulo n d)) #f]
                      [else (bucle (+ d 2))]
                      ))]
        ))


;Ejercicio 10: [Función que toma un símbolo y un entero n>0, anida el símbolo dentro de n-1 listas]
;entierra :: quote number -> (lisoftype (listoftype ... (listoftype quote)))
(define (entierra n a)
  (if(< n 0)
     (error "No es una entrada apropiada")
     (if (= n 0)
         a
         (cons (entierra (- n 1) a) empty))))


#|Ejercicio 11: [Función que toma una lista de tripletas y nos indica que tipo
de triángulo definen las magnitudes ellas, si no existen triángulos de
tales dimensiones nos lo indica]|#
(define (son-triangulos lst)
  (map (lambda (x) (tipo-de-triangulo x)) lst))

#|[Función auxiliar para el ejercicio 11: Toma una tripleta y nos indica que tipo
de triángulo definen las magnitudes en ella, si no existe un triángulo de
tales dimensiones nos lo indica. Es conveniente separar la "manipulación" de las tripletas
de la iteración sobre las mismas, para así facilitar el acceso a los datos de estas últimas
y mejorar la legilibildiad del código]|#
(define (tipo-de-triangulo ls)
  (if (=
       3 (length ls))
      (if (or (negative? (first ls)) (or (negative? (second ls)) (negative? (third ls))))
          "No es el caso"
          (if (or (<= (+ (first ls) (second ls)) (third ls))
                  (or (<= (+ (third ls) (second ls)) (first ls)) (<= (+ (first ls) (third ls)) (second ls))))
              "No es el caso"
              (if (and (= (first ls) (second ls)) (= (second ls) (third ls)))
                  "Equilatero"
                  (if (or (= (first ls) (second ls)) (or (= (second ls) (third ls)) (= (first ls) (third ls))))
                      "Isóceles"
                      "No es el caso"
                  )
              )
          )
     )
     "No es el caso" 
  )
)
