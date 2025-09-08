#lang racket
(require "TDA_libro_215694194_ContrerasNunez.rkt" "TDA_prestamos_215694194_ContrerasNunez.rkt"
         "TDA_usuario_215694194_ContrerasNunez.rkt")
;----- CONSTRUCTOR -----
; Descripción: Constructor TDA biblioteca 
; Dominio: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X
;           max-libros-usuario (int) X dias-max-prestamo (int) X tasa-multa-diaria
;          (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
; Recorrido: Biblioteca
; Recursión: No aplica

(define (crear-biblioteca libros usuarios prestamos max-libros dias-max
                          tasa-multa limite-deuda dias-retraso fecha-inicial)
  (list libros usuarios prestamos max-libros dias-max
                          tasa-multa limite-deuda dias-retraso fecha-inicial))

;----- PERTENENCIAS -----

(define (libro-disponible? biblioteca id-libro)
  (if (string=?(cadr(filter (lambda (x) (eq? (first x) id-libro))
                            (get-libros biblioteca))) "disponible")
      #t
       #f))
                                                                   
  

;----- GETTERS-----


; Descripción: Busca usuario por ID
; Dominio: biblioteca (Biblioteca) X id (int)
; Recorrido: Usuario o null
; Recursión: No aplica

(define (obtener-usuario biblioteca id)
  (if (null? (filter (lambda (x) (eq? (first x) id))
               (cadr biblioteca))) null
      (car (filter (lambda (x) (eq? (first x) id))
               (cadr biblioteca)))))

; Descripción: Busca libro por ID, autor o titulo
; Dominio: biblioteca (Biblioteca) X criterio (string) X valor (string/int)
; Recorrido: Libro o null
; Recursión: No aplica

(define (buscar-libro biblioteca criterio valor)
  (cond
    ((integer? valor)
     (if (null?(filter (lambda (x) (eq? (first x) valor))
              (car biblioteca))) null
       (car (filter (lambda (x) (eq? (first x) valor))
              (car biblioteca)))))
    ((string-ci=? "autor" criterio)
      (if (null?(filter (lambda (x) (string-ci=? (last x) valor))
              (car biblioteca))) null
       (car (filter (lambda (x) (string-ci=? (last x) valor))
              (car biblioteca)))))
    ((string-ci=? "titulo" criterio)
     (if (null?(filter (lambda (x) (string-ci=? (third x) valor))
              (car biblioteca))) null
     (car (filter (lambda (x) (string-ci=? (third x) valor))
              (car biblioteca)))))))

; Descripción: Obtiene la lista de libros dentro de la biblioteca
; Dominio: biblioteca (Biblioteca) 
; Recorrido: libros

(define (get-libros biblioteca)
  (list-ref biblioteca 0))

; Descripción: Obtiene la lista de usuarios dentro de la biblioteca
; Dominio: biblioteca (Biblioteca) 
; Recorrido: usuarios

(define (get-usuarios biblioteca)
  (list-ref biblioteca 1))

; Descripción: Obtiene la lista de prestamos solicitados
; Dominio: biblioteca (Biblioteca) 
; Recorrido: prestamos

(define (get-prestamos biblioteca)
  (list-ref biblioteca 2))

; Descripción: Obtiene el maximo de libros que puede tener cada usuario de la biblioteca
; Dominio: biblioteca (Biblioteca)
; Recorrido: int (max-libros)

(define (get-max-libros biblioteca)
  (list-ref biblioteca 3))

; Descripción: Obtiene el maximo de dias de prestamo que puede tener cada usuario de la biblioteca
; Dominio: biblioteca (Biblioteca)
; Recorrido: int (dias-max)

(define (get-dias-max biblioteca)
  (list-ref biblioteca 4))

; Descripción: Obtiene la tasa de multas diaria 
; Dominio: biblioteca (Biblioteca) 
; Recorrido: int (tasa-multa)

(define (get-tasa-multa biblioteca)
  (list-ref biblioteca 5))

; Descripción: Obtiene la deuda maxima antes de la suspension
; Dominio: biblioteca (Biblioteca)
; Recorrido: int (limite-deuda)

(define (get-limite-deuda biblioteca)
  (list-ref biblioteca 6))

; Descripción: Obtiene los dias maximos de retraso antes de la suspension
; Dominio: biblioteca (Biblioteca) 
; Recorrido: int (dias-retraso)

(define (get-dias-retraso biblioteca)
  (list-ref biblioteca 7))

; Descripción: Obtiene la fecha inicial en el formato DD/MM
; Dominio: biblioteca (Biblioteca) 
; Recorrido: string (fecha-inicial)

(define (get-fecha biblioteca)
  (list-ref biblioteca 8))


;----- SETTERS-----


; Descripción: Agrega un libro a la biblioteca siempre y cuando el id no exista
; Dominio: biblioteca (Biblioteca) X libro (Libro)
; Recorrido: Biblioteca
; Recursión: Natural

(define(agregar-libro biblioteca libro)
  (if(null?(get-libros biblioteca))
     (list (list libro) (get-usuarios biblioteca)
           (get-prestamos biblioteca) (get-max-libros biblioteca)
           (get-dias-max biblioteca) (get-tasa-multa biblioteca)
           (get-limite-deuda biblioteca) (get-dias-retraso biblioteca)
           (get-fecha biblioteca))
     (if(= (get-libro-id (car(get-libros biblioteca))) (get-libro-id libro))
        biblioteca
        (list(cons(car(get-libros biblioteca))
                  (agregar-libro (list (cdr (get-libros biblioteca))
                                 (get-usuarios biblioteca)
                                 (get-prestamos biblioteca) (get-max-libros biblioteca)
                                 (get-dias-max biblioteca) (get-tasa-multa biblioteca)
                                 (get-limite-deuda biblioteca) (get-dias-retraso biblioteca)
                                 (get-fecha biblioteca)) libro))
             (get-usuarios biblioteca)
             (get-prestamos biblioteca) (get-max-libros biblioteca)
             (get-dias-max biblioteca) (get-tasa-multa biblioteca)
             (get-limite-deuda biblioteca) (get-dias-retraso biblioteca)
             (get-fecha biblioteca)))))



; Descripción: Registra un usuario en la biblioteca siempre y cuando el id no exista
; Dominio: biblioteca (Biblioteca) X usuario (Usuario)
; Recorrido: Biblioteca
; Recursión: Natural

(define (registrar-usuario biblioteca usuario)
  (if(null? (get-usuarios biblioteca))
     (list(get-libros biblioteca) (list usuario)
          (get-prestamos biblioteca) (get-max-libros biblioteca)
          (get-dias-max biblioteca) (get-tasa-multa biblioteca)
          (get-limite-deuda biblioteca) (get-dias-retraso biblioteca)
          (get-fecha biblioteca))
     (if(= (get-usuario-id (car(get-usuarios biblioteca))) (get-usuario-id usuario))
        biblioteca
        (list (get-libros biblioteca)
              (cons(car(get-usuarios biblioteca))
                   (get-usuarios (registrar-usuario
                                  (list (get-libros biblioteca) (cdr (get-usuarios biblioteca))
                                        (get-prestamos biblioteca) (get-max-libros biblioteca)
                                        (get-dias-max biblioteca) (get-tasa-multa biblioteca)
                                        (get-limite-deuda biblioteca) (get-dias-retraso biblioteca)
                                        (get-fecha biblioteca)) usuario)))
              
              (get-prestamos biblioteca) (get-max-libros biblioteca)
              (get-dias-max biblioteca) (get-tasa-multa biblioteca)
              (get-limite-deuda biblioteca) (get-dias-retraso biblioteca)
              (get-fecha biblioteca)))))













  

