#lang racket

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

;;(define (registrar-usuario biblioteca usuario)

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
       