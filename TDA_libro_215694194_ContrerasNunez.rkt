#lang racket

(provide crear-libro get-libro-id get-titulo get-autor prestar-libro)

;----- CONSTRUCTOR -----

; Descripción: Crea el TDA libro
; Dominio: id (int) X titulo (string) X autor (string)
; Recorrido: list
; Recursión: No aplica

(define (crear-libro id titulo autor)
  (list id "disponible" (string-downcase titulo) (string-downcase autor)))

;----- SELECTORES -----

; Descripción: Obtiene el id del libro
; Dominio: libro (Libro)
; Recorrido: int

(define (get-libro-id libro)
  (list-ref libro 0))

; Descripción: Obtiene el titulo del libro
; Dominio: libro (Libro)
; Recorrido: string

(define (get-titulo libro)
  (list-ref libro 2))

; Descripción: Obtiene el autor del libro
; Dominio: libro (Libro)
; Recorrido: string

(define (get-autor libro)
  (list-ref libro 3))

;----- MODIFICADORES -----

; Descripción: Cambia el estado del libro a ocupado
; Dominio: libro (Libro)
; Recorrido: list

(define (prestar-libro libro)
  (list (get-libro-id libro) "ocupado"
        (get-titulo libro) (get-autor libro)))
