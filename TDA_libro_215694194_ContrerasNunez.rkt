#lang racket

(provide crear-libro get-libro-id get-titulo get-autor prestar-libro)
;----- CONSTRUCTOR -----

(define (crear-libro id titulo autor)
  (list id "disponible" (string-downcase titulo) (string-downcase autor)))

;----- SELECTORES -----

(define (get-libro-id libro)
  (list-ref libro 0))

(define (get-titulo libro)
  (list-ref libro 2))

(define (get-autor libro)
  (list-ref libro 3))

;----- MODIFICADORES -----

(define (prestar-libro libro)
  (list (get-libro-id libro) "ocupado"
        (get-titulo libro) (get-autor libro)))
