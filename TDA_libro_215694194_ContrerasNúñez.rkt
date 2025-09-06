#lang racket

(define (crear-libro id titulo autor)
  (list id "disponible" (string-downcase titulo) (string-downcase autor)))

(define (get-libro-id libro)
  (list-ref libro 2))