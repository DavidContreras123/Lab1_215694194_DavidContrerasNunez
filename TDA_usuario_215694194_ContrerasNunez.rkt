#lang racket

(provide crear-usuario usuario-suspendido? obtener-deuda get-usuario-id)

;----- CONSTRUCTOR -----
; Descripción: Constructor TDA usuario
; Dominio: id (int) X nombre(string)
; Recorrido: usuario
; Recursión: No aplica
(define (crear-usuario id nombre)
  (list id nombre 0 "sin libros" "activo"))

;----- PERTENENCIA -----
; Descripción: Comprueba si el usuario está suspendido
; Dominio: usuario
; Recorrido: #t o #f (bool)
; Recursión: No aplica
(define(usuario-suspendido? usuario)
  (string=? (list-ref usuario 4) "suspendido"))
     
;----- SELECTOR -----
; Descripción: Obtiene la deuda del usuario
; Dominio: usuario
; Recorrido: int (deuda)
; Recursión: No aplica
(define(obtener-deuda usuario)
  (list-ref usuario 2))

(define(get-usuario-id usuario)
  (list-ref usuario 0))

(define(get-usuario-libros usuario)
  (list-ref usuario 3))
