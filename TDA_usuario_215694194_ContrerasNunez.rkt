#lang racket

(provide crear-usuario usuario-suspendido? obtener-deuda get-usuario-id
         get-nombre get-usuario-libros agregar-libro-usuario)

;----- CONSTRUCTOR -----
; Descripción: Constructor TDA usuario
; Dominio: id (int) X nombre(string)
; Recorrido: usuario
; Recursión: No aplica
(define (crear-usuario id nombre)
  (list id nombre 0 0 "activo"))

;----- PERTENENCIA -----
; Descripción: Comprueba si el usuario está suspendido
; Dominio: usuario
; Recorrido: #t o #f (bool)
; Recursión: No aplica
(define(usuario-suspendido? usuario)
  (string=? (list-ref usuario 4) "suspendido"))
     
;----- SELECTORES -----

; Descripción: Obtiene la deuda del usuario
; Dominio: usuario
; Recorrido: int 

(define(obtener-deuda usuario)
  (list-ref usuario 2))

; Descripción: Obtiene el id del usuario
; Dominio: usuario (Usuario)
; Recorrido: int 

(define(get-usuario-id usuario)
  (list-ref usuario 0))

; Descripción: Obtiene el nombre del usuario
; Dominio: usuario (Usuario)
; Recorrido: string 

(define(get-nombre usuario)
  (list-ref usuario 1))

; Descripción: Obtiene la cantidad de libros del usuario
; Dominio: usuario (Usuario)
; Recorrido: int 

(define(get-usuario-libros usuario)
  (list-ref usuario 3))


;----- MODIFICADORES -----

; Descripción: Suma uno a la cantidad de libros del usuario
; Dominio: usuario (Usuario)
; Recorrido: list

(define(agregar-libro-usuario usuario)
  (list (get-usuario-id usuario) (get-nombre usuario)
        (obtener-deuda usuario) (+(get-usuario-libros usuario) 1)
        "activo"))
      














            