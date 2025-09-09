#lang racket

(provide crear-usuario usuario-suspendido? obtener-deuda get-usuario-id
         get-nombre get-usuario-libros cantidad-libros agregar-libro-usuario)

;----- CONSTRUCTOR -----
; Descripción: Constructor TDA usuario
; Dominio: id (int) X nombre(string)
; Recorrido: usuario
; Recursión: No aplica
(define (crear-usuario id nombre)
  (list id nombre 0 '() "activo"))

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

(define(get-nombre usuario)
  (list-ref usuario 1))

(define(get-usuario-libros usuario)
  (list-ref usuario 3))

(define(cantidad-libros usuario)
  (define (cantidad-aux libros contador)
    (if(null? libros)
       contador
       (cantidad-aux (cdr libros) (+ contador 1))))
  (cantidad-aux (get-usuario-libros usuario) 0))

;----- SETTERS -----

(define(agregar-libro-usuario libro usuario)
  (if (null? (get-usuario-libros usuario))
      (list (get-usuario-id usuario) (get-nombre usuario)
            (obtener-deuda usuario)(list libro) "activo")
      (list(get-usuario-id usuario) (get-nombre usuario)
            (obtener-deuda usuario)
           (cons(car(get-usuario-libros usuario))
                (get-usuario-libros (agregar-libro-usuario libro
                                                           (list(get-usuario-id usuario) (get-nombre usuario)
                                                                (obtener-deuda usuario) (cdr(get-usuario-libros usuario))
                                                                "activo"))))
           "activo")))
      














            