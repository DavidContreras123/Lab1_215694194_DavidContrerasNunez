#lang racket

(provide crear-prestamo)

(define (crear-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
  (list id id-usuario id-libro fecha-prestamo dias-solicitados))


;----- GETTERS -----


; Descripción: Obtiene la fecha del prestamo
; Dominio: prestamo (Prestamo)
; Recorrido: string

(define (get-fecha-prestamo prestamo)
  (list-ref prestamo 3))

; Descripción: Obtiene los dias solicitados
; Dominio: prestamo (Prestamo)
; Recorrido: int

(define (get-dias-solicitados prestamo)
  (list-ref prestamo 4))

; Descripción: Calcula fecha de vencimiento sumando días solicitados a fecha de prestamo
; Dominio: prestamo (Prestamo)
; Recorrido: string
; Recursión: cola

(define (obtener-fecha-vencimiento prestamo)
  (define (obtener-fecha-aux fecha dias)
    [cond
      ((= dias 0)
       fecha)
      ((and (string=? "30" (car (regexp-split #rx"/" fecha))) (> (string->number(cadr (regexp-split #rx"/" fecha))) 9))
           (if(< (string->number(cadr (regexp-split #rx"/" fecha))) 12)
              (obtener-fecha-aux (string-append "01" "/" (number->string(+ (string->number(cadr (regexp-split #rx"/" fecha))) 1))) (- dias 1))
              (obtener-fecha-aux (string-append "01" "/" "01") (- dias 1))))

      ((and (string=? "30" (car (regexp-split #rx"/" fecha))) (< (string->number(cadr (regexp-split #rx"/" fecha))) 10))
       (if(< (+ (string->number(cadr (regexp-split #rx"/" fecha))) 1) 10)
          (obtener-fecha-aux (string-append "01" "/" "0" (number->string(+ (string->number(cadr (regexp-split #rx"/" fecha))) 1))) (- dias 1))
          (obtener-fecha-aux (string-append "01" "/"  (number->string(+ (string->number(cadr (regexp-split #rx"/" fecha))) 1))) (- dias 1))))
      
      ((< (string->number(car (regexp-split #rx"/" fecha))) 30)
       (if(< (+ (string->number(car (regexp-split #rx"/" fecha))) 1) 10)
              (obtener-fecha-aux (string-append "0" (number->string(+ (string->number(car (regexp-split #rx"/" fecha))) 1))
                                                "/" (cadr (regexp-split #rx"/" fecha))) (- dias 1))
              (obtener-fecha-aux (string-append  (number->string(+ (string->number(car (regexp-split #rx"/" fecha))) 1)) "/" "01") (- dias 1))))
      
      (else
       (obtener-fecha-aux (string-append(number->string(+ (string->number(car (regexp-split #rx"/" fecha))) 1)) "/" (cadr (regexp-split #rx"/" fecha))) (- dias 1)))])

  (obtener-fecha-aux (get-fecha-prestamo prestamo) (get-dias-solicitados prestamo)))
       
                  

