#lang racket

(provide crear-prestamo get-prestamo-id get-usuarioId-prestamo get-libroId-prestamo get-fecha-prestamo get-dias-solicitados
         obtener-fecha-vencimiento calcular-dias-retraso calcular-multa get-estado)

;----- CONSTRUCTOR -----

; Descripción: Crea el TDA prestamo
; Dominio: id (int) X id-usuario (int) X id-libro (int) X fecha-prestamo (string X dias-solicitados (int)
; Recorrido: list
; Recursión: No aplica

(define (crear-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
  (list id id-usuario id-libro fecha-prestamo dias-solicitados "activo"))


;----- SELECTORES -----

; Descripción: Obtiene el id del prestamo
; Dominio: prestamo (Prestamo)
; Recorrido: int

(define (get-prestamo-id prestamo)
  (list-ref prestamo 0))

; Descripción: Obtiene el id del usuario dentro del prestamo
; Dominio: prestamo (Prestamo)
; Recorrido: int

(define (get-usuarioId-prestamo prestamo)
  (list-ref prestamo 1))

; Descripción: Obtiene el id del libro dentro del prestamo
; Dominio: prestamo (Prestamo)
; Recorrido: int

(define (get-libroId-prestamo prestamo)
  (list-ref prestamo 2))
                      

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

(define (get-estado prestamo)
  (list-ref prestamo 5))

;----- OTROS -----

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

; Descripción: Calcula los dias de retraso
; Dominio: fecha-ven (string) fecha-actual (string)
; Recorrido: int
; Recursión: No aplica

(define (calcular-dias-retraso fecha-ven fecha-actual)
  (if(> (string->number(cadr (regexp-split #rx"/" fecha-actual))) (string->number(cadr (regexp-split #rx"/" fecha-ven))))
     (- (string->number(car (regexp-split #rx"/" fecha-actual))) (string->number(car (regexp-split #rx"/" fecha-ven))))     
     (if (> (string->number(car (regexp-split #rx"/" fecha-actual))) (string->number(car (regexp-split #rx"/" fecha-ven))))
            (- (string->number(car (regexp-split #rx"/" fecha-actual))) (string->number(car (regexp-split #rx"/" fecha-ven))))
            0)))
     
; Descripción: Calcula la multa
; Dominio: prestamo (Prestamo) fecha-ven (string) tasa (int)
; Recorrido: int
; Recursión: No aplica         

(define (calcular-multa prestamo fecha-actual tasa)
  (if(> (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual) 0)
     (* (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual) tasa)
     0))


                  

