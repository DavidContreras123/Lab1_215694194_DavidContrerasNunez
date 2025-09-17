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
  (string=? (list-ref (buscar-libro biblioteca "id" id-libro) 1) "disponible"))

; Descripción: Verifica si el usuario debe suspenderse
; Dominio: biblioteca (Biblioteca) X id-usuario (int) X fecha-actual (string)
; Recorrido: bool
; Recursión: Cola

(define (debe-suspenderse? biblioteca id-usuario fecha-actual)
  (define (suspender-aux prestamo)
    (if (null? prestamo)
        #f
        (if (or ( > (obtener-deuda (obtener-usuario biblioteca id-usuario)) (get-limite-deuda biblioteca))
                (calcular-dias-retraso (obtener-fecha-vencimiento  (car prestamo)) fecha-actual))
            #t  
            (suspender-aux (cdr prestamo))))) 
  (suspender-aux (filter (lambda (x) (= (cadr x) id-usuario)) (get-prestamos biblioteca))))

; Descripción: Busca prestamo por ID
; Dominio: biblioteca (Biblioteca) X id-usuario (int) X fecha-actual (string)
; Recorrido: bool
; Recursión: Cola

(define (tiene-atrasos? prestamos id-usuario fecha-actual)
  (if (null? prestamos)
      #f
      (if (and (= (get-usuarioId-prestamo (car prestamos)) id-usuario) 
               (> (calcular-dias-retraso (obtener-fecha-vencimiento (car prestamos)) fecha-actual) 0))
               #t
               (tiene-atrasos? (cdr prestamos) id-usuario fecha-actual)))) 


;----- SELECTORES -----

; Descripción: Busca prestamo por ID
; Dominio: biblioteca (Biblioteca) X id-prestamo (int)
; Recorrido: prestamo
; Recursión: No aplica

(define (obtener-prestamo biblioteca  id-prestamo)
   (if (null? (filter (lambda (x) (= (car x) id-prestamo))
                      (get-prestamos biblioteca)))
       null
       (car(filter (lambda (x) (= (car x) id-prestamo))
                   (get-prestamos biblioteca)))))
  
; Descripción: Busca usuario por ID
; Dominio: biblioteca (Biblioteca) X id (int)
; Recorrido: Usuario o null
; Recursión: No aplica

(define (obtener-usuario biblioteca id)
  (if (null? (filter (lambda (x) (= (car x) id))
               (get-usuarios biblioteca))) null
  (car(filter (lambda (x) (= (first x) id))
               (get-usuarios biblioteca)))))

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


;----- MODIFICADORES -----


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
        ; Se crea una lista con el par conformado por el primer libro y
        ; el resto de la biblioteca con el primer elemento obtenido por el llamado
        ; recursivo (lista de libros)
        (list(cons(car(get-libros biblioteca))
                  (car(agregar-libro (list (cdr (get-libros biblioteca))
                                 (get-usuarios biblioteca)
                                 (get-prestamos biblioteca) (get-max-libros biblioteca)
                                 (get-dias-max biblioteca) (get-tasa-multa biblioteca)
                                 (get-limite-deuda biblioteca) (get-dias-retraso biblioteca)
                                 (get-fecha biblioteca)) libro)))
         ; Se reconstruye el resto de la biblioteca
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
        ; Se crea una lista con los libros primero y despues se usa un par con el primer
        ;; usuario con el resto de la lista de usuarios del llamado recursivo 
        ; Despues del llamado se termina de construir la biblioteca
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
    


; Descripción: Actualiza la biblioteca a la hora de tomar un prestamo
; Dominio: biblioteca (Biblioteca) X usuario-mod (Usuario) X libro-mod (Libro) X dias (int) fecha-actual (string)
; Recorrido: Biblioteca
; Recursión: No aplica

(define (actualizar-biblioteca biblioteca usuario-mod libro-mod dias fecha-actual)
  (list
   (map (lambda (x)
          (if(= (get-libro-id x) (get-libro-id libro-mod))
             libro-mod
             x)) (get-libros biblioteca))
   (map (lambda (x)
          (if(= (get-usuario-id x) (get-usuario-id usuario-mod))
             usuario-mod
             x)) (get-usuarios biblioteca))

   (cons (crear-prestamo (nueva-id-prestamo biblioteca) (get-usuario-id usuario-mod) (get-libro-id libro-mod)
                   fecha-actual dias) (get-prestamos biblioteca))
   (get-max-libros biblioteca) (get-dias-max biblioteca) (get-tasa-multa biblioteca)
   (get-limite-deuda biblioteca) (get-dias-retraso biblioteca) (get-fecha biblioteca)))
   
  

; Descripción: Toma un prestamo 
; Dominio: biblioteca (Biblioteca) X id-usuario (int) X id-libro (int) X dias (int) fecha-actual (string)
; Recorrido: Biblioteca
; Recursión: No aplica

(define (tomar-prestamo biblioteca id-usuario id-libro dias fecha-actual)
  [cond
    ((and(libro-disponible? biblioteca id-libro) (not(usuario-suspendido? (obtener-usuario biblioteca id-usuario)))
         (< (get-usuario-libros (obtener-usuario biblioteca id-usuario)) (get-max-libros biblioteca))
         (<= dias (get-dias-max biblioteca)) (<= (obtener-deuda (obtener-usuario biblioteca id-usuario)) (get-limite-deuda biblioteca)))
     (actualizar-biblioteca biblioteca (agregar-libro-usuario (obtener-usuario biblioteca id-usuario))
                             (prestar-libro (buscar-libro biblioteca "id" id-libro)) dias fecha-actual))
    (else  biblioteca)])


; Descripción: Devuelve un libro
; Dominio: biblioteca (Biblioteca) X id-usuario (int) X id-libro (int) X fecha-actual (string)
; Recorrido: Biblioteca
; Recursión: No aplica

(define (devolver-libro biblioteca id-usuario id-libro fecha-actual)
  (list
   (map (lambda (x)
          (if (= id-libro (get-libro-id x))
              (list id-libro "disponible"
                    (get-titulo x) (get-autor x))
              x)) (get-libros biblioteca))
   (map (lambda (x)
          (if (= id-usuario (get-usuario-id x))
              (if(> (+ (calcular-multa (car(filter (lambda (x) (= (third x) id-libro)) (get-prestamos biblioteca)))
                                    fecha-actual (get-tasa-multa biblioteca)) (obtener-deuda x)) (get-limite-deuda biblioteca))
                 (list (get-usuario-id x) (get-nombre x)
                       (+ (obtener-deuda x) (calcular-multa (car(filter (lambda (x) (= (third x) id-libro)) (get-prestamos biblioteca)))
                                                            fecha-actual (get-tasa-multa biblioteca)))
                       (- (get-usuario-libros x) 1)
                       "suspendido")
                 (list (get-usuario-id x) (get-nombre x)
                       (+ (obtener-deuda x) (calcular-multa (car(filter (lambda (x) (= (third x) id-libro)) (get-prestamos biblioteca)))
                                                            fecha-actual (get-tasa-multa biblioteca)))
                       (- (get-usuario-libros x) 1)
                       (get-estado-usuario x)))
              x)) (get-usuarios biblioteca))
   (map (lambda (x)
          (if (= id-usuario (get-usuarioId-prestamo x))
              (list (get-prestamo-id x) (get-usuarioId-prestamo x)
                    (get-libroId-prestamo x) (get-fecha-prestamo x)
                    (get-dias-solicitados x) "expirado")
              x)) (get-prestamos biblioteca))
   
   (get-max-libros biblioteca) (get-dias-max biblioteca) (get-tasa-multa biblioteca)
   (get-limite-deuda biblioteca) (get-dias-retraso biblioteca) (get-fecha biblioteca)))
              

; Descripción: Suspende al usuario
; Dominio: biblioteca (Biblioteca) X id-usuario (int) 
; Recorrido: Biblioteca
; Recursión: No aplica

(define (suspender-usuario biblioteca id-usuario) 
  (list (get-libros biblioteca)
        (map (lambda (x)
               (if (= id-usuario (get-usuario-id x))
                   (list
                    (get-usuario-id x) (get-nombre x)
                   (obtener-deuda x) (get-usuario-libros x)
                   "suspendido")
                   x))(get-usuarios biblioteca))
        (get-prestamos biblioteca)  (get-max-libros biblioteca) (get-dias-max biblioteca) (get-tasa-multa biblioteca)
   (get-limite-deuda biblioteca) (get-dias-retraso biblioteca) (get-fecha biblioteca)))


; Descripción: Renueva el prestamo
; Dominio: biblioteca (Biblioteca) X id-prestamo (int) X dias-extra (int) X fecha-actual (string)
; Recorrido: Biblioteca
; Recursión: No aplica  

(define (renovar-prestamo biblioteca)
  (lambda (id-prestamo)
    (lambda (dias-extra)
      (lambda (fecha-actual)
        ; Se verifica que se cumplan las condiciones para renovar el prestamo
        [cond
          ((and (not (null?(obtener-prestamo biblioteca id-prestamo)))
                (string=? (get-estado (obtener-prestamo biblioteca id-prestamo)) "activo")
                (not (usuario-suspendido? (obtener-usuario biblioteca (get-usuarioId-prestamo (obtener-prestamo biblioteca id-prestamo)))))
                (eq? (tiene-atrasos? (obtener-prestamo biblioteca id-prestamo) (get-usuarioId-prestamo (obtener-prestamo biblioteca id-prestamo))
                                   fecha-actual) #f)
                (<= (+ dias-extra (get-dias-solicitados (obtener-prestamo biblioteca id-prestamo))) (get-dias-max biblioteca)))
           (list
            (get-libros biblioteca) (get-usuarios biblioteca)
            (map (lambda (x)
                   ; Verifica si tienen el mismo id
                   (if (= id-prestamo (get-prestamo-id x))
                       ; Se crea un prestamo actualizando los dias solicitados
                       (list  (get-prestamo-id x) (get-usuarioId-prestamo x)
                              (get-libroId-prestamo x) (get-fecha-prestamo x)
                              (+ dias-extra (get-dias-solicitados x)) (get-estado x))
                       ; Deja el prestamo como estaba y sigue buscando
                       x)) (get-prestamos biblioteca))
            ; Se construye la parte faltante de la biblioteca
             (get-max-libros biblioteca) (get-dias-max biblioteca) (get-tasa-multa biblioteca)
             (get-limite-deuda biblioteca) (get-dias-retraso biblioteca) (get-fecha biblioteca)))
          (else biblioteca)]))))


; Descripción: Paga la deuda del usuario
; Dominio: biblioteca (Biblioteca) X id-usuario (int) X monto (int)
; Recorrido: Biblioteca
; Recursión: No aplica

(define (pagar-deuda biblioteca id-usuario monto)
  (list
   (get-libros biblioteca)
   (map (lambda (x)
        (if (= id-usuario (get-usuario-id x))
            (if (and (= (- monto (obtener-deuda x)) 0) (eq? (tiene-atrasos? (filter (lambda (x)
                                                                                      (= (cadr x) id-usuario))(get-prestamos biblioteca))
                                                                            id-usuario (get-fecha biblioteca)) #f))
                (list (get-usuario-id x) (get-nombre x) (- monto (obtener-deuda x))
                      (get-usuario-libros x) "activo")

                (list (get-usuario-id x) (get-nombre x) (abs(- monto (obtener-deuda x)))
                      (get-usuario-libros x) "suspendido"))
            x)) (get-usuarios biblioteca))
   (get-prestamos biblioteca) (get-max-libros biblioteca) (get-dias-max biblioteca) (get-tasa-multa biblioteca)
       (get-limite-deuda biblioteca) (get-dias-retraso biblioteca) (get-fecha biblioteca)))

;----- OTROS -----

(define(nueva-id-prestamo biblioteca)
  (define (nueva-id-aux prestamos contador)
    (if(null? prestamos)
       contador
       (nueva-id-aux (cdr prestamos) (+ contador 1))))
  (nueva-id-aux (get-prestamos biblioteca) 0))



      






  

