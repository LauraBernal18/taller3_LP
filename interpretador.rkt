#lang eopl

;;Taller 3 - Fundamentos de Interpretación y Combinación
;;de Lenguajes de Programación
;
;;Integrantes:
;;LAURA VALENTINA BERNAL LOZADA - 2437088
;;LUIS SANTIAGO ARENAS HINCAPIÉ - 2435300
;;SAMUEL VARGAS VALDERRUTEN - 2537761
;;DANIEL SANTIAGO MORA CHAVES - 2539841
;
;;Repositorio: https://github.com/LauraBernal18/taller3_LP

;******************************************************************************************
;;;;; Interpretador para lenguaje con operaciones infijas, textos, números, boleanos, variables locales y procedimiento

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>      ::= <expresion>
;;                      <un-programa (exp)>

;;  <expresion>    ::= <numero>
;;                      <numero-lit (num)>

;;                  ::= "\""<texto>"\"
;;                      <texto-lit(txt)>

;;                  ::= <identificador>
;;                      <var-exp (id)>

;;                  ::= (<expresion> <primitiva-binaria> <expresion>)
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>

;;                  ::= <primitiva-unaria>(<expresion>)
;;                      <primapp-un-exp (prim-unaria exp)>

;; <primitva-binaria>  ::= +       (primitiva-suma)
;;                     ::= ~       (primitiva-resta)
;;                     ::= /       (primitiva-div)
;;                     ::= *       (primitiva-multi)
;;                     ::= concat  (primitiva-concat)
;;                     ::= >       (primitiva-mayor)
;;                     ::= <       (primitiva-menor)
;;                     ::= >=      (primitiva-mayor-igual)
;;                     ::= <=      (primitiva-menor-igual)
;;                     ::= !=      (primitiva-diferente)  
;;                     ::= ==      (primitiva-comparador-igual)

;; <primitiva-unaria>  ::= longitud   (primitiva-longitud)
;;                     ::= add1       (primitiva-add1)
;;                     ::= sub1       (primitiva-sub1)
;;                     ::= neg        (primitiva-negacion-booleana)

;;******************************************************************************************

;;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp (whitespace) skip)
  (comment ("%" (arbno (not #\newline))) skip)
  
  (identifier ("@" letter (arbno (or letter digit "_" "?"))) symbol)

  (text (letter (arbno (or letter digit "_"))) string)
  
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (number (digit (arbno digit) "." digit (arbno digit)) number)
  (number ("-" digit (arbno digit) "." digit (arbno digit)) number) ))

;; en base al ejemplo practicado en clase


;Especificación Sintáctica (gramática)
(define grammar-simple-interpreter
  '((program (expression) un-programa)
    (expression (number) numero-lit)
    (expression ("\"" text "\"") texto-lit)
    (expression (identifier) var-exp)
    (expression ("(" expression primitive-bin expression ")") primapp-bin-exp)
    (expression (primitive-un "(" expression ")") primapp-un-exp)
    (expression ("Si" expression "{" expression "}""sino""{" expression "}") condicional-exp)
    (expression ("declarar" "(" (arbno identifier "=" expression ";") ")" "{" expression "}") variableLocal-exp)
    (expression ("procedimiento" "(" (separated-list identifier ",") ")" "{" expression "}") procedimiento-ex)
    (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval") app-exp)
    ;Punto 8.1: Definir los llamados recursivos en la gramatica
    (expression ("declarec" "(" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression ";") ")" "{" expression "}")
                declarec-exp)
    
    (primitive-bin ("+") primitiva-suma)
    (primitive-bin ("~") primitiva-resta)
    (primitive-bin ("/") primitiva-div)
    (primitive-bin ("*") primitiva-multi)
    (primitive-bin ("concat") primitiva-concat)
    (primitive-bin (">") primitiva-mayor)
    (primitive-bin ("<") primitiva-menor)
    (primitive-bin (">=") primitiva-mayor-igual)
    (primitive-bin ("<=") primitiva-menor-igual)
    (primitive-bin ("!=") primitiva-diferente)
    (primitive-bin ("==") primitiva-comparador-igual)

    (primitive-un ("longitud") primitiva-longitud)
    (primitive-un ("add1") primitiva-add1)
    (primitive-un ("sub1") primitiva-sub1)
    (primitive-un ("neg") primitiva-negacion-booleana)))

;Datatypes construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

;Reconoce una cadena de entrada y la convierte en la estructura
;sintactica del programa que se va a evaluar.
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

;Analiza lexicamente una cadena y devuelve los tokens encontrados.
;Se utiliza para revisar como se reconoce la entrada del lenguaje.
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


;El Interpretador (FrontEnd + Evaluación + señal para lectura )

;Define el ciclo interactivo del interprete: recibe expresiones,
;las analiza y muestra el resultado de evaluarlas.
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;***********************************************************************
;;Ambiente inicial
;Crea el ambiente inicial con las variables y valores establecidos
;por el punto 2 del taller.
(define init-env
  (lambda ()
    (extend-env
     (list (string->symbol "@a")
           (string->symbol "@b")
           (string->symbol "@c")
           (string->symbol "@d")
           (string->symbol "@e"))
     (list 1 2 3 "hola" "FLP")
     (empty-env))))

;;************************************************************************
;;El interprete

;Evalua un programa completo. Obtiene su expresión y la evalua
;utilizando el ambiente inicial definido para el taller.
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (exp)
        (eval-expression exp (init-env))))))


;Evalua una expresion segun su tipo: numero, texto, variable
;u operacion primitiva. Recibe el ambiente para resolver variables.
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))

      (primapp-bin-exp (exp1 prim exp2)
                   (let ((valor1 (eval-expression exp1 env))
                         (valor2 (eval-expression exp2 env)))
                     (aplicar-primitiva-binaria prim valor1 valor2)))
      
      (primapp-un-exp (prim exp1)
                      (let ((valor (eval-expression exp1 env)))
                        (aplicar-primitiva-unaria prim valor)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (= 0 (valor-verdad? (eval-expression test-exp env)))
                           (eval-expression false-exp env)
                           (eval-expression true-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                   (let ((args (eval-rands exps env)))
                     (eval-expression cuerpo
                                      (extend-env ids args env))))
      (procedimiento-ex (ids cuerpo) (cerradura ids cuerpo env))
      (app-exp (exp exps)
               (let ((proc (eval-expression exp env))
                     (args (eval-rands exps env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      ;Punto 8.5
      (declarec-exp (nombres-proc params cuerpos cuerpo-declarec)
                    (eval-expression cuerpo-declarec
                                     (extend-env-recursively nombres-proc params cuerpos env))))))

         
                           
    

;;------------------------------------------------------------
;funciones auxiliares para aplicar eval-expression

; Evalua cada expresion en el ambiente dado y devuelve la lista de valores (vista en clase)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

; Evalua una sola expresion en el ambiente dado (vista en clase)
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

; datatype para la cerradura (vista en clase)
(define-datatype procVal procVal?
  (cerradura
   (ids (list-of symbol?))
   (exp expression?)
   (amb environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente (vista en clase)
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))
;Operaciones primitivas binarias

;Evalua una operacion binaria usando los valores de sus dos operandos.
;Incluye operaciones aritmeticas, de texto y de comparacion.
;Las comparaciones convierten sus resultados a 1 o 0 mediante la funcion valor-verdad?.
(define aplicar-primitiva-binaria
  (lambda (prim valor1 valor2)
    (cases primitive-bin prim
      (primitiva-suma () (+ valor1 valor2))
      (primitiva-resta () (- valor1 valor2))
      (primitiva-div () (/ valor1 valor2))
      (primitiva-multi () (* valor1 valor2))
      (primitiva-concat () (string-append valor1 valor2))
      (primitiva-mayor ()
                       (valor-verdad? (> valor1 valor2)))
      (primitiva-menor ()
                       (valor-verdad? (< valor1 valor2)))
      (primitiva-mayor-igual ()
                       (valor-verdad? (>= valor1 valor2)))
      (primitiva-menor-igual ()
                       (valor-verdad? (<= valor1 valor2)))
      (primitiva-diferente ()
                       (valor-verdad? (not (equal? valor1 valor2))))
      (primitiva-comparador-igual ()
                       (valor-verdad? (equal? valor1 valor2))))))
  

; Operaciones primitivas unarias

; Evalua una operacion que recibe un solo valor.
; Incluye longitud de texto, incremento, decremento y negación booleana.
(define aplicar-primitiva-unaria
  (lambda (prim valor)
    (cases primitive-un prim
      (primitiva-longitud()
                         (string-length valor))
      (primitiva-add1 ()
                      (+ valor 1))
      (primitiva-sub1()
                      (- valor 1))
      (primitiva-negacion-booleana ()
                                  (if (= (valor-verdad? valor) 1)0 1)))))


;Convierte un valor a la representacion numerica de verdadero o falso.
;Si recibe un booleano, convierte #t en 1 y #f en 0.
;Si recibe un número, convierte el cero en 0 y cualquier otro numero en 1.
(define valor-verdad?
  (lambda (valor)
    (if (boolean? valor)
        (if valor 1 0)
        (if (= valor 0) 0 1))))





;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  ;Punto 8.2 Agregar el nuevo ambiente (ambiente recursivo)
  (recursively-extended-env-record (nombres-proc (list-of symbol?))
                                   (params (list-of (list-of symbol?)))
                                   (cuerpos (list-of expression?))
                                   (env environment?)))

;Acepta cualquier valor de Racket como valor valido del interprete.
;Se utiliza como predicado para almacenar numeros, textos y otros valores.
(define scheme-value? (lambda (v) #t))

;Crea un ambiente vacío, que se utiliza cuando todavía
;no hay variables asociadas a valores.
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;Crea un ambiente que asocia una lista de simbolos con
;sus valores y conserva el ambiente anterior para buscar
;variables que no esten en la lista actual.
(define extend-env
  (lambda (simbolos valores ambiente)
    (extended-env-record simbolos valores ambiente)))

;Punto 8.3
;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;Crea un ambiente extendido para procedimientos recursivos, recibe la lista con los nombres de las
;funciones, una lista de listas que contiene los parametros de cada funcion, la lista con el cuerpo
;de cada funcion y recibe un ambiente anterior, se usa para poder declarar procedimientos que se llamen
;a ellos mismos.
(define extend-env-recursively
  (lambda (nombres-proc params cuerpos ambiente-anterior)
    (recursively-extended-env-record nombres-proc params cuerpos ambiente-anterior)))


;Busca un simbolo en el ambiente y devuelve su valor.
;Si no lo encuentra, continua buscando en el ambiente anterior.
;Si llega a un ambiente vacio, informa que la variable no existe.
(define buscar-variable
  (lambda (ambiente simbolo)
    (cases environment ambiente
      (empty-env-record ()
                        (eopl:error 'buscar-variable "Error, la variable no existe: ~s" simbolo))
      (extended-env-record (simbolos valores ambiente-anterior)
                           (let ((posicion (list-find-position simbolo simbolos)))
                             (if (number? posicion)
                                 (list-ref valores posicion)
                                 (buscar-variable ambiente-anterior simbolo))))
      ;Punto 8.4
      (recursively-extended-env-record (nombres-proc params cuerpos ambiente-anterior)
                                       (let ((posicion (list-find-position simbolo nombres-proc)))
                                         (if (number? posicion)
                                             (cerradura (list-ref params posicion)
                                                        (list-ref cuerpos posicion)
                                                        ambiente)
                                             (buscar-variable ambiente-anterior simbolo)))))))


;********************;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

; Busca la posicion de un simbolo dentro de una lista.
; Se utiliza para encontrar el valor asociado a una variable.
(define list-find-position
  (lambda (simbolo lista-simbolos)
    (list-index (lambda (otro-simbolo) (eqv? otro-simbolo simbolo)) lista-simbolos)))

; Recorre una lista y devuelve la posicion del primer elemento
; que cumple la condicion indicada; si no existe, devuelve #f.
(define list-index
  (lambda (condicion lista)
    (cond
      ((null? lista) #f)
      ((condicion (car lista)) 0)
      (else (let ((resultado (list-index condicion (cdr lista))))
              (if (number? resultado)
                (+ resultado 1)
                #f))))))

;******************************************************************************************
;Pruebas del scanner, parser y evaluador para los puntos 1, 2 y 3.

(show-the-datatypes)

just-scan
scan&parse

(just-scan "add1(@b)")
(just-scan "sub1(   @c   )%comentario")
(just-scan "longitud(\"casa\")")
(just-scan "neg(  (@a > @b)  )%prueba")

(scan&parse "add1(@c)")
(scan&parse "(@a ~ @c)")
(scan&parse "(8 >= 8)")
(scan&parse "(@b != @c)")
(scan&parse "neg(0)")
(scan&parse "longitud(\"casa\")")
(scan&parse "(\"buenos\" concat \"dias\")")

(eval-program (scan&parse "add1(@c)"))
(eval-program (scan&parse "(@a ~ @c)"))
(eval-program (scan&parse "(8 >= 8)"))
(eval-program (scan&parse "(@b != @c)"))
(eval-program (scan&parse "neg(0)"))
(eval-program (scan&parse "longitud(\"casa\")"))
(eval-program (scan&parse "(\"buenos\" concat \"dias\")"))

;Pruebas del condicional (punto 4)

(scan&parse "Si (2+3) { 2 } sino { 3 }")
(eval-program (scan&parse "Si (2+3) { 2 } sino { 3 }"))   ; 2
(eval-program (scan&parse "Si (@a > @b) { 10 } sino { 20 }"))   ; 20
(eval-program (scan&parse "Si 0 { \"si\" } sino { \"no\" }"))   ; "no"

;Pruebas de declarar (punto 5)

(scan&parse "declarar (@x=2;@y=3;@a=7;){ (@a + (@x ~ @y)) }")
(eval-program (scan&parse "declarar (@x=2;@y=3;@a=7;){ (@a + (@x ~ @y)) }"))   ; 6
(eval-program (scan&parse "declarar (@x=2;@y=3;@a=7;){ (@a + @b) }"))          ; 9
(eval-program (scan&parse "declarar (@x=5;){ (@x * @x) }"))                    ; 25

;Pruebas de procedimientos (punto 6)

(scan&parse "procedimiento (@x,@y,@z) { ((@x + @y) + @z) }")
(eval-program (scan&parse "procedimiento (@x,@y,@z) { ((@x + @y) + @z) }"))
(eval-program (scan&parse "procedimiento (@x) { add1(@x) }"))
(eval-program (scan&parse "procedimiento () { 5 }"))

;Pruebas de evaluar procedimientos (punto 7)

(eval-program (scan&parse "declarar (@x=2;@y=3;@a=procedimiento (@x,@y,@z) { ((@x + @y) + @z) };){ evaluar @a (1,2,@x) finEval }"))   ; 5
(eval-program (scan&parse "evaluar procedimiento (@x) { add1(@x) } (4) finEval"))   ; 5
(eval-program (scan&parse "declarar (@f=procedimiento () { 7 };){ evaluar @f () finEval }"))   ; 7


;*****************************************************************************************************
;Desarrollo Ejercicios Punto 9

;a.)


;b.)Programa que calcula la suma de los primeros n enteros positivos de forma recursiva.
;Ejemplo con (n = 5), resultado esperado: 15
; "declarec (
;    @sumaNaturales ( @n ) = Si @n {
;                              (@n + evaluar @sumaNaturales ((@n ~ 1)) finEval)
;                            } sino {
;                              0
;                            };
; )
; { evaluar @sumaNaturales (5) finEval }"


;Ejemplo con (n = 10), resultado esperado: 55
; "declarec (
;    @sumaNaturales ( @n ) = Si @n {
;                              (@n + evaluar @sumaNaturales ((@n ~ 1)) finEval)
;                            } sino {
;                              0
;                            };
; )
; { evaluar @sumaNaturales (10) finEval }"


;c.)
;...
