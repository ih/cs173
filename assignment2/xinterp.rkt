#lang plai-typed
;; login : 854796922
(define (undefined) (error 'undefined "not defined"))

(define-type Binding
  [binding (name : symbol) (named-expr : CFWAE)])

(define-type CFWAE
  [num (n : number)]
  [binop (op : (number number -> number)) (lhs : CFWAE) (rhs : CFWAE)]
  [with (lob : (listof Binding)) (body : CFWAE)]
  [id (name : symbol)]
  [if0 (c : CFWAE) (t : CFWAE) (e : CFWAE)]
  [fun (args : (listof symbol)) (body : CFWAE)]
  [app (f : CFWAE) (args : (listof CFWAE))])

(define-type Env
  [mtEnv]
  [anEnv (name : symbol) (value : CFWAE-Value) (env : Env)])

(define-type CFWAE-Value
  [numV (n : number)]
  [closureV (params : (listof symbol))
            (body : CFWAE)
            (env : Env)])

;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
;; (parse '{+ 2 2}) should produce (num 4)
(define (parse sexp)
  (cond
    [(s-exp-number? sexp) (num (s-exp->number sexp))]
    [(s-exp-symbol? sexp) (id (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (case (s-exp->symbol (first sexp-list))
         ;;TODO refactor binop cases
         [(+) (binop + (parse (second sexp-list)) (parse (third sexp-list)))]
         [(-) (binop - (parse (second sexp-list)) (parse (third sexp-list)))]
         [(*) (binop * (parse (second sexp-list)) (parse (third sexp-list)))]
         [(if0) (if0 (parse (second sexp-list)) (parse (third sexp-list)) (parse (fourth sexp-list)))]
         [(with) (with (map parse-binding (s-exp->list (second sexp-list)) (parse (third sexp-list))))] 
    [else (error 'parse "invalid input")]))]))

(define (parse-binding sexp-list)
  (if (= (length sexp-list) 2)
      (binding (parse (first sexp-list)) (parse (second sexp-list)))
      (error 'parse-binding "tried to parse a binding that was not valid")))
             
         
(test (parse '2) (num 2))
(test (parse ''a) (id 'a))
(test (parse '{+ 2 2}) (binop + (num 2) (num 2)))
(test (parse '{+ {+ 2 3} 5}) (binop + (binop + (num 2) (num 3)) (num 5)))
(test (parse '{- 9 0}) (binop - (num 9) (num 0)))
(test (parse '{* 9 0}) (binop * (num 9) (num 0)))
(test (parse '{if0 2 {+ 2 2} 3}) (if0 (num 2) (binop + (num 2) (num 2)) (num 3)))
(test (parse 
       '{with {{x 2} {y 3}} {with {{z {+ x y}}} {+ x z}}}) 
      (with [(binding 'x (num 2)) (binding 'y (num 3))] (binop + (id 'x) (id 'z)))) 

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp expr)
  (num 2))



;(define (parser sexp) : ArithC
;  (cond
;    [(s-exp-number? sexp) (numC (s-exp->number sexp))]
;    [(s-exp-list? sexp)
;     (let ([sexp-list (s-exp->list sexp)])
;       (case (s-exp->symbol (first sexp-list))
;         [(+) (plusC (parser (second sexp-list)) (parser (third sexp-list)))]
;         [(*) (multC (parser (second sexp-list)) (parser (third sexp-list)))]))]))
;
;(define (interpret arithC-exp) : number
;  (type-case ArithC arithC-exp
;    [numC (num) num]
;    [plusC (l r) (+ (interpret l) (interpret r))]
;    [multC (l r) (* (interpret l) (interpret r))]))
;
;(test (interpret (parser '(+ 2 3))) 5)
;
;
