#lang plai

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [divCC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)])

(define (parser sexp) : ArithS
  (cond
    [(s-exp-number? sexp) (numS (s-exp->number sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (case (s-exp->symbol (first sexp-list))
         [(+) (plusS (parser (second sexp-list)) (parser (third sexp-list)))]
         [(*) (multS (parser (second sexp-list)) (parser (third sexp-list)))]
         [(-) (bminusS (parser (second sexp-list)) (parser (third sexp-list)))]))]))

(define (interpret arithC-exp) : number
  (type-case ArithC arithC-exp
    [numC (num) num]
    [plusC (l r) (+ (interpret l) (interpret r))]
    [multC (l r) (* (interpret l) (interpret r))]))

(test (interpret (parser '(+ 2 3))) 5)
