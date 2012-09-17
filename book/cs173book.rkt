#lang plai-typed

(define-type Animal
  [Caml (humps : number)]
  [Yacc (height : number)])

(define (good? [animal : Animal]) : boolean
  (type-case Animal animal
    [Caml (humps) (>= humps 2)]
    [Yacc (height) (> height 1.6)]))

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (condition : ExprC) (branch1 : ExprC) (branch2 : ExprC)]
  [idC (s : symbol)]
  [appC (fun : symbol) (argument : ExprC)])

(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [ifS (condition : ExprS) (branch1 : ExprS) (branch2 : ExprS)]
  [idS (variable : symbol)]
  [appS (fun : symbol) (arg : ExprS)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (parser sexp) : ExprS
  (cond
    [(s-exp-number? sexp) (numS (s-exp->number sexp))]
    [(s-exp-symbol? sexp) (idS (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (case (s-exp->symbol (first sexp-list))
         [(+) (plusS (parser (second sexp-list)) (parser (third sexp-list)))]
         [(*) (multS (parser (second sexp-list)) (parser (third sexp-list)))]
         [(-) (bminusS (parser (second sexp-list)) (parser (third sexp-list)))]
         [(if) (ifS (parser (second sexp-list)) (parser (third sexp-list)) (parser (fourth sexp-list)))]))]))

(define (interpret ExprC-exp fun-defs) : number
  (type-case ExprC ExprC-exp
    [numC (num) num]
    [plusC (l r) (+ (interpret l fun-defs) (interpret r fun-defs))]
    [multC (l r) (* (interpret l fun-defs) (interpret r fun-defs))]
    [ifC (c l r) (if (= (interpret c fun-defs) 0) 
                     (interpret r fun-defs) 
                     (interpret l fun-defs))]
    [idC (s) (error 'interpret "encountered a free variable!")]
    [appC (fun arg) (let ([fun-def (get-def fun fun-defs)])
                      (interpret (subst (fdC-arg fun-def) arg (fdC-body fun-def))
                                 fun-defs))]))

(define (get-def name fun-defs) : FunDefC
  (cond 
    [(empty? fun-defs) (error 'get-def "function not defined")]
    [(eq? name (fdC-name (first fun-defs))) (first fun-defs)]
    [else (get-def name (rest fun-defs))]))

(define (desugar ExprS-exp) : ExprC
  (type-case ExprS ExprS-exp
    [numS (num) (numC num)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [ifS (c l r) (ifC (desugar c) (desugar l) (desugar r))]
    [else (error 'desugar "can't desugar this case")]))

(define (subst [variable : symbol] [argument : ExprC] [body : ExprC]) : ExprC
  (type-case ExprC body
    [numC (n) body]
    [plusC (l r) (plusC (subst variable argument l) 
                        (subst variable argument r))]
    [multC (l r) (multC (subst variable argument l)
                        (subst variable argument r))]
    [ifC (c l r) (ifC (subst variable argument c)
                      (subst variable argument l)
                      (subst variable argument r))]
    [idC (s) (if (eq? variable s) argument body)]
    [appC (fun arg) (appC fun (subst variable argument arg))]))
;(test (interpret (parser '(+ 2 3))) 5)



