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
  [appC (fun : ExprC) (argument : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [ifS (condition : ExprS) (branch1 : ExprS) (branch2 : ExprS)]
  [idS (variable : symbol)]
  [appS (fun : symbol) (arg : ExprS)])

;(define-type FunDefC
;  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (variable : symbol) (value : Value)])

(define-type-alias Env (listof Binding))

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define mt-env empty)
(define extend-env cons)

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

(define (interpret [expr : ExprC] [environment : Env]) : Value
  (type-case ExprC expr
    [numC (num) (numV num)]
    [plusC (l r) (num+ (interpret l environment) 
                    (interpret r environment ))]
    [multC (l r) (num* (interpret l environment) 
                    (interpret r environment))]
    [ifC (c l r) (if (num= (interpret c environment) (numV 0)) 
                     (interpret r environment) 
                     (interpret l environment))]
    [idC (s) (lookup s environment)]
    [appC (fun arg) (local ([define fun-def (interpret fun environment)]) 
                      (interpret 
                         (funV-body fun-def)
                         (extend-env (bind (funV-arg fun-def) 
                                           (interpret arg environment)) 
                                     mt-env)))]
    [fdC (name arg body) (funV name arg body)]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond 
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "trying to add non-numV values")]))
        
(define (num* [l : Value] [r : Value]) : Value
  (cond 
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "trying to multiply non-numV values")]))

(define (num= a b)
  (cond
    [(and (numV? a) (numV? b)) (= (numV-n a) (numV-n b))]
    [else (error 'num= "trying to check equality between two non-values")]))
;(define (get-def name fun-defs) : FunDefC
;  (cond 
;    [(empty? fun-defs) (error 'get-def "function not defined")]
;    [(eq? name (fdC-name (first fun-defs))) (first fun-defs)]
;    [else (get-def name (rest fun-defs))]))

(define (desugar ExprS-exp) : ExprC
  (type-case ExprS ExprS-exp
    [numS (num) (numC num)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [ifS (c l r) (ifC (desugar c) (desugar l) (desugar r))]
    [else (error 'desugar "can't desugar this case")]))

;(define (subst [variable : symbol] [argument : ExprC] [body : ExprC]) : ExprC
;  (type-case ExprC body
;    [numC (n) body]
;    [plusC (l r) (plusC (subst variable argument l) 
;                        (subst variable argument r))]
;    [multC (l r) (multC (subst variable argument l)
;                        (subst variable argument r))]
;    [ifC (c l r) (ifC (subst variable argument c)
;                      (subst variable argument l)
;                      (subst variable argument r))]
;    [idC (s) (if (eq? variable s) argument body)]
;    [appC (fun arg) (appC fun (subst variable argument arg))]))

(define (lookup name env) : Value
  (cond [(empty? env) (error 'lookup "variable not found")]
        [(eq? name (bind-variable (first env))) (bind-value (first env))]
        [else (lookup name (rest env))]))


(test (interpret (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
 
(test/exn (interpret (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          "lookup: variable not found")

;(test (interpret (plusC (numC 10) (appC 'const5 (numC 10)))
;                 mt-env
;              (list (fdC 'const5 '_ (numC 5)))) 15)
;
;(test (interpret (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
;              mt-env
;              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
;      16)
;
;(test (interpret (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
;              mt-env
;              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
;                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
;      22)
;(test (interpret (parser '(+ 2 3))) 5)



