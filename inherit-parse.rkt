#lang plai-typed
(require plai-typed/s-exp-match
         "class.rkt"
         "inherit.rkt")

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (parse-class [s : s-expression]) : ClassI
  (cond
   [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
    (classI (s-exp->symbol (second (s-exp->list s)))
            (s-exp->symbol (fourth (s-exp->list s)))
            (map parse-field
                 (s-exp->list (fourth (rest (s-exp->list s)))))
            (map parse-method 
                 (rest (rest (rest (rest (rest (s-exp->list s))))))))]
   [else (error 'parse-class "invalid input")]))

(define (parse-field [s : s-expression]) : symbol
  (cond
   [(s-exp-match? `SYMBOL s)
    (s-exp->symbol s)]
   [else (error 'parse-field "invalid input")]))

(define (parse-method [s : s-expression]) : MethodI
  (cond
   [(s-exp-match? `{SYMBOL ANY} s)
    (methodI (s-exp->symbol (first (s-exp->list s)))
             (parse (second (s-exp->list s))))]
   [else (error 'parse-method "invalid input")]))

(define (parse [s : s-expression]) : ExprI
  (cond
   [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
   [(s-exp-match? `arg s) (argI)]
   [(s-exp-match? `this s) (thisI)]
   [(s-exp-match? '{+ ANY ANY} s)
    (plusI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? '{* ANY ANY} s)
    (multI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? '{new SYMBOL ANY ...} s)
    (newI (s-exp->symbol (second (s-exp->list s)))
          (map parse (rest (rest (s-exp->list s)))))]
   [(s-exp-match? '{get ANY SYMBOL} s)
    (getI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s))))]
   [(s-exp-match? '{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? '{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [(s-exp-match? '{instanceof ANY SYMBOL} s)
    (instanceofI (parse (second (s-exp->list s)))
                 (s-exp->symbol (third (s-exp->list s))))]
   [(s-exp-match? '{if0 ANY ANY ANY} s)
    (if0I (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
   [(s-exp-match? '{cast SYMBOL ANY} s)
    (castI (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse '{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse '{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse '{new posn 1 2})
        (newI 'posn (list (numI 1) (numI 2))))
  (test (parse '{get 1 x})
        (getI (numI 1) 'x))
  (test (parse '{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse '{super m 1})
        (superI 'm (numI 1)))
  ; instanceof
  (test (parse '{instanceof {new posn 1 2} posn})
        (instanceofI (newI 'posn (list (numI 1) (numI 2))) 'posn))
  ; if0
  (test (parse '{if0 0 {new posn 1 2} {new posn3D 1 2 3}})
        (if0I (numI 0) (newI 'posn (list (numI 1)(numI 2)))(newI 'posn3D (list (numI 1)(numI 2)(numI 3)))))
  ; castC
  (test (parse '{cast posn {new posn 1 2}})
        (castI 'posn (newI 'posn (list (numI 1)(numI 2)))))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field '{x 1})
            "invalid input")

  (test (parse-method `{m this})
        (methodI 'm (thisI)))
  (test/exn (parse-method `{m 1 2})
            "invalid input")
  
  (test (parse-class '{class posn3D extends posn
                             {x y z}
                             {m1 arg}
                             {m2 this}})
        (classI 'posn3D 'posn
                (list 'x 'y 'z)
                (list (methodI 'm1 (argI))
                      (methodI 'm2 (thisI)))))
  (test/exn (parse-class '{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-prog [classes : (listof s-expression)] [a : s-expression]) : s-expression
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [numV (n) (number->s-exp n)]
      [objV (class-name field-vals) `object])))

(module+ test
    (define posn-class
    '{class posn extends object
       {x y}
       {mdist {+ {get this x} {get this y}}}
       {addDist {+ {send arg mdist 0}
                   {send this mdist 0}}}})
  (define posn3D-class
    '{class posn3D extends posn
       {z}
       {mdist {+ {get this z} 
                 {super mdist arg}}}})
  
  (test (interp-prog
         (list
          '{class empty extends object
                  {}})
         '{new empty})
        `object)

  (test (interp-prog 
         (list posn-class posn3D-class)
         '{send {new posn3D 5 3 1} addDist {new posn 2 7}})
        '18)
  ;; Test for instanceof
  (test (interp-prog 
         (list
          posn-class posn3D-class)
         '{instanceof {new posn 2 7} posn3D})
        '1)
  (test (interp-prog 
         (list posn-class posn3D-class)
         '{instanceof {new posn3D 4 5 6} posn})
        '0)
  (test/exn (interp-prog 
             (list posn-class posn3D-class)
             '{instanceof  13 posn})
            "not an object")
  ;; Test for if0
  (test (interp-prog
         (list)
         '{if0 0 14 15})
        '14)
  (test (interp-prog
         (list posn-class posn3D-class)
         '{if0 1 5 {new posn3D 4 5 6}})
        `object)
  ;; Test for cast
  (test (interp-prog
         (list posn-class posn3D-class)
         '{cast object {new posn3D 8 0 8}})
        `object))

