#lang plai-typed

;; Make all "class.rkt" definitions available here, where
;; the "class.rkt" file must be in the same directory
;; as this one:
(require "class.rkt")

(define-type ExprI
  [numI (n : number)]
  [plusI (lhs : ExprI)
         (rhs : ExprI)]
  [multI (lhs : ExprI)
         (rhs : ExprI)]
  [argI]
  [thisI]
  [nullI]
  [newI (class-name : symbol)
        (args : (listof ExprI))]
  [getI (obj-expr : ExprI)
        (field-name : symbol)]
  [sendI (obj-expr : ExprI)
         (method-name : symbol)
         (arg-expr : ExprI)]
  [superI (method-name : symbol)
          (arg-expr : ExprI)]
  [instanceofI (obj-expr : ExprI)
               (class-name : symbol)]
  [if0I (check : ExprI)
        (then-stmt : ExprI)
        (else-stmt : ExprI)]
  [castI (class-name : symbol)
         (obj-expr : ExprI)])

(define-type ClassI
  [classI (name : symbol)
          (super-name : symbol)
          (field-names : (listof symbol))
          (methods : (listof MethodI))])

(define-type MethodI
  [methodI (name : symbol)
           (body-expr : ExprI)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (expr-i->c [a : ExprI] [super-name : symbol]) : ExprC
  (local [(define (recur expr)
            (expr-i->c expr super-name))]
    (type-case ExprI a
      [numI (n) (numC n)]
      [plusI (l r) (plusC (recur l) (recur r))]
      [multI (l r) (multC (recur l) (recur r))]
      [argI () (argC)]
      [thisI () (thisC)]
      [nullI () (nullC)]
      [newI (class-name field-exprs)
            (newC class-name (map recur field-exprs))]
      [getI (expr field-name)
            (getC (recur expr) field-name)]
      [sendI (expr method-name arg-expr)
             (sendC (recur expr)
                    method-name
                    (recur arg-expr))]
      [superI (method-name arg-expr)
              (ssendC (thisC)
                      super-name
                      method-name
                      (recur arg-expr))]
      [instanceofI (obj-expr class-name)
                   (instanceofC (recur obj-expr) class-name)]
      [if0I (check then-stmt else-stmt)
            (if0C (recur check)(recur then-stmt)(recur else-stmt))]
      [castI (class-name obj-expr)
             (castC class-name (recur obj-expr))])))

(module+ test
  (test (expr-i->c (numI 10) 'object)
        (numC 10))
  (test (expr-i->c (plusI (numI 10) (numI 2)) 'object)
        (plusC (numC 10) (numC 2)))
  (test (expr-i->c (multI (numI 10) (numI 2)) 'object)
        (multC (numC 10) (numC 2)))
  (test (expr-i->c (argI) 'object)
        (argC))
  (test (expr-i->c (thisI) 'object)
        (thisC))
  ; Test for nullI
  (test (expr-i->c (nullI) 'object)
        (nullC))
  (test (expr-i->c (newI 'object (list (numI 1))) 'object)
        (newC 'object (list (numC 1))))
  (test (expr-i->c (getI (numI 1) 'x) 'object)
        (getC (numC 1) 'x))
  (test (expr-i->c (sendI (numI 1) 'mdist (numI 2)) 'object)
        (sendC (numC 1) 'mdist (numC 2)))
  (test (expr-i->c (superI 'mdist (numI 2)) 'posn)
        (ssendC (thisC) 'posn 'mdist (numC 2)))
  ; Test for instanceOfI
  (test (expr-i->c (instanceofI (newI 'object (list (numI 1))) 'object) 'object)
        (instanceofC (newC 'object (list (numC 1))) 'object))
  ; Test for if0I
  (test (expr-i->c (if0I (numI 0) (numI 3) (numI 4)) 'object)
        (if0C (numC 0) (numC 3) (numC 4)))
  ; Test for castI
  (test (expr-i->c (castI 'object (newI 'posn (list (numI 3)(numI 4)))) 'object)
        (castC 'object (newC 'posn (list (numC 3)(numC 4))))))

;; ----------------------------------------

(define (method-i->c [m : MethodI] [super-name : symbol]) : MethodC
  (type-case MethodI m
    [methodI (name body-expr) 
             (methodC name 
                      (expr-i->c body-expr super-name))]))

(module+ test
  (define posn3d-mdist-i-method
    (methodI 'mdist
             (plusI (getI (thisI) 'z)
                    (superI 'mdist (argI)))))
  (define posn3d-mdist-c-method
    (methodC 'mdist
             (plusC (getC (thisC) 'z)
                    (ssendC (thisC) 'posn 'mdist (argC)))))

  (test (method-i->c posn3d-mdist-i-method 'posn)
        posn3d-mdist-c-method))

;; ----------------------------------------

(define (class-i->c-not-flat [c : ClassI]) : ClassC
  (type-case ClassI c
    [classI (name super-name field-names methods)
            (classC
             name
             super-name
             field-names
             (map (lambda (m) (method-i->c m super-name))
                  methods))]))

(module+ test
  (define posn3d-i-class 
    (classI 'posn3d
            'posn
            (list 'z)
            (list posn3d-mdist-i-method)))
  (define posn3d-c-class-not-flat
    (classC 'posn3d
            'posn
            (list 'z)
            (list posn3d-mdist-c-method)))
  
  (test (class-i->c-not-flat posn3d-i-class)
        posn3d-c-class-not-flat))

;; ----------------------------------------

(define (flatten-class [c : ClassC] 
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassC c
    [classC (name super-name field-names methods)
            (type-case ClassC (flatten-super name classes i-classes)
              [classC (super-name super-class-name super-field-names super-methods)
                      (classC
                       name
                       super-name
                       (add-fields super-field-names field-names)
                       (add/replace-methods super-methods methods))])]))

(define (flatten-super [name : symbol]
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassI (find-i-class name i-classes)
    [classI (name super-name field-names i-methods)
            (if (equal? super-name 'object)
                (classC 'object 'object empty empty)
                (flatten-class (find-class super-name classes)
                               classes
                               i-classes))]))

(module+ test
  (define posn-i-class 
    (classI 'posn
            'object
            (list 'x 'y)
            (list (methodI 'mdist
                           (plusI (getI (thisI) 'x)
                                  (getI (thisI) 'y)))
                  (methodI 'addDist
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))
  (define addDist-c-method
    (methodC 'addDist
             (plusC (sendC (thisC) 'mdist (numC 0))
                    (sendC (argC) 'mdist (numC 0)))))
  (define posn-c-class-not-flat
    (classC 'posn
            'object
            (list 'x 'y)
            (list (methodC 'mdist
                           (plusC (getC (thisC) 'x)
                                  (getC (thisC) 'y)))
                  addDist-c-method)))
  (define posn3d-c-class
    (classC 'posn3d
            'posn
            (list 'x 'y 'z)
            (list posn3d-mdist-c-method
                  addDist-c-method)))

  (test (flatten-class posn3d-c-class-not-flat
                       (list posn-c-class-not-flat
                             posn3d-c-class-not-flat)
                       (list posn-i-class
                             posn3d-i-class))
        posn3d-c-class))

;; ----------------------------------------

(define add-fields append)

(define (add/replace-methods [methods : (listof MethodC)]
                             [new-methods : (listof MethodC)])
  : (listof MethodC)
  (cond
    [(empty? new-methods) methods]
    [else (add/replace-methods
           (add/replace-method methods (first new-methods))
           (rest new-methods))]))

(define (add/replace-method [methods : (listof MethodC)] 
                            [new-method : MethodC])
  : (listof MethodC)
  (cond
    [(empty? methods) (list new-method)]
    [else
     (if (equal? (methodC-name (first methods))
                 (methodC-name new-method))
         (cons new-method (rest methods))
         (cons (first methods) 
               (add/replace-method (rest methods)
                                   new-method)))]))

(module+ test
  (test (add-fields (list 'x 'y) (list 'z))
        (list 'x 'y 'z))

  (test (add/replace-methods empty empty)
        empty)
  (test (add/replace-methods empty (list (methodC 'm (numC 0))))
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0))) empty)
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))))
  (test (add/replace-methods (list (methodC 'm (numC 0))
                                   (methodC 'n (numC 2)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))
                                   (methodC 'n (numC 2))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))

  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'm (numC 1)))
        (list (methodC 'm (numC 1))))
  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'n (numC 2)))
        (list (methodC 'm (numC 0))
              (methodC 'n (numC 2)))))

;; ----------------------------------------

(define find-i-class : (symbol (listof ClassI) -> ClassI)
  (make-find classI-name))

;; ----------------------------------------

(define (interp-i [i-a : ExprI] [i-classes : (listof ClassI)]) : Value
  (local [(define a (expr-i->c i-a 'object))
          (define classes-not-flat (map class-i->c-not-flat i-classes))
          (define classes
            (map (lambda (c)
                   (flatten-class c classes-not-flat i-classes))
                 classes-not-flat))]
    (interp a classes (numV -1) (numV -1))))

(module+ test
  (test (interp-i (numI 0) empty)
        (numV 0))

  (test (interp-i
         (sendI (newI 'posn3d (list (numI 5) (numI 3) (numI 1)))
                'addDist
                (newI 'posn (list (numI 2) (numI 7))))
         (list posn-i-class
               posn3d-i-class))
        (numV 18))
  ;; instanceofI test
  (test (interp-i
         (instanceofI (newI 'posn3d (list (numI 5)(numI 4)(numI 2)))
                               'posn)
         (list posn-i-class
               posn3d-i-class))
        (numV 0))
  (test (interp-i
         (instanceofI (newI 'posn (list (numI 4)(numI 2)))
                      'posn3d)
         (list posn-i-class
               posn3d-i-class))
        (numV 1))
  ;; if0 test
  (test (interp-i
         (if0I (numI 0)
               (newI 'posn3d (list (numI 3)(numI 4)(numI 5)))
               (newI 'posn (list (numI 3)(numI 4))))
         (list posn-i-class
               posn3d-i-class))
        (objV 'posn3d (list (numV 3)(numV 4)(numV 5))))
  (test (interp-i
         (if0I (numI 1)
               (newI 'posn3d (list (numI 3)(numI 4)(numI 5)))
               (newI 'posn (list (numI 3)(numI 4))))
         (list posn-i-class
               posn3d-i-class))
        (objV 'posn (list (numV 3)(numV 4))))
  (test (interp-i
         (castI 'object (newI 'posn (list (numI 3)(numI 4))))
         (list posn-i-class posn3d-i-class))
        (objV 'posn (list (numV 3)(numV 4))))
  ;; null test
  (test/exn (interp-i
             (getI (nullI) 'mdist)
             (list posn-i-class
                   posn3d-i-class))
            "null pointer exception")
  (test/exn (interp-i
         (sendI (nullI) 'mdist (numI 0))
                  (list posn-i-class
                        posn3d-i-class))
            "null pointer exception"))
