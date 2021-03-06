#lang plai-typed

(require "class.rkt"
         "inherit.rkt")

(define-type ClassT
  [classT (name : symbol)
          (super-name : symbol)
          (fields : (listof FieldT))
          (methods : (listof MethodT))])

(define-type FieldT
  [fieldT (name : symbol)
          (type : Type)])

(define-type MethodT
  [methodT (name : symbol)
           (arg-type : Type)
           (result-type : Type)
           (body-expr : ExprI)])

(define-type Type
  [numT]
  [objT (class-name : symbol)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define find-classT
  (make-find classT-name))

(define find-fieldT
  (make-find fieldT-name))

(define find-methodT
  (make-find methodT-name))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'object)
      empty        
      (type-case ClassT (find-classT class-name t-classes)
        [classT (name super-name fields methods)
                (append 
                 (get-all-field-types super-name t-classes)
                 (map fieldT-type fields))])))

;; ----------------------------------------

(define (make-find-in-tree find-in-list extract)
  (lambda (name t-class t-classes)
    (local [(define items (extract t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'object)
          (find-in-list name items)
          (try (find-in-list name items)
               (lambda ()
                 ((make-find-in-tree find-in-list extract)
                  name 
                  (find-classT super-name t-classes)
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree find-fieldT classT-fields))

(define find-method-in-tree
  (make-find-in-tree find-methodT classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) true]
    [(equal? name1 'object) false]
    [else
     (type-case ClassT (find-classT name1 t-classes)
       [classT (name super-name fields methods)
               (is-subclass? super-name name2 t-classes)])]))

(define (is-superclass? super name t-classes)
  (cond
    [(equal? super name) #t]
    [(equal? 'object name) #f]
    [else
     (type-case ClassT (find-classT name t-classes)
       [classT (class-name super-name fields methods)
               (is-superclass? super super-name t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2 
            [objT (name2)
                  (is-subclass? name1 name2 t-classes)]
            [else false])]
    [else (equal? t1 t2)]))

(define (is-supertype? t1 t2 t-classes)
  (type-case Type t1
    [objT (super-name)
          (type-case Type t2
            [objT (class-name)
                  (is-superclass? super-name class-name t-classes)]
            [else #f])]
    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (classT 'a 'object empty empty))
  (define b-t-class (classT 'b 'a empty empty))
  (define c-t-class (classT 'c 'b empty empty))
  (define d-t-class (classT 'd 'b empty empty))

  ;; Superclass and supertype tests
  (test (is-superclass? 'a 'c (list a-t-class b-t-class c-t-class))
        #t)
  (test (is-superclass? 'c 'b (list  a-t-class b-t-class c-t-class))
        #f)
  (test (is-supertype? (objT 'a) (objT 'c) (list a-t-class b-t-class c-t-class))
        #t)
  (test (is-supertype? (objT 'b) (objT 'a) (list  a-t-class b-t-class c-t-class))
        #f)

  (test (is-subclass? 'object 'object empty)
        true)
  (test (is-subclass? 'a 'b (list a-t-class b-t-class))
        false)
  (test (is-subclass? 'b 'a (list a-t-class b-t-class))
        true)

  (test (is-subtype? (numT) (numT) empty)
        true)
  (test (is-subtype? (numT) (objT 'object) empty)
        false)
  (test (is-subtype? (objT 'object) (numT) empty)
        false)
  (test (is-subtype? (objT 'a) (objT 'b) (list a-t-class b-t-class))
        false)
  (test (is-subtype? (objT 'b) (objT 'a) (list a-t-class b-t-class))
        true))

;; ----------------------------------------

(define typecheck-expr : (ExprI (listof ClassT) Type Type -> Type)
  (lambda (expr t-classes arg-type this-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes arg-type this-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [numT ()
                      (type-case Type (recur r)
                        [numT () (numT)]
                        [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExprI expr
        [numI (n) (numT)]
        [plusI (l r) (typecheck-nums l r)]
        [multI (l r) (typecheck-nums l r)]
        [argI () arg-type]
        [thisI () this-type]
        [newI (class-name exprs)
              (local [(define arg-types (map recur exprs))
                      (define field-types
                        (get-all-field-types class-name t-classes))]
                (if (and (= (length arg-types) (length field-types))
                         (foldl (lambda (b r) (and r b))
                                true
                                (map2 (lambda (t1 t2) 
                                        (is-subtype? t1 t2 t-classes))
                                      arg-types
                                      field-types)))
                    (objT class-name)
                    (type-error expr "field type mismatch")))]
        [getI (obj-expr field-name)
              (type-case Type (recur obj-expr)
                [objT (class-name)
                      (local [(define t-class
                                (find-classT class-name t-classes))
                              (define field
                                (find-field-in-tree field-name
                                                    t-class
                                                    t-classes))]
                        (type-case FieldT field
                          [fieldT (name type) type]))]
                [else (type-error obj-expr "object")])]
        [sendI (obj-expr method-name arg-expr)
               (local [(define obj-type (recur obj-expr))
                       (define arg-type (recur arg-expr))]
                 (type-case Type obj-type
                   [objT (class-name)
                         (typecheck-send class-name method-name
                                         arg-expr arg-type
                                         t-classes)]
                   [else
                    (type-error obj-expr "object")]))]
        [superI (method-name arg-expr)
                (local [(define arg-type (recur arg-expr))
                        (define this-class
                          (find-classT (objT-class-name this-type)
                                       t-classes))]
                  (typecheck-send (classT-super-name this-class)
                                  method-name
                                  arg-expr arg-type
                                  t-classes))]
        ;; Umair's implementations
        [instanceofI (obj-expr class-name)
                     (type-case Type (recur obj-expr)
                       [objT (obj-class-name) (numT)]
                       [else (type-error obj-expr "object")])]
        [if0I (check then-stmt else-stmt)
              (type-case Type (recur check)
                [numT ()
                 (local [(define then-t (recur then-stmt))
                         (define else-t (recur else-stmt))]
                   (cond
                     [(is-subtype? then-t else-t t-classes) else-t]
                     [(is-subtype? else-t then-t t-classes) then-t]
                     [else (type-case Type then-t
                             [objT (class-name-then)
                                   (type-case Type else-t
                                     [objT (class-name-else)
                                           (same-super-type?
                                            (find-classT class-name-then t-classes)
                                            (find-classT class-name-else t-classes)
                                            t-classes)]
                                     [else (type-error else-t "number")])]
                             [else (type-error then-t "number")])]))]
                [else (type-error check "number")])]
        [castI (class-name obj-expr)
               (local [(define t (recur obj-expr))]
                 (type-case Type t
                   [objT (class-name-t)
                         (if (or (is-subtype? t (objT class-name) t-classes)
                                 (is-supertype? t (objT class-name) t-classes))
                                 t
                                 (type-error t "invalid cast"))]
                   [else (type-error t "number")]))]))))

(define (typecheck-send [class-name : symbol]
                        [method-name : symbol]
                        [arg-expr : ExprI]
                        [arg-type : Type]
                        [t-classes : (listof ClassT)])
  (type-case MethodT (find-method-in-tree
                      method-name
                      (find-classT class-name t-classes)
                      t-classes)
    [methodT (name arg-type-m result-type body-expr)
             (if (is-subtype? arg-type arg-type-m t-classes)
                 result-type
                 (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (listof ClassT)]) : ()
  (type-case MethodT method
    [methodT (name arg-type result-type body-expr)
             (if (is-subtype? (typecheck-expr body-expr t-classes
                                              arg-type this-type)
                              result-type
                              t-classes)
                 (values)
                 (type-error body-expr (to-string result-type)))]))

(define (check-override [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (listof ClassT)])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree (methodT-name method)
                                  (find-classT super-name t-classes)
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string (methodT-name method)))))))

(define (typecheck-class [t-class : ClassT] [t-classes : (listof ClassT)])
  (type-case ClassT t-class
    [classT (name super-name fields methods)
            (map (lambda (m)
                   (begin
                     (typecheck-method m (objT name) t-classes)
                     (check-override m t-class t-classes)))
                 methods)]))

(define (typecheck [a : ExprI] [t-classes : (listof ClassT)]) : Type
  (begin
    (map (lambda (t-class)
           (typecheck-class t-class t-classes))
         t-classes)
    (typecheck-expr a t-classes (numT) (objT 'bad))))

(define (same-super-type? [thn : ClassT][els : ClassT][t-classes : (listof ClassT)])
  (type-case ClassT thn
    [classT (name-t super-t fields-t methods-t)
            (type-case ClassT els
              [classT (name-e super-e fields-e methods-e)
                      (if (eq? name-t name-e)
                          (objT name-t)
                          (same-super-type?
                           (find-classT super-t t-classes)
                           (find-classT super-e t-classes) t-classes))])]))
                              

;; ----------------------------------------

(module+ test
  (define posn-t-class
    (classT 'posn 'object
            (list (fieldT 'x (numT)) (fieldT 'y (numT)))
            (list (methodT 'mdist (numT) (numT) 
                           (plusI (getI (thisI) 'x) (getI (thisI) 'y)))
                  (methodT 'addDist (objT 'posn) (numT)
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))

  (define posn3D-t-class 
    (classT 'posn3D 'posn
            (list (fieldT 'z (numT)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))

  (define square-t-class 
    (classT 'square 'object
            (list (fieldT 'topleft (objT 'posn)))
            (list)))

  ;; Objects to test general if0
  (define animal-t-class
    (classT 'animal 'object
            (list (fieldT 'age (numT)))
            (list (methodT 'addyear (numT) (numT)
                           (plusI (getI (thisI) 'age) (argI))))))

  (define mammal-t-class
    (classT 'mammal 'animal
            (list (fieldT 'weight (numT)))
            (list (methodT 'addweight (numT) (numT)
                           (plusI (getI (thisI) 'weight) (argI))))))
  (define reptile-t-class
    (classT 'reptile 'animal
            (list (fieldT 'legs (numT)))
            (list (methodT 'addlegs (numT) (numT)
                           (plusI (getI (thisI) 'legs) (argI))))))
  
  (define whale-t-class
    (classT 'whale 'mammal
            (list (fieldT 'length (numT)))
            (list (methodT 'addlength (numT) (numT)
                           (plusI (getI (thisI) 'length) (argI))))))

  (define snake-t-class
    (classT 'snake 'reptile
            (list)
            (list)))

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class)))

  (define (typecheck-animal a)
    (typecheck a
               (list animal-t-class mammal-t-class reptile-t-class whale-t-class snake-t-class)))
  
  (define posn27 (newI 'posn (list (numI 2) (numI 7))))
  (define posn531 (newI 'posn3D (list (numI 5) (numI 3) (numI 1))))

  (test (typecheck-posn (sendI posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI posn531 'addDist posn27))
        (numT))  
  (test (typecheck-posn (sendI posn27 'addDist posn531))
        (numT))

  (test (typecheck-posn (newI 'square (list (newI 'posn (list (numI 0) (numI 1))))))
        (objT 'square))
  (test (typecheck-posn (newI 'square (list (newI 'posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  ;; Tests for instanceof
  (test (typecheck-posn (instanceofI (newI 'posn (list (numI 2) (numI 7))) 'object))
        (numT))
  (test (typecheck-posn (instanceofI posn27 'posn531))
        (numT))
  (test/exn (typecheck-posn (instanceofI (numI 10) 'object))
            "no type")

  ;; Tests for if0
  (test (typecheck (if0I (numI 0) (numI 3) (numI 4)) empty)
        (numT))
  (test (typecheck-animal (if0I (numI 4)
                         (newI 'whale (list (numI 5)(numI 4)(numI 10)))
                         (newI 'snake (list (numI 3)(numI 0)))))
        (objT 'animal))
  (test (typecheck-posn (if0I (numI 1) posn27 posn531))
        (objT 'posn))
  (test/exn (typecheck (if0I (newI 'object empty) (numI 3) (numI 4)) empty)
            "no type")
  (test/exn (typecheck-posn (if0I (numI 1) (numI 4) posn531))
            "no type")

  ;; Tests for cast
  (test (typecheck-animal (castI 'whale (newI 'mammal (list (numI 4)(numI 17)))))
        (objT 'mammal))
  (test (typecheck-posn (castI 'posn posn531))
        (objT 'posn3D))
  
  (test/exn (typecheck (castI 'posn (newI 'whale (list (numI 4)(numI 30)(numI 90))))
                       (list posn-t-class posn3D-t-class square-t-class
                             animal-t-class mammal-t-class reptile-t-class
                             whale-t-class snake-t-class))
            "invalid cast")
  (test/exn (typecheck-animal (castI 'snake (newI 'whale (list (numI 10)(numI 50)(numI 87)))))
            "invalid cast")
  
  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI posn27 'mdist posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class 
                             (classT 'other 'posn
                                     (list)
                                     (list (methodT 'mdist 
                                                    (objT 'object) (numT)
                                                    (numI 10))))))
            "bad override")
  (test/exn (typecheck-method (methodT 'm (numT) (objT 'object) (numI 0)) (objT 'object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (classT 'cube 'square
                                     empty
                                     (list
                                      (methodT 'm (numT) (numT)
                                               ;; No such method in superclass:
                                               (superI 'm (numI 0)))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [classT (name super-name fields methods)
              (classI
               name 
               super-name
               (map fieldT-name fields)
               (map (lambda (m)
                      (type-case MethodT m
                        [methodT (name arg-type result-type body-expr)
                                 (methodI name body-expr)]))
                    methods))])))

(define interp-t : (ExprI (listof ClassT) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map strip-types t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI posn531 'addDist posn27))
        (numV 18))
  (test (interp-t-posn (sendI posn27 'addDist posn531))
        (numV 18)))
