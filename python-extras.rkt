#lang racket

(require "./python.rkt")



(require (for-syntax racket))
(require (for-syntax racket/syntax))


(provide 
         append-py
         append-py*
         py-set

         get
         set
         loop
         while
         repeat
         py-if
         
         py-begin 
         py-random
         py-for-in

         py-define
         py-continue
         (all-from-out "./python.rkt"))

 

(define (->string x) (format "~a" x))


(define-for-syntax (split-dots syntax)
  (define (fix-numbers s)
    (if (string->number s)
        (string->number s)
        s))
  (define (fix-vars s)
    (if (string=? "_" (substring s 0 1))
        (string->symbol (substring s 1))
        s))
  (map fix-vars
       (map fix-numbers
            (string-split (format "~a" (syntax->datum syntax)) "."))))

(define-syntax (get2 stx)
  (syntax-case stx ()
    [(_ name (things ...))
     (with-syntax ()
       #`(quote (hy-DOT name [hy-SQUARE things] ...)))]))

(define-syntax (get stx)
  (syntax-case stx ()
    [(_ dotted-datum)
     (with-syntax ([r (rest (split-dots #'dotted-datum))]
                   [f (string->symbol (first (split-dots #'dotted-datum)))])
       #`(get2 f r))]))


(define-syntax (set2 stx)
  (syntax-case stx ()
    [(_ (this (args ...)) that)
     (with-syntax ()
       #`(quasiquote (do
                         (setv ,(get2 this (args ...))
                               ,that)
                         this
                         )))]))

(define-syntax (set stx)
  (syntax-case stx ()
    [(_ dotted-datum other)
     (with-syntax ([r (rest (split-dots #'dotted-datum))]
                   [f (string->symbol (first (split-dots #'dotted-datum)))])
       #`(set2 (f r) other))]))


(define-syntax (py-set stx)
  (syntax-case stx ()
    [(_ var-name other)
     (with-syntax []
       #`(begin
           `(setv ,var-name ,other)))]))

(define-syntax (while stx)
  (syntax-case stx ()
    [(_ condition lines ...)
     (with-syntax ()
       #`(quasiquote (while ,condition
                            ,lines ...)))]))

(define-syntax (loop stx)
  (syntax-case stx ()
    [(_ var max lines ...)
     (with-syntax ()
       #`(let ([var 'n])
           (quasiquote (for (var (range ,max))
                       ,lines ...))))]))

(define-syntax (py-for-in stx)
  (syntax-case stx ()
    [(_ (var in) lines ...)
     (with-syntax ()
       #`(let ([var 'var])
           (quasiquote (for (var ,in)
                         ,lines ...))))]))

(define-syntax (repeat stx)
  (syntax-case stx ()
    [(_ max lines ...)
     (with-syntax ()
       #`(loop n max lines ... ))]))


(define (py-if c t . f)
  (if (empty? f)
      `(if ,c ,t)
      `(if ,c ,t ,(first f))))

(define-syntax (py-begin stx)
  (syntax-case stx ()
    [(_ lines ...)
     (with-syntax ()
       #`(list 'do
               lines ...))]))

(define (is-py-begin? thing)
  (and (not (empty? thing))
       (eq? 'do (first thing))))

(define (append-py a b)
  (cond [(and (is-py-begin? a) (is-py-begin? b)) (append (rest a) (rest b))]
        [(and (list? a) (is-py-begin? b))        (append a        (rest b))]
        [(and (is-py-begin? a) (list? b))        (append (rest a) b)]
        [(and (list? a) (list? b))               (append a        b)]))


(define (append-py* . things)
  (cond
    [(> 2 (length things)) things]
    [(= 2 (length things))
     (append-py (first things)
                (second things))]
    [else
     (foldl append-py
            (first (reverse things))
            (rest (reverse things)))])
  )



(define-syntax-rule (define-op local-name real-name)
  (begin
    (provide local-name)
    (define (local-name . args)
      `(real-name ,@args ))))

(define-op py-gt >)
(define-op py-lt <)
(define-op py-lte <=)
(define-op py-gte >=)
(define-op py-eq  =)
(define-op py-add +)
(define-op py-sub -)
(define-op py-mul *)
(define-op py-div /)
(define-op py-mod %)
(define-op py-min min)

(define-op py-or or)
(define-op py-and and)

(define-op py-max max)
(define-op py-not not)

(define-op py-in in)
(define-op py-print print)

(define-op py-int int)

(define-op py-return return)
(define-op py-list hy-SQUARE)

(define-op py-global global)


(define-op py-dot hy-DOT)

(define-op py-tuple hy-COMMA)


(define py-continue '(continue))

(define (py-random) '(random.random))


(define-syntax (py-define stx)
  (syntax-case stx ()
    [(_ (name params ...) lines ...)
     (with-syntax ()
       #`(begin
           ;  (define (name params ...) '(name params ...))
           (let ([params 'params]
                 ...)
             (quasiquote (defn name [hy-SQUARE params ...]
                           (do                              
                               ,lines ...)
                           )))
           ))]
    ))


