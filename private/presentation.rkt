#lang racket/base

(require racket/gui/base)
(require racket/class racket/contract racket/match racket/set)
(require (for-syntax racket/base syntax/parse))

(provide presentation<%> presenter<%>
         presentation-type?
         make-presentation-type
         presentation-type/c
         presentation-has-type?
         presentation-presentation-type presentation-value
         prop:presentation presentation?
         presented-object-equal?
         presentation-context<%> presentation-context%
         (contract-out [current-presentation-context
                        (parameter/c (is-a?/c presentation-context<%>))])
         value/p)

(module+ test
  (require rackunit))

;;; A presentation type is an opaque object whose equality is eq?. But
;;; the name is saved for debugging purposes.
(struct presentation-type (name per make-set))

(define
  (make-presentation-type name)
  (presentation-type name eq? seteq))

(define (presented-object-equal? type v1 v2)
  (define per (presentation-type-per type))
  (per v1 v2))

(define (value-acceptable? v pres)
  (define per (presentation-type-per pres))
  (per v v))

(define (presentation-has-type? presentation type)
  (eq? (presentation-presentation-type presentation) type))

(define (presentation-type/c pres-type)
  (make-flat-contract
   #:name `(presentation-type/c ,pres-type)
   #:first-order (lambda (x) (value-acceptable? x pres-type))))

;;; prop:presentation should be set to a two-element list in which the
;;; first element is the projection to get the presented object and
;;; the second is the projection to get the presentation type.
(define-values (prop:presentation presentation? presentation-accessor)
  (make-struct-type-property
   'prop:presentation
   (lambda (x info)
     (if (and (list? x)
              (= (length x) 2)
              (procedure-arity-includes? (car x) 1)
              (procedure-arity-includes? (cadr x) 1))
         x
         (raise-argument-error 'prop:presentation
                               "A list of two accessors"
                               x)))))

(define/contract (presentation-value pres)
  (-> presentation? any/c)
  ((car (presentation-accessor pres)) pres))

(define/contract (presentation-presentation-type pres)
  (-> presentation? presentation-type?)
  ((cadr (presentation-accessor pres)) pres))

(define presentation<%>
  (interface*
   ()
   ([prop:presentation (list (lambda (x) (send x get-presented-value))
                             (lambda (x) (send x get-presentation-type)))])
   [get-presentation-type
    (->m presentation-type?)]
   [get-presented-value
    (->i ([me any/c])
         (result (me) (presentation-type/c (send me get-presentation-type))))]))

(define presenter<%>
  (interface ()
    ;; Equivalent presentations should be shown in a highlighted state.
    [highlight (->i ([me any/c]
                     [p-t presentation-type?]
                     [value (p-t) (presentation-type/c p-t)])
                    ()
                    [result void?])]
    ;; No more highlighting.
    [no-highlighting (->m void?)]
    ;; A mutation in presented data has occurred
    [mutation (->m void?)]))

(define presentation-context<%>
  (interface ()
    [register-presenter (->m (is-a?/c presenter<%>) void?)]
    [currently-accepting (->m (or/c #f presentation-type?))]
    [accept (->i ([me any/c]
                  [p-t presentation-type?]
                  [callback (p-t) (-> (presentation-type/c p-t) any/c)])
                 ()
                 [result void?])]
    [accepted (->i ([me any/c]
                    [pres (me) (let ([accepting-now (send me currently-accepting)])
                                 (and/c presentation?
                                        (lambda (pres)
                                          (eq? (presentation-presentation-type pres)
                                               accepting-now))))])
                   ()
                   [result any/c])]
    [make-active (->m presentation? void?)]
    [nothing-active (->m void?)]        
    [mutation (->m void?)]))

;;; A presentation context manages the global application presentation
;;; state, including:
;;;
;;; 1. Whether the application is currently accepting a presentation,
;;;    and if so, what kinds.
;;;
;;; 2. The collection of presented objects
(define presentation-context%
  (class* object% (presentation-context<%>)
    (super-new)
    (define accepting-stack null)
    (define/public (currently-accepting)
      (if (pair? accepting-stack)
          (caar accepting-stack)
          #f))

    (define (propagate-state new-state)
      (for ([p (in-set presenters)])
        (send p accepting (currently-accepting))))

    (define/public (accept type callback)
      (set! accepting-stack (cons (cons type callback) accepting-stack)))

    (define/public (accepted pres)
      (match-define (cons (cons pres-type callback) todo) accepting-stack)
      (queue-callback
       (lambda ()
         (callback (presentation-value pres))))
      (set! accepting-stack todo))

    (define presenters (weak-seteq))
    (define/public (register-presenter presenter)
      (set-add! presenters presenter))

    ;; Instruct all registered presenters to show equivalent
    ;; presentations as active
    (define/public (make-active p)
      (for ([presenter (in-set presenters)])
        (send presenter highlight
              (presentation-presentation-type p)
              (presentation-value p))))

    (define/public (nothing-active)
      (for ([p (in-set presenters)])
        (send p no-highlighting)))

    (define/public (mutation)
      (for ([presenter (in-set presenters)])
        (send presenter mutation)))))

(define current-presentation-context
  (make-parameter (new presentation-context%)))

(define value/p (make-presentation-type 'value/p))
