#lang racket/base

(require racket/gui/base)
(require racket/class racket/match racket/set)

(provide presenter<%>
         presentation-has-type?
         presentation-value
         prop:presentation
         presentation?
         presented-object-equal?
         current-presentation-context
         value/p
         (struct-out textual-presentation))

;;; A presentation type is an opaque object whose equality is eq?. But
;;; the name is saved for debugging purposes.
(struct presentation-type (name))

(define (presented-object-equal? type v1 v2)
  (eq? v1 v2))

(define (value-acceptable? v pres)
  (eq? v v))

(define (presentation-has-type? presentation type)
  (eq? (presentation-presentation-type presentation) type))

;;; prop:presentation should be set to a two-element list in which the
;;; first element is the projection to get the presented object and
;;; the second is the projection to get the presentation type.
(define-values (prop:presentation presentation? presentation-accessor)
  (make-struct-type-property 'prop:presentation))

(define (presentation-value pres)
  ((car (presentation-accessor pres)) pres))

(define (presentation-presentation-type pres)
  ((cadr (presentation-accessor pres)) pres))

(struct textual-presentation (offset len value type)
  #:property prop:presentation
  (list (lambda (x) (textual-presentation-value x))
        (lambda (x) (textual-presentation-type x))))

(define presenter<%>
  (interface ()
    ;; Equivalent presentations should be shown in a highlighted state.
    highlight
    ;; No more highlighting.
    no-highlighting))

(define presentation-context<%>
  (interface ()
    register-presenter
    currently-accepting
    accept
    accepted
    make-active
    nothing-active))

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
    ; a list of pairs
    (define accepting-stack null)
    (define/public (currently-accepting)
      (if (pair? accepting-stack)
          (caar accepting-stack)
          #f))

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
        (send p no-highlighting)))))

(define current-presentation-context
  (make-parameter (new presentation-context%)))

(define value/p (presentation-type 'value/p))
