#lang racket/base

(require racket/gui/base)
(require racket/class racket/match racket/set)

(provide presenter<%>
         current-presentation-context
         value/p
         (struct-out textual-presentation))

;;; A presentation type is an opaque object whose equality is eq?. But
;;; the name is saved for debugging purposes.
(struct presentation-type (name))

(struct textual-presentation (offset len value type))

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
         (callback (textual-presentation-value pres))))
      (set! accepting-stack todo))

    (define presenters (weak-seteq))
    (define/public (register-presenter presenter)
      (set-add! presenters presenter))

    ;; Instruct all registered presenters to show equivalent
    ;; presentations as active
    (define/public (make-active p)
      (for ([presenter (in-set presenters)])
        (send presenter highlight
              (textual-presentation-type p)
              (textual-presentation-value p))))

    (define/public (nothing-active)
      (for ([p (in-set presenters)])
        (send p no-highlighting)))))

(define current-presentation-context
  (make-parameter (new presentation-context%)))

(define value/p (presentation-type 'value/p))
