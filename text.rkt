#lang racket/base

(require racket/class racket/contract racket/match racket/set)
(require racket/gui/base)
(require "presentation.rkt")

(provide presentation-string<%>
         pstring
         pstring-append
         pstring-annotate
         presentation-text%         
         presentation-text%)

(struct textual-presentation (offset len value type)
  #:property prop:presentation
  (list (lambda (x) (textual-presentation-value x))
        (lambda (x) (textual-presentation-type x))))

(define presentation-string<%>
  (interface ()
    [get-string (->m string?)]
    [get-length (->m exact-nonnegative-integer?)]
    [get-presentations (->m (listof textual-presentation?))]))

(define presentation-string%
  (class* object%
    (presentation-string<%>)
    (init-field string)
    (super-new)
    (define len (string-length string))
    (define/public (get-string) string)
    (define/public (get-length) len)
    (define/public (get-presentations) '())))

(define presentation-string-append%
  (class* object%
    (presentation-string<%>)
    (init-field strings)
    (super-new)
    (define len
      (apply + (for/list ([str strings])
                 (send str get-length))))
    (define/public (get-string)
      (apply string-append
             (for/list ([str strings])
               (send str get-string))))
    (define/public (get-length) len)
    (define/public (get-presentations)
      (define length 0)
      (apply append
             (for*/list ([str (in-sequences strings)])
               (define result
                 (for/list ([pres (send str get-presentations)])
                   (match-define (textual-presentation offset len object presentation-type)
                     pres)
                   (textual-presentation (+ offset length) len object presentation-type)))
               (set! length (+ length (string-length (send str get-string))))
               result)))))

(define presentation-of-string%
  (class* object%
    (presentation-string<%>)
    (super-new)
    (init-field string object presentation-type)
    (define len (send string get-length))
    (define/public (get-string)
      (send string get-string))
    (define/public (get-length)
      len)
    (define/public (get-presentations)
      (cons (textual-presentation 0 len object presentation-type) (send string get-presentations)))))

(define (pstring str)
  (new presentation-string% [string str]))
(define (pstring-append . strs)
  (new presentation-string-append% [strings strs]))
(define (pstring-annotate object presentation-type str)
  (new presentation-of-string% [string str] [object object] [presentation-type presentation-type]))


(define presentation-text%
  (class* text%
    (presenter<%>)
    (init-field highlight-callback
                [presentation-context #f])
    (super-new)
    (unless presentation-context
      (set! presentation-context (current-presentation-context)))
    (send presentation-context register-presenter this)

    (define active-presentations (seteq))

    ;; TODO: less-dumb data structure
    ;; For now, a list of lists containing offset, length, object, presentation-type
    (define presented-objects '())

    ;; Maintain the presented-objects map
    (define/public (insert-presenting pstring [start #f])
      (unless start
        (set! start (let ([b (box 0)])
                      (send this get-position b)
                      (unbox b))))
      (define str (send pstring get-string))
      (define pres (send pstring get-presentations))
      (send this insert str start)
      (set! presented-objects
            (append (for/list ([p pres])
                      (match-define (textual-presentation obj-start obj-len object presentation-type)
                        p)
                      (textual-presentation (+ obj-start start) obj-len object presentation-type))
                    presented-objects)))


    (define/override (on-paint before?
                               dc
                               left
                               top
                               right
                               bottom
                               dx
                               dy
                               draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (for ([p active-presentations])
          (match-define (textual-presentation start len object presentation-type) p)
          (define relevant-lines
            (in-range (send this position-line start)
                      (add1 (send this position-line (+ start len)))))
          (for ([line relevant-lines])
            (define line-start (send this line-start-position line))
            (define line-end (send this line-end-position line))
            (define x-begin (box 0.0))
            (define y-begin (box 0.0))
            (define x-end (box 0.0))
            (define y-end (box 0.0))
            (define hl-start-pos (max line-start start))
            (define hl-end-pos (min line-end (+ start len)))
            (when (> hl-end-pos hl-start-pos) ;; needed to deal with newline at end of presentation
              (send this position-location hl-start-pos x-begin y-begin #t #f)
              (send this position-location hl-end-pos x-end y-end #f #t)
              (highlight-callback
               dc
               (+ (unbox x-begin) dx) (+ (unbox y-begin) dy)
               (- (unbox x-end) (unbox x-begin)) (- (unbox y-end) (unbox y-begin))))))))

    (define/public (highlight type value)
      (set! active-presentations
            (for/seteq ([p presented-objects]
                        #:when (presented-object-equal? type
                                                        (presentation-value p)
                                                        value))
              p))
      (send this invalidate-bitmap-cache))

    (define/public (no-highlighting)
      (set! active-presentations (seteq))
      (send this invalidate-bitmap-cache))

    (define (presentation-at x y)
      (define (smallest a b)
        (if (< (textual-presentation-len a)
               (textual-presentation-len b))
            a
            b))
      (define accepting (send presentation-context currently-accepting))
      (define pos (send this find-position x y))
      ;; We need to first check if the found snip itself handles
      ;; presentations, and if so, trust it to do the right thing.
      ;; Otherwise we get flickering as both the text% and the snip
      ;; that does presenting fight for cursor control.
      (define snip (send this find-snip pos 'after))
      (if (is-a? snip presenter<%>)
          'presenter
          (let ([candidates
                 (for/list ([p presented-objects]
                            #:when (and (or (not accepting)
                                            (presentation-has-type? p accepting))
                                        (>= pos (textual-presentation-offset p))
                                        (< pos (+ (textual-presentation-offset p)
                                                  (textual-presentation-len p)))))
                   p)])
            (if (null? candidates)
                'nothing
                (let loop ([best (car candidates)]
                           [remaining (cdr candidates)])
                  (if (null? remaining)
                      best
                      (loop (smallest best (car remaining))
                            (cdr remaining))))))))

    (define/override (on-default-event ev)
      (super on-default-event ev)
      (define-values (x y)
        (send this dc-location-to-editor-location (send ev get-x) (send ev get-y)))
      (cond [(or (send ev moving?) (send ev entering?))
             (let ([pres (presentation-at x y)])
               (match pres
                 [(? presentation?)
                  (send presentation-context make-active pres)]
                 ['nothing
                  (send presentation-context nothing-active)]
                 ['presenter (void)]))]
            [(and (send presentation-context currently-accepting) (send ev button-down?))
             (let ([pres (presentation-at x y)])
               (when (presentation? pres)
                 (send presentation-context accepted pres)))]))))
