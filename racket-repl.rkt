#lang racket

(require racket/gui)
(require "presentation.rkt"
         "repl.rkt"
         "text.rkt")

(define (intersperse sep lst)
  (cond
    [(null? lst) null]
    [(and (pair? lst) (null? (cdr lst)))
     lst]
    [else (cons (car lst) (cons sep (intersperse sep (cdr lst))))]))

(define (present-exn exn) "Error")

(define (pretty-present object)
  (define width-preference 80)
  (define (width str)
    (apply max (for/list ([line (string-split str "\n")])
                 (string-length line))))
  (define (pretty-present-sequence xs start-col)
    (define presented-xs
      (for/list ([x xs])
        (real-pretty-present x
                             (+ start-col 2))))
    (define break? (> (+ start-col
                         (length xs)
                         (apply + (for/list ([p presented-xs])
                                    (send p get-length))))
                      width-preference))
    (define contents
      (if break?
          (intersperse (pstring (string-append "\n"
                                               (build-string start-col
                                                             (thunk* #\space))))
                       presented-xs)
          (intersperse (pstring " ") presented-xs)))
    (apply pstring-append contents))

  (define (real-pretty-present obj start-col)
    (match obj
      [(? null? x)
       (pstring-annotate x value/p (pstring "'()"))]
      [(list xs ...)
       (define contents (pretty-present-sequence xs (+ start-col 2)))
       (define start (pstring "'("))
       (define end (pstring ")"))
       (pstring-annotate obj value/p (pstring-append start contents end))]
      [(vector xs ...)
       (define contents (pretty-present-sequence xs (+ start-col 2)))
       (define start (pstring "#("))
       (define end (pstring ")"))
       (pstring-annotate obj value/p (pstring-append start contents end))]
      [other (pstring-annotate obj value/p (pstring (format "~v" other)))]))
  (real-pretty-present object 0))

(module+ main

  (define (rep str)
    (with-handlers ([exn? present-exn])
      (pretty-present (read (open-input-string str)))))

  (define frame (new frame% [label "REPL"] [width 800] [height 600]))
  (define repl (new presentation-repl%
                    [highlight-callback
                     (lambda (dc x1 y1 x2 y2)
                       (define old-brush (send dc get-brush))
                       (define old-pen (send dc get-pen))
                       (send* dc
                         (set-brush "white" 'transparent)
                         (set-pen (make-object color% 200 30 0 0.3) 5 'solid)
                         (draw-rectangle x1 y1 x2 y2)
                         (set-brush old-brush)
                         (set-pen old-pen)))]
                    [eval-callback rep]))
  (define editor-canvas (new editor-canvas%
                             [parent frame]
                             [editor repl]))
  (send frame show #t))
