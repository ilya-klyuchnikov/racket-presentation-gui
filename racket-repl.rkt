#lang racket

(require racket/gui/base)
(require "repl.rkt" "text.rkt")

(define (intersperse sep lst)
  (cond
    [(null? lst) null]
    [(and (pair? lst) (null? (cdr lst))) lst]
    [else (cons (car lst) (cons sep (intersperse sep (cdr lst))))]))

(define (present-exn exn) "Error")

; convert s-expr to presentation
; similar to scala.text.Document
(define (pretty-present object)
  (define width-preference 30)

  (define (pretty-present-seq xs start-col)
    (define presented-xs (map (lambda (x) (pretty-present-obj x (+ start-col 2))) xs))
    (define break? (> (+ start-col (length xs) (apply + (map (lambda (p) (send p get-length)) presented-xs))) width-preference))
    (define sep
      (if break?
          (pstring (string-append "\n" (make-string start-col #\space)))
          (pstring " ")))
    (define contents (intersperse sep presented-xs))      
    (apply pstring-append contents))

  (define (pretty-present-obj obj start-col)
    (match obj
      [(? null?)
       (new presentation-of-string% [str (pstring "'()")] [object obj])]
      [(list xs ...)
       (let ([contents (pretty-present-seq xs (+ start-col 2))]
             [start (pstring "'(")]
             [end (pstring ")")])
         (new presentation-of-string% [string (pstring-append start contents end)] [object obj]))]
      [other
         (new presentation-of-string% [string (pstring (format "~v" other))] [object obj])]))
  (pretty-present-obj object 0))

(module+ main

  (define (rep str)
    ;(with-handlers ([exn? present-exn])
      (pretty-present (read (open-input-string str))));)

  (define frame
    (new frame% [label "REPL"] [width 800] [height 600]))

  (define repl
    (new presentation-repl%
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

  (define editor-canvas
    (new editor-canvas%
         [parent frame]
         [editor repl]))

  (send frame show #t))
