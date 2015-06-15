#lang racket/base
(provide
 make-coverage-database
 coverage-database
 get-file-data
 file-coverage?
 (struct-out ast-tree))
(require racket/dict
         racket/match
         "file-utils.rkt")

;; the only field is a (dictof any/c file-coverage?)
(struct coverage-database (map))
;; the first field is an sexp tree
;; the second field is a (dict-of natural (list natural? natural?))
;;     it is a one indexed map of line numbers to char ranges
;; the third field is a (dict-of natural (listof ast-tree))
;;     one indexed map of char locations to any expressions that start at that char
(struct file-coverage (tree line-map char-map))

(struct ast-tree (position span covered children))

(define (make-coverage-database coverage . files)
  (for/hash ([f files])
    (define-values (name stx str) (file->data f))
    (define stx (file->sexp f))
    (define ast-tree (build-ast-tree (curry coverage name) stx))
    (values name
            (file-coverage ast-tree
                           (build-line-map str)
                           (ast-tree->char-map ast-tree)))))

(define (get-file-data d p)
  (dict-ref (coverage-database-map d) p #f))

(define (ast-tree->char-map tree [map (hash)])
  (define added ))

(define (build-ast-tree coverage stx)
  (define p (syntax-position stx))
  (ast-tree
   p
   (syntax-span stx)
   (if p (coverage p) 'missing)
   (let ([c (syntax->list stx)])
     (if c
         null
         (map (lambda (stx) (build-file-coverage coverage stx)) c)))))

(define (build-line-map str)
  (let loop ([chars (string->list str)]
             [start 1]
             [current 1]
             [line 1]
             [map (hash)])
    (match
     [(list) map]
     [(list* #\newline #\return rst ...)
      (loop rst
            (+ 2 current)
            (+ 2 current)
            (add1 line)
            (dict-set map line (list start current)))]
     [(list* #\newline rst)
      (loop rst
            (add1 current)
            (add1 current)
            (add1 line)
            (dict-set map line (list start current)))]
     [(list* _ rst)
      (loop rst
            start
            (add1 current)
            line
            map)])))

(define (file->data f)
  (define name (file->name f))
  (define p (file->port f))
  (define str (file->string p))
  (define stx
    (with-module-reading-parameterization
        (lambda ()
          (define sp (open-input-string p))
          (port-count-lines! sp)
          (read-syntax name sp))))
  (when (path-string? f) (close-input-port p))
  (values name stx str))


;; (or/c pathstring input-port) -> any/c
(define (file->name f)
  (if (path->string? f)
      (path->string (->absolute f))
      (object-name? f)))

(define (file->port f)
  (if (input-port? f)
      f
      (open-input-file (->absolute f))))
