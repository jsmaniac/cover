#lang racket/base
(require "private/contracts.rkt"
         "private/database.rkt")
(provide
 (contract-out
  ;; data
  [coverage-database? (-> any/c boolean?)]
  [file-coverage? (-> any/c boolean?)]

  [ast-tree? (-> any/c boolean?)]
  [ast-tree-position (-> ast-tree? real?)]
  [ast-tree-span (-> ast-tree? real?)]
  [ast-tree-children (-> ast-tree? (listof ast-tree?))]
  [ast-tree-covered (-> ast-tree? (or/c 'missing 'covered 'uncovered))]

  ;; construction
  [make-coverage-database
   (->* (coverage/c) () #:rest (listof (or/c path-string input-port?))
        coverage-database?)]
  [get-file-data
   (-> coverage-database? any/c (maybe/c file-coverage?))]

  ;; querying
  [get-line-info (-> file-coverage? exact-nonnegative-integer?
                     (values real? real? (listof ast-tree?)))]
  [get-point-info (-> file-coverage? exact-nonnegative-integer?
                      ast-tree?)]))
