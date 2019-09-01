#lang br/quicklang

(require "citrus.rkt")

(define-macro (citrus-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))

(provide (rename-out [citrus-module-begin #%module-begin]))
(provide (all-from-out "citrus.rkt"))
