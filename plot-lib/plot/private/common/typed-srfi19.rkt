#lang typed/racket/base

(provide make-date date->string)

(require/typed
 srfi/19
 [#:opaque SRFI-Date date?]
 [make-date  (-> Any Any Any Any Any Any Any Any SRFI-Date)]
 [date->string  (-> SRFI-Date String String)])
