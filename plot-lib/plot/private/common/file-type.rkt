#lang typed/racket/base

;; Detect an image file type from a filename extension.

(require racket/list
         "types.rkt")

(provide (all-defined-out))

(: file-type-hash (Listof (Pair Regexp Image-File-Format)))
(define file-type-hash
  '((#rx"(?i:.*\\.png)$" . png)
    (#rx"(?i:.*\\.(jpg)|(jpeg))$" . jpeg)
    (#rx"(?i:.*\\.xbm)$" . xbm)
    (#rx"(?i:.*\\.xpm)$" . xpm)
    (#rx"(?i:.*\\.bmp)$" . bmp)
    (#rx"(?i:.*\\.pdf)$" . pdf)
    (#rx"(?i:.*\\.ps)$" . ps)
    (#rx"(?i:.*\\.svg)$" . svg)))

(: detect-image-file-type (-> Path-String Image-File-Format))
(define (detect-image-file-type output)
  (define name
    (cond [(string? output)  output]
          [(path? output)    (path->string output)]
          [else  (error 'detect-image-file-type "cannot detect file type for ~e" output)]))
  (let loop ([hash  file-type-hash])
    (cond [(empty? hash)  (error 'detect-image-file-type "~e has unknown file type" name)]
          [(regexp-match (car (first hash)) name)  (cdr (first hash))]
          [else  (loop (rest hash))])))
