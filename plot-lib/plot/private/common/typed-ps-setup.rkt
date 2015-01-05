#lang typed/racket/base

(require typed/racket/class)

(provide PS-Setup%
         ps-setup%
         current-ps-setup)

(define-type PS-Setup%
  (Class (init)
         [copy-from (-> (Instance PS-Setup%) Any Void)]
         [get-command (-> String)]
         [get-editor-margin (-> (Boxof Nonnegative-Real) (Boxof Nonnegative-Real) Void)]
         [get-file (-> (U Path-String #f))]
         [get-level-2 (-> Boolean)]
         [get-margin (-> (Boxof Nonnegative-Real) (Boxof Nonnegative-Real) Void)]
         [get-mode (-> (U 'preview 'file 'printer))]
         [get-orientation (-> (U 'portrait 'landscape))]
         [get-paper-name (-> String)]
         [get-preview-command (-> String)]
         [get-scaling (-> (Boxof Nonnegative-Real) (Boxof Nonnegative-Real) Void)]
         [get-translation (-> (Boxof Nonnegative-Real) (Boxof Nonnegative-Real) Void)]
         [set-command (-> String Void)]
         [set-editor-margin (-> Natural Natural Void)]
         [set-file (-> (U Path-String #f) Void)]
         [set-level-2 (-> Any Void)]
         [set-margin (-> Nonnegative-Real Nonnegative-Real Void)]
         [set-mode (-> (U 'preview 'file 'printer) Void)]
         [set-orientation (-> (U 'portrait 'landscape) Void)]
         [set-paper-name (-> String Void)]
         [set-preview-command (-> String Void)]
         [set-scaling (-> Nonnegative-Real Nonnegative-Real Void)]
         [set-translation (-> Real Real Void)]))

(require/typed
 racket/draw
 [ps-setup%  PS-Setup%]
 [current-ps-setup  (Parameterof (Instance PS-Setup%))])
