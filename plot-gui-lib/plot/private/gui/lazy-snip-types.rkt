#lang typed/racket/base

(require (only-in typed/mred/mred Snip% Frame% Bitmap%)
         plot/utils)

(provide (all-defined-out))

(define-type Make-2D-Plot-Snip
  (-> (Instance Bitmap%)
      Plot-Parameters
      (-> Boolean Rect Positive-Integer Positive-Integer
          (Values (Instance Bitmap%) Rect (-> Rect Rect)))
      Rect
      Rect
      (-> Rect Rect)
      Positive-Integer
      Positive-Integer
      (Instance Snip%)))

(define-type Make-3D-Plot-Snip
  (-> (Instance Bitmap%)
      Plot-Parameters
      (-> Boolean Real Real Positive-Integer Positive-Integer (Instance Bitmap%))
      Real
      Real
      Positive-Integer
      Positive-Integer
      (Instance Snip%)))

(define-type Make-Snip-Frame
  (-> (Instance Snip%)
      Positive-Real
      Positive-Real
      String
      (Instance Frame%)))
