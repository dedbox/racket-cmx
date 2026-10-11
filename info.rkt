#lang info

(define collection "cmx")

(define deps
  '("base"
    "event"))

(define build-deps
  '("draw-lib"
    "pict-lib"
    "racket-doc"
    "rackunit-lib"
    "sandbox-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/cmx.scrbl")))
