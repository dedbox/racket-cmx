#lang info

(define collection "cmx")

(define deps
  '("base"
    "event-lang"))

(define build-deps
  '("draw-lib"
    "event-lang"
    "pict-lib"
    "racket-doc"
    "rackunit-lib"
    "sandbox-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/cmx.scrbl")))
