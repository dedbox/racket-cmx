#lang info

(define collection "cmx")

(define deps
  '("base"
    "event-lang"))

(define build-deps
  '("rackunit-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/cmx.scrbl")))
