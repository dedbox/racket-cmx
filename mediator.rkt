#lang racket/base

(provide (all-defined-out))

(require
 event
 racket/function)

(struct mediator (offer accept put get) #:mutable)

(define (make-mediator)
  (define ctrl-ch (make-channel))
  (define data-ch (make-channel))
  (mediator
   (λ vs (fmap void (channel-put-evt ctrl-ch vs)))
   (fmap (curry apply values) ctrl-ch)
   (λ vs (fmap void (channel-put-evt data-ch vs)))
   (fmap (curry apply values) data-ch)))

;; Commands

(define (offer m . vs)
  (offer* m vs))

(define (offer* m vs)
  (apply (mediator-offer m) vs))

(define (accept m)
  (mediator-accept m))

(define (put m . vs)
  (put* m vs))

(define (put* m vs)
  (apply (mediator-put m) vs))

(define (get m)
  (mediator-get m))

;; Hooks

(define (on-offer m f)
  (define next-on-offer (mediator-offer m))
  (set-mediator-offer! m (λ vs (bind next-on-offer (pure (apply f vs))))))

(define (on-accept m f)
  (set-mediator-accept! m (fmap f (mediator-accept m))))

(define (on-put m f)
  (define next-on-put (mediator-put m))
  (set-mediator-put! m (λ vs (bind next-on-put (pure (apply f vs))))))

(define (on-get m f)
  (set-mediator-get! m (fmap f (mediator-get m))))

;;; Unit Tests

(module+ test
  (require rackunit)

  ;; Commands

  (test-case
    "offer"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (accept m)) j)))))
    (for ([i 10]) (check-pred void? (sync (offer m i))))
    (void (sync t)))

  (test-case
    "accept"
    (define m (make-mediator))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (offer m i)))))))
    (for ([j 10]) (check = (sync (accept m)) j))
    (void (sync t)))

  (test-case
    "put"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (get m)) j)))))
    (for ([i 10]) (check-pred void? (sync (put m i))))
    (void (sync t)))

  (test-case
    "get"
    (define m (make-mediator))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (put m i)))))))
    (for ([j 10]) (check = (sync (get m)) j))
    (void (sync t)))

  ;; Hooks

   (test-case
     "on-offer"
     (define m (make-mediator))
     (on-offer m add1)
     (define t
       (thread (λ () (for ([j 10]) (check = (sync (accept m)) (add1 j))))))
     (for ([i 10]) (check-pred void? (sync (offer m i))))
     (void (sync t)))

  (test-case
    "on-accept"
    (define m (make-mediator))
    (on-accept m add1)
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (offer m i)))))))
    (for ([j 10]) (check = (sync (accept m)) (add1 j)))
    (void (sync t)))

  (test-case
    "on-put"
    (define m (make-mediator))
    (on-put m add1)
    (define t (thread (λ () (for ([j 10]) (check = (sync (get m)) (add1 j))))))
    (for ([i 10]) (check-pred void? (sync (put m i))))
    (void (sync t)))

  (test-case
    "on-get"
    (define m (make-mediator))
    (on-get m add1)
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (put m i)))))))
    (for ([j 10]) (check = (sync (get m)) (add1 j)))
    (void (sync t)))

  (test-case
    "on-*"
    (define m (make-mediator))
    (on-offer m (curry + 1))
    (on-offer m (curry * 2))
    (on-accept m (curry + 3))
    (on-accept m (curry * 4))
    (on-put m (curry + 5))
    (on-put m (curry * 6))
    (on-get m (curry + 7))
    (on-get m (curry * 8))
    (define t1
      (thread (λ () (for ([i 10]) (check-pred void? (sync (offer m i)))))))
    (define t2
      (thread (λ () (for ([i 10]) (check-pred void? (sync (put m i)))))))
    (for ([j 10]) (check = (sync (accept m)) (* 4 (+ 3 (+ 1 (* 2 j))))))
    (for ([j 10]) (check = (sync (get m)) (* 8 (+ 7 (+ 5 (* 6 j))))))
    (void (sync t1))
    (void (sync t2))))
