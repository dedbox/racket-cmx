#lang racket/base

(provide (all-defined-out))

(require
 event
 racket/function)

(struct mediator (offer accept put get))

(define (make-mediator)
  (define ctrl-ch (make-channel))
  (define data-ch (make-channel))
  (mediator
   (λ vs (event-void (channel-put-evt ctrl-ch vs)))
   (fmap (curry apply values) ctrl-ch)
   (λ vs (event-void (channel-put-evt data-ch vs)))
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

(define (offer/put m vs)
  (seq (offer m m) (put* m vs)))

(define (offer/get m)
  (seq (offer m m) (get m)))

(define (accept/put m vs)
  (bind (accept m) (curryr put* vs)))

(define (accept/get m)
  (bind (accept m) get))

;; Hooks

(define (on-offer m f)
  (on-offer* m (λ (next) (λ vs (bind (pure (apply f vs)) next)))))

(define (on-offer* m f)
  (define next (mediator-offer m))
  (struct-copy mediator m [offer (λ vs (apply (f next) vs))]))

(define (on-accept m f)
  (on-accept* m (λ (next) (fmap f next))))

(define (on-accept* m f)
  (struct-copy mediator m [accept (f (mediator-accept m))]))

(define (on-put m f)
  (on-put* m (λ (next) (λ vs (bind (pure (apply f vs)) next)))))

(define (on-put* m f)
  (struct-copy mediator m [put (f (mediator-put m))]))

(define (on-get m f)
  (on-get* m (curry fmap f)))

(define (on-get* m f)
  (struct-copy mediator m [get (f (mediator-get m))]))

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
     (define m (on-offer (make-mediator) add1))
     (define t
       (thread (λ () (for ([j 10]) (check = (sync (accept m)) (add1 j))))))
     (for ([i 10]) (check-pred void? (sync (offer m i))))
     (void (sync t)))

  (test-case
    "on-accept"
    (define m (on-accept (make-mediator) add1))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (offer m i)))))))
    (for ([j 10]) (check = (sync (accept m)) (add1 j)))
    (void (sync t)))

  (test-case
    "on-put"
    (define m (on-put (make-mediator) add1))
    (define t (thread (λ () (for ([j 10]) (check = (sync (get m)) (add1 j))))))
    (for ([i 10]) (check-pred void? (sync (put m i))))
    (void (sync t)))

  (test-case
    "on-get"
    (define m (on-get (make-mediator) add1))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (put m i)))))))
    (for ([j 10]) (check = (sync (get m)) (add1 j)))
    (void (sync t)))

  (test-case
    "on-*"
    (define m (make-mediator))
    (set! m (on-offer m (curry + 1)))
    (set! m (on-offer m (curry * 2)))
    (set! m (on-accept m (curry + 3)))
    (set! m (on-accept m (curry * 4)))
    (set! m (on-put m (curry + 5)))
    (set! m (on-put m (curry * 6)))
    (set! m (on-get m (curry + 7)))
    (set! m (on-get m (curry * 8)))
    (define t1
      (thread (λ () (for ([i 10]) (check-pred void? (sync (offer m i)))))))
    (define t2
      (thread (λ () (for ([i 10]) (check-pred void? (sync (put m i)))))))
    (for ([j 10]) (check = (sync (accept m)) (* 4 (+ 3 (+ 1 (* 2 j))))))
    (for ([j 10]) (check = (sync (get m)) (* 8 (+ 7 (+ 5 (* 6 j))))))
    (void (sync t1))
    (void (sync t2))))
