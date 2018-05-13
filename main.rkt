#lang racket/base

(require
 cmx/mediator
 event
 racket/function
 racket/list)

;; Simple Exchanges

(define (say-to m . vs)
  (say-to* m vs))

(define (say-to* m vs [m0 (make-mediator)])
  (seq (offer m m0) (accept/put m0 vs)))

(define (hear-from m)
  (bind (accept m) offer/get))

(define (ask m [m0 (make-mediator)])
  (seq (offer m m0) (accept/get m0)))

(define (tell m . vs)
  (tell* m vs))

(define (tell* m vs)
  (bind (accept m) (curryr offer/put vs)))

;; Input

(define (collect m N)
  (if (<= N 0)
      (pure null)
      (fmap cons (hear-from m) (collect m (sub1 N)))))

(define (quorum m N)
  (event-let ([m0s (fmap* list (make-list N (accept m)))])
    (fmap list (async-set* (map (λ (m0) (seq (offer m0 m0) (get m0))) m0s)))))

;; Control

(define (forward m1 m2)
  (bind (accept m1) (curry offer m2)))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
    "say-to"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (hear-from m)) j)))))
    (for ([i 10]) (check-pred void? (sync (say-to m i))))
    (void (sync t)))

  (test-case
    "hear-from"
    (define m (make-mediator))
    (define t (thread (λ () (for ([i 10]) (sync (say-to m i))))))
    (for ([j 10]) (check = (sync (hear-from m)) j))
    (void (sync t)))

  (test-case
    "ask"
    (define m (make-mediator))
    (define t (thread (λ () (for ([i 10]) (sync (tell m i))))))
    (for ([j 10]) (check = (sync (ask m)) j))
    (void (sync t)))

  (test-case
    "tell"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (ask m)) j)))))
    (for ([i 10]) (check-pred void? (sync (tell m i))))
    (void (sync t)))

  (test-case
    "collect"
    (define m (make-mediator))
    (define t (thread (λ () (for ([i 10]) (sync (say-to m i))))))
    (check equal? (sync (collect m 5)) '(0 1 2 3 4))
    (check equal? (sync (collect m 5)) '(5 6 7 8 9))
    (void (sync t)))

  (test-case
    "quorum"
    (define m (make-mediator))
    (define ts (for/list ([i 10]) (thread (λ () (sync (say-to m i))))))
    (define seen null)
    (define (check-quorum N)
      (define xs (sync (quorum m N)))
      (check-pred list? xs)
      (check = (length xs) N)
      (check-true
       (andmap (λ (x) (and (>= x 0) (< x 10) (not (member x seen)))) xs))
      (set! seen (append xs seen))
      (for-each (compose sync (curry list-ref ts)) xs))
    (check-quorum 3)
    (check-quorum 3)
    (check-quorum 4)
    (for-each sync ts))

  (test-case
    "forward"
    (define m1 (make-mediator))
    (define m2 (make-mediator))
    (define t1 (thread (λ () (for ([i 10]) (sync (say-to m1 i))))))
    (define t2 (thread (λ () (for ([j 10]) (check = (sync (hear-from m2)) j)))))
    (for ([_ 10]) (sync (forward m1 m2)))
    (sync (fmap void t1 t2)))

  ;; ;; (test-case
  ;; ;;   "filter"
  ;; ;;   (define m0 (make-mediator))
  ;; ;;   (define m1 (make-mediator))
  ;; ;;   (define m2 (make-mediator))
  ;; ;;   (define t0 (thread (λ () (for ([i 10]) (sync (put m0 m1 i))))))
  ;; ;;   (define t1
  ;; ;;     (thread (λ () (for ([j 10]) (check = (sync (get m2)) (add1 j))))))
  ;; ;;   (for ([_ 10]) (check-pred void? (sync (filter m1 m2 add1)))))

  )
