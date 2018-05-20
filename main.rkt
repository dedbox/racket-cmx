#lang racket/base

(require
 cmx/mediator
 event
 event/base
 racket/function
 racket/list)

(provide (all-defined-out))

;; Simple Exchanges

(define (say m . vs)
  (say* m vs))

(define (say* m vs [m0 (make-mediator)])
  (seq (offer m m0) (bind (accept m0) (curryr put* vs))))

(define (hear m [make-m* values])
  (event-let
   ([m0 (accept m)])
   (let ([m* (make-m* m0)])
     (seq (offer m0 m*) (get m*)))))

(define (ask m [m0 (make-mediator)])
  (seq (offer m m0) (bind (accept m0) get)))

(define (tell m . vs)
  (tell* m vs))

(define (tell* m vs [make-m* values])
  (event-let
   ([m0 (accept m)])
   (let ([m* (make-m* m0)])
     (seq (offer m0 m0) (put* m0 vs)))))

;; Forwarding

(define (forward m1 m2)
  (bind (accept m1) (curry offer m2)))

(define (dispatch m ms [default void-mediator])
  (define (dispatch-put m0)
    (bind-put
     (make-mediator)
     (λ (~put) (λ (k . vs) (say* (hash-ref ms k default) vs m0)))))
  (event-let ([m0 (accept m)]) (offer m0 (dispatch-put m0))))

;; Multiple senders

(define (collect m N)
  (if (<= N 0)
      (pure null)
      (fmap cons (hear m) (collect m (sub1 N)))))

(define (quorum m N)
  (event-let
   ([m0s (fmap* list (make-list N (accept m)))])
   (fmap list (async-set* (map (λ (m0) (seq (offer m0 m0) (get m0))) m0s)))))

;; Multiple receivers

(define (broadcast m ms)
  (define G (make-gate))
  (define (gated-get)
    (bind-get (make-mediator) (λ (~get) (λ () (gated G (~get))))))
  (define (gated-put mk)
    (bind-put mk (λ (~put) (λ vs (gated G (apply ~put vs))))))
  (define (offer-gated-get mk)
    (define m* (gated-get))
    (seq (offer mk m*)
         (accept m*)))
  (event-let
   ([m0 (accept m)]
    [m*s (async-list* (map offer-gated-get ms))])
   (let ([m0* (gated-put m0)])
     (seq
      (offer m0 m0*)
      (event-let
       ([v (get m0*)])
       (async-set* (map (curryr put v) m*s))
       (open-gate G))))))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case "say"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (hear m)) j)))))
    (for ([i 10]) (check-pred void? (sync (say m i))))
    (void (sync t)))

  (test-case "hear"
    (define m (make-mediator))
    (define t (thread (λ () (for ([i 10]) (sync (say m i))))))
    (for ([j 10]) (check = (sync (hear m)) j))
    (void (sync t)))

  (test-case "ask"
    (define m (make-mediator))
    (define t (thread (λ () (for ([i 10]) (sync (tell m i))))))
    (for ([j 10]) (check = (sync (ask m)) j))
    (void (sync t)))

  (test-case "tell"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (ask m)) j)))))
    (for ([i 10]) (check-pred void? (sync (tell m i))))
    (void (sync t)))

  (test-case "forward"
    (define m1 (make-mediator))
    (define m2 (make-mediator))
    (define t1 (thread (λ () (for ([i 10]) (sync (say m1 i))))))
    (define t2 (thread (λ () (for ([j 10]) (check = (sync (hear m2)) j)))))
    (for ([_ 10]) (sync (forward m1 m2)))
    (sync (fmap void t1 t2)))

  (test-case "dispatch"
    (define m (make-mediator))
    (define ms
      (hash
       1 (make-mediator)
       2 (make-mediator)))
    (define t0 (thread (λ () (sync (seq (dispatch m ms) (dispatch m ms))))))
    (define t1 (thread (λ () (sync (say m 1 'X)))))
    (define t2 (thread (λ () (sync (say m 2 'Y)))))
    (sync
     (async-void
      (thread (λ () (check eq? (sync (hear (hash-ref ms 1))) 'X)))
      (thread (λ () (check eq? (sync (hear (hash-ref ms 2))) 'Y))))))

  (test-case "collect"
    (define m (make-mediator))
    (define t (thread (λ () (for ([i 10]) (sync (say m i))))))
    (check equal? (sync (collect m 5)) '(0 1 2 3 4))
    (check equal? (sync (collect m 5)) '(5 6 7 8 9))
    (void (sync t)))

  (test-case "quorum"
    (define m (make-mediator))
    (define ts (for/list ([i 10]) (thread (λ () (sync (say m i))))))
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

  (test-case "broadcast"
    (define m (make-mediator))
    (define ms (for/list ([_ 10]) (make-mediator)))
    (define t (thread (λ () (check-pred void? (sync (say m 1))))))
    (define ts (for/list ([mk ms] [k 10]) (thread (λ () (check = (sync (hear mk)) 1)))))
    (sync (broadcast m ms)))

  (test-case "broadcast example"
    (define m (make-mediator))
    (define ms (for/list ([_ 10]) (make-mediator)))
    (define t1 (thread (λ () (check-pred void? (sync (say m 1))))))
    (define t2 (thread (λ () (check-pred void? (sync (broadcast m ms))))))
    (define ts (for/list ([mk ms]) (thread (λ () (check = (sync (hear mk)) 1)))))
    (void (sync (async-list* t1 t2 ts))))

  )
