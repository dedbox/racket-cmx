#lang racket/base

(require
 cmx/mediator
 event
 racket/contract/base
 racket/function
 racket/list)

(provide (all-from-out cmx/mediator))

(provide
 (contract-out
  [say (-> mediator? any/c ... evt?)]
  [say* (->* (mediator? list?) (mediator?) evt?)]
  [hear (->* (mediator?) (procedure?) evt?)]
  [ask (->* (mediator?) (mediator?) evt?)]
  [tell (-> mediator? any/c ... evt?)]
  [tell* (->* (mediator? list?) (procedure?) evt?)]
  [forward (-> mediator? mediator? evt?)]
  [filter (->* (mediator? mediator?) (#:offer procedure?
                                      #:accept procedure?
                                      #:put procedure?
                                      #:get procedure?) evt?)]
  [couple (->* (mediator? mediator?) (mediator?) evt?)]
  [dispatch (->* (mediator? (hash/c any/c mediator?)) (mediator?) evt?)]
  [collect (-> mediator? exact-nonnegative-integer? evt?)]
  [quorum (-> mediator? exact-nonnegative-integer? evt?)]
  [broadcast (-> mediator? (listof mediator?) evt?)]
  [multicast (->* (mediator? (hash/c any/c mediator?))
                  ((-> (listof (or/c mediator? #f)) list? list? mediator?))
                  evt?)]))

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

(define (filter m1 m2
                #:offer [offer-hook #f]
                #:accept [accept-hook #f]
                #:put [put-hook #f]
                #:get [get-hook #f])
  (define (add-hooks m0)
    (when offer-hook (set! m0 (on-offer m0 offer-hook)))
    (when accept-hook (set! m0 (on-accept m0 accept-hook)))
    (when put-hook (set! m0 (on-put m0 put-hook)))
    (when get-hook (set! m0 (on-get m0 get-hook)))
    m0)
  (event-let ([m0 (accept m1)]) (offer m2 (add-hooks m0))))

(define (couple m1 m2 [m0 (make-mediator)])
  (event-void
   (offer m1 m0)
   (event-let ([m0* (accept m0)]) (offer m2 m0*) (accept m0*))))

(define (dispatch m ms [default void-mediator])
  (define (dispatch-put m0)
    (bind-put
     (make-mediator)
     (λ _ (λ (k . vs) (say* (hash-ref ms k default) vs m0)))))
  (event-let ([m0 (accept m)]) (offer m0 (dispatch-put m0))))

;; Multiple senders

(define (collect m N)
  (if (<= N 0)
      (pure null)
      (fmap cons (hear m) (collect m (sub1 N)))))

(define (quorum m N)
  (event-let
   ([m0s (fmap* list (make-list N (accept m)))])
   (async-list* (map (λ (m0) (seq (offer m0 m0) (get m0))) m0s))))

(define (gather ms [N #f])
  (memoize (fmap list (async-args* (map hear ms) #:limit N))))

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

(define (multicast m ms [default (λ _ void-mediator)])
  (define (deliver m0 mks vs)
    (define md (make-mediator))
    (thread (λ () (sync (say* md vs m0))))
    (broadcast md mks))
  (define (multi-put m0)
    (bind-put
     (make-mediator)
     (λ _
       (λ (ks . vs)
         (define mks (map (λ (k) (hash-ref ms k #f)) ks))
         (if (or (null? mks) (member #f mks))
             (default mks ks vs)
             (deliver m0 mks vs))))))
  (event-let ([m0 (accept m)]) (offer m0 (multi-put m0))))

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
    (sync (async-void t1 t2)))

  (test-case "filter-put"
    (define m1 (make-mediator))
    (define m2 (make-mediator))
    (define t1 (thread (λ () (for ([i 10]) (sync (say m1 i))))))
    (define t2 (thread (λ () (for ([j 10]) (check = (sync (hear m2)) (+ j 1))))))
    (for ([_ 10]) (sync (filter m1 m2 #:put add1))))

  (test-case "filter-get"
    (define m1 (make-mediator))
    (define m2 (make-mediator))
    (define t1 (thread (λ () (for ([i 10]) (sync (say m1 i))))))
    (define t2 (thread (λ () (for ([j 10]) (check = (sync (hear m2)) (+ j 1))))))
    (for ([_ 10]) (sync (filter m1 m2 #:get add1))))

  (test-case "couple tell-hear"
    (define m1 (make-mediator))
    (define m2 (make-mediator))
    (define t1 (thread (λ () (for ([i 10]) (sync (tell m1 i))))))
    (define t2 (thread (λ () (for ([j 10]) (check = (sync (hear m2)) j)))))
    (for ([_ 10]) (sync (couple m1 m2)))
    (sync (fmap void t1 t2)))

  (test-case "couple hear-tell"
    (define m1 (make-mediator))
    (define m2 (make-mediator))
    (define t1 (thread (λ () (for ([i 10]) (sync (tell m1 i))))))
    (define t2 (thread (λ () (for ([j 10]) (check = (sync (hear m2)) j)))))
    (for ([_ 10]) (sync (couple m2 m1)))
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

  (test-case "gather"
    (define ms (build-list 10 (λ _ (make-mediator))))
    (define ts (for/list ([i 10] [m ms]) (thread (λ () (sync (say m i))))))
    (define seen null)
    (define (check-gather N)
      (define xs (sync (gather ms N)))
      (check-pred list? xs)
      (check = (length xs) N)
      (check-true
       (andmap (λ (x) (and (>= x 0) (< x 10) (not (member x seen)))) xs))
      (set! seen (append xs seen))
      (for-each (compose sync (curry list-ref ts)) xs))
    (check-gather 3)
    (check-gather 3)
    (check-gather 4)
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

  (test-case "multicast"
    (define m (make-mediator))
    (define ms (hash 0 (make-mediator)
                     1 (make-mediator)
                     2 (make-mediator)))
    (for* ([x 3] [y 3] #:when (not (= x y)))
      (define t0 (thread (λ () (sync (say m (list x y) 'X+Y)))))
      (define tx (thread (λ () (check eq? (sync (hear (hash-ref ms x))) 'X+Y))))
      (define ty (thread (λ () (check eq? (sync (hear (hash-ref ms y))) 'X+Y))))
      (void (sync (async-list (multicast m ms) t0 tx ty)))))

  (test-case "multicast example"
    (define M (make-mediator))
    (define Ms (for/hash ([i 10]) (values i (make-mediator))))
    (sync
     (async-void
      (thread (λ () (sync (say M '(1 3 4 7 8) 'X))))
      (thread (λ () (check-pred void? (sync (multicast M Ms)))))
      (thread (λ () (check eq? (sync (hear (hash-ref Ms 1))) 'X)))
      (thread (λ () (check eq? (sync (hear (hash-ref Ms 3))) 'X)))
      (thread (λ () (check eq? (sync (hear (hash-ref Ms 4))) 'X)))
      (thread (λ () (check eq? (sync (hear (hash-ref Ms 7))) 'X)))
      (thread (λ () (check eq? (sync (hear (hash-ref Ms 8))) 'X)))))))
