#lang racket/base

(require
 event
 racket/contract/base
 racket/function)

(provide
 (contract-out
  [handler/c contract?]
  [struct mediator
    ([offer-handler handler/c]
     [accept-handler handler/c]
     [put-handler handler/c]
     [get-handler handler/c])]
  [make-mediator (-> mediator?)]
  [void-mediator mediator?]
  [offer (-> mediator? any/c ... evt?)]
  [offer* (-> mediator? (listof any/c) evt?)]
  [accept (-> mediator? evt?)]
  [put (-> mediator? any/c ... evt?)]
  [put* (-> mediator? (listof any/c) evt?)]
  [get (-> mediator? evt?)]
  [bind-offer (-> mediator? handler/c mediator?)]
  [bind-accept (-> mediator? handler/c mediator?)]
  [bind-put (-> mediator? handler/c mediator?)]
  [bind-get (-> mediator? handler/c mediator?)]
  [on-offer (-> mediator? procedure? mediator?)]
  [on-accept (-> mediator? procedure? mediator?)]
  [on-put (-> mediator? procedure? mediator?)]
  [on-get (-> mediator? procedure? mediator?)]))

(define handler/c
  (-> (unconstrained-domain-> evt?)
      (unconstrained-domain-> evt?)))

(struct mediator (offer-handler accept-handler put-handler get-handler))

(define (make-mediator)
  (define ctrl-ch (make-channel))
  (define data-ch (make-channel))
  (mediator
   (λ vs (fmap void (channel-put-evt ctrl-ch vs)))
   (λ () (fmap (curry apply values) ctrl-ch))
   (λ vs (fmap void (channel-put-evt data-ch vs)))
   (λ () (fmap (curry apply values) data-ch))))

(define void-mediator
  (mediator
   (λ _ (pure (void)))
   (λ _ (pure (void)))
   (λ _ (pure (void)))
   (λ _ (pure (void)))))

;; Commands

(define (offer m . vs)
  (offer* m vs))

(define (offer* m vs)
  (apply (mediator-offer-handler m) vs))

(define (accept m)
  ((mediator-accept-handler m)))

(define (put m . vs)
  (put* m vs))

(define (put* m vs)
  (apply (mediator-put-handler m) vs))

(define (get m)
  ((mediator-get-handler m)))

;; Handlers

(define (bind-offer m f)
  (define handler (mediator-offer-handler m))
  (struct-copy mediator m [offer-handler (λ vs (apply (f handler) vs))]))

(define (bind-accept m f)
  (struct-copy mediator m [accept-handler (f (mediator-accept-handler m))]))

(define (bind-put m f)
  (struct-copy mediator m [put-handler (f (mediator-put-handler m))]))

(define (bind-get m f)
  (struct-copy mediator m [get-handler (f (mediator-get-handler m))]))

;; Hooks

(define (on-offer m f)
  (bind-offer m (λ (handler) (λ vs (bind (pure (apply f vs)) handler)))))

(define (on-accept m f)
  (bind-accept m (λ (handler) (λ () (fmap f (handler))))))

(define (on-put m f)
  (bind-put m (λ (handler) (λ vs (bind (pure (apply f vs)) handler)))))

(define (on-get m f)
  (bind-get m (λ (handler) (λ () (fmap f (handler))))))

;;; Unit Tests

(module+ test
  (require rackunit)

  ;; Commands

  (test-case "offer"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (accept m)) j)))))
    (for ([i 10]) (check-pred void? (sync (offer m i))))
    (void (sync t)))

  (test-case "accept"
    (define m (make-mediator))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (offer m i)))))))
    (for ([j 10]) (check = (sync (accept m)) j))
    (void (sync t)))

  (test-case "put"
    (define m (make-mediator))
    (define t (thread (λ () (for ([j 10]) (check = (sync (get m)) j)))))
    (for ([i 10]) (check-pred void? (sync (put m i))))
    (void (sync t)))

  (test-case "get"
    (define m (make-mediator))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (put m i)))))))
    (for ([j 10]) (check = (sync (get m)) j))
    (void (sync t)))

  (test-case "void-mediator"
    (check-pred void? (sync (offer void-mediator)))
    (check-pred void? (sync (accept void-mediator)))
    (check-pred void? (sync (put void-mediator)))
    (check-pred void? (sync (get void-mediator))))

  ;; Hooks

  (test-case "on-offer"
    (define m (on-offer (make-mediator) add1))
    (define t
      (thread (λ () (for ([j 10]) (check = (sync (accept m)) (add1 j))))))
    (for ([i 10]) (check-pred void? (sync (offer m i))))
    (void (sync t)))

  (test-case "on-accept"
    (define m (on-accept (make-mediator) add1))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (offer m i)))))))
    (for ([j 10]) (check = (sync (accept m)) (add1 j)))
    (void (sync t)))

  (test-case "on-put"
    (define m (on-put (make-mediator) add1))
    (define t (thread (λ () (for ([j 10]) (check = (sync (get m)) (add1 j))))))
    (for ([i 10]) (check-pred void? (sync (put m i))))
    (void (sync t)))

  (test-case "on-get"
    (define m (on-get (make-mediator) add1))
    (define t
      (thread (λ () (for ([i 10]) (check-pred void? (sync (put m i)))))))
    (for ([j 10]) (check = (sync (get m)) (add1 j)))
    (void (sync t)))

  (test-case "on-*"
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
