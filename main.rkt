#lang racket/base

;; (require
;;  cmx/mediator
;;  event
;;  racket/function
;;  racket/list)

;; (define (say m v [m0 (make-mediator)])
;;   (seq (offer m m0) (event-let ([m* (accept m0)]) (put m* v))))

;; (define (hear m [m* (make-mediator)])
;;   (event-let ([m0 (accept m)]) (offer m0 m*) (get m*)))

;; (define (ask m [m0 (make-mediator)])
;;   (seq (offer m m0) (event-let ([m* (accept m0)]) (get m*))))

;; (define (tell m v [m* (make-mediator)])
;;   (event-let ([m0 (accept m)]) (offer m0 m*) (put m* v)))

;; ;; Input

;; (define (collect m N)
;;   (if (<= N 0)
;;       (pure null)
;;       (fmap cons (hear m) (collect m (sub1 N)))))

;; (define (quorum m N [m* (make-mediator)])
;;   (fmap list
;;         (event-let ([m0s (fmap list (async-set* (make-list N (accept m))))])
;;           (async-set* (map (curryr offer m*) m0s))
;;           (async-set* (make-list N (get m*))))))

;; ;; Control

;; (define (forward m1 m2)
;;   (bind (curry offer m2) (accept m1)))

;; (define (filter m1 m2 f)
;;   )



;; (define (filter m1 m2 f [m (make-mediator)])
;;   (event-let ([m0 (accept m1)])
;;     (offer m2 m)
;;     (event-let ([m3 (accept m)])
;;       (offer m1 ()))))

;; ;; (define (filter m1 m2 f)
;; ;;   (hold m1 (λ (v) (put m2 (f v)))))

;; ;; (define (filter m1 m2 f [m (make-mediator)])
;; ;;   (event-let ([m-put (accept m1)])
;; ;;     (pure (writeln 'F1))
;; ;;     (offer m2 m)
;; ;;     (pure (writeln 'F2))
;; ;;     (event-let ([m-get (accept m)])
;; ;;       (pure (writeln 'F3))
;; ;;       (offer m-put m)
;; ;;       (pure (writeln 'F4))
;; ;;       (hold
;; ;;        m (λ (v)
;; ;;            (writeln 'F5)
;; ;;            (put m-get (f v)))))))

;; ;; Output

;; ;; (define (route m1 ms [m (make-mediator)])
;; ;;   (hold
;; ;;    m1 (λ (k+v)
;; ;;         (let ([m2 (hash-ref ms (car k+v))])
;; ;;           (put m m2 (cdr k+v))))))

;; ;;; Unit Tests

;; (module+ test
;;   (require rackunit)

;;   (test-case
;;     "say"
;;     (define m (make-mediator))
;;     (define t (thread (λ () (for ([j 10]) (check = (sync (hear m)) j)))))
;;     (for ([i 10]) (check-pred void? (sync (say m i))))
;;     (sync (fmap void t)))

;;   (test-case
;;     "hear"
;;     (define m (make-mediator))
;;     (define t (thread (λ () (for ([i 10]) (sync (say m i))))))
;;     (for ([j 10]) (check = (sync (hear m)) j))
;;     (sync (fmap void t)))

;;   (test-case
;;     "ask"
;;     (define m (make-mediator))
;;     (define t (thread (λ () (for ([i 10]) (sync (tell m i))))))
;;     (for ([j 10]) (check = (sync (ask m)) j))
;;     (sync (fmap void t)))

;;   (test-case
;;     "tell"
;;     (define m (make-mediator))
;;     (define t (thread (λ () (for ([j 10]) (check = (sync (ask m)) j)))))
;;     (for ([i 10]) (check-pred void? (sync (tell m i))))
;;     (sync (fmap void t)))

;;   (test-case
;;     "collect"
;;     (define m (make-mediator))
;;     (define t (thread (λ () (for ([i 10]) (sync (say m i))))))
;;     (check equal? (sync (collect m 5)) '(0 1 2 3 4))
;;     (check equal? (sync (collect m 5)) '(5 6 7 8 9)))

;;   (test-case
;;     "quorum"
;;     (define m (make-mediator))
;;     (define ts (for/list ([i 10]) (thread (λ () (sync (say m i))))))
;;     (define seen null)
;;     (define (check-quorum N)
;;       (define xs (sync (quorum m N)))
;;       (check-pred list? xs)
;;       (check = (length xs) N)
;;       (check-true
;;        (andmap (λ (x) (and (>= x 0) (< x 10) (not (member x seen)))) xs))
;;       (set! seen (append xs seen))
;;       (for-each (compose sync (curry list-ref ts)) xs))
;;     (check-quorum 3)
;;     (check-quorum 3)
;;     (check-quorum 4)
;;     (sync (fmap* void ts)))

;;   (test-case
;;     "forward"
;;     (define m1 (make-mediator))
;;     (define m2 (make-mediator))
;;     (define t1 (thread (λ () (for ([i 10]) (sync (say m1 i))))))
;;     (define t2 (thread (λ () (for ([j 10]) (check = (sync (hear m2)) j)))))
;;     (for ([_ 10]) (sync (forward m1 m2)))
;;     (sync (fmap void t1 t2)))

;;   ;; (test-case
;;   ;;   "filter"
;;   ;;   (define m0 (make-mediator))
;;   ;;   (define m1 (make-mediator))
;;   ;;   (define m2 (make-mediator))
;;   ;;   (define t0 (thread (λ () (for ([i 10]) (sync (put m0 m1 i))))))
;;   ;;   (define t1
;;   ;;     (thread (λ () (for ([j 10]) (check = (sync (get m2)) (add1 j))))))
;;   ;;   (for ([_ 10]) (check-pred void? (sync (filter m1 m2 add1)))))

;;   )

;; ;; (for ([i 100000])
;; ;;   (define ch1 (make-channel))
;; ;;   (define ch2 (make-channel))
;; ;;   (define L null)
;; ;;   (define t1
;; ;;     (thread
;; ;;      (λ ()
;; ;;        (sync
;; ;;         (handle-evt
;; ;;          (channel-put-evt ch1 1)
;; ;;          (λ _ (set! L (cons 1 L))))))))
;; ;;   (define t2
;; ;;     (thread
;; ;;      (λ () (sync (handle-evt ch2 (λ (v) (set! L (cons 1 L))))))))
;; ;;   (void
;; ;;    (sync
;; ;;     (replace-evt
;; ;;      ch1 (λ (v) (set! L (cons 0 L)) (channel-put-evt ch2 v)))))
;; ;;   (check = (car L) 0))
