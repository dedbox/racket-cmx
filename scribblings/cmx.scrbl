#lang scribble/manual

@title{A calculus of mediated exchange (cmx)}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  "diagram.rkt"
  event/racket
  (only-in cmx/mediator handler/c)
  racket/contract/base
  racket/sandbox
  scribble/examples
  (for-label cmx
             cmx/mediator
             event
             racket/base
             racket/contract/base))

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define cmx-evaluator
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize
       ([sandbox-output 'string]
        [sandbox-error-output 'string])
       (make-evaluator 'racket #:requires '(cmx cmx/mediator event))))))

@(define-syntax-rule (example expr ...)
   @examples[
     #:eval cmx-evaluator
     #:label #f
     expr ...
   ])

This package implements a calculus of mediated @tech{exchange} for expressing
synchronizable rendezvous events with any number of participants linked
arbitrarily. The calculus models a discrete communication as a series of
rendezvous operations carried out by programmable forwarding constructs called
@tech{mediators}.

@section{Exchanges}

@defmodule[cmx]

An @deftech{exchange} is a process by which some number of threads transfer
values through one or more @tech{mediators}. An exchange is
@deftech{push-based} when the sender initiates with a passive receiver, and
@deftech{pull-based} when the receiver initiates with a passive sender.

@subsection{Simple push-based exchanges}

A simple exchange is a three-step process involving a single sender and
receiver.

@itemlist[
  @item{The initiating side offers a base mediator and the passive side
    accepts.}
  @item{The passive side offers a final mediator through the base mediator and
    the initiating side accepts.}
  @item{One side puts values into the final mediator as the other side gets
    the values out.}
  #:style 'ordered
]

@deftogether[(
  @defproc[(say [m mediator?] [v any/c] ...) evt?]
  @defproc[(say* [m mediator?]
                 [vs (listof any/c)]
                 [m0 mediator? (make-mediator)]) evt?]
)]{

  @(offer "m" "m0")
  @(accept "m0" "m*")
  @(put "m*" "vs")

  Returns a @rtech{synchronizable event} that performs the initiating side of
  a simple @tech{push-based} @tech{exchange}. Offers @var[m0] to @var[m],
  accepts a final @tech{mediator} @racketid[m*] from @var[m0], and puts
  @var[v]s into @racketid[m*]. Becomes @rtech{ready for synchronization} when
  the receiver is ready to accept @var[vs] from the data channel of
  @racketid[m*].

  @example[
    (define M (make-mediator))
    (eval:alts (thread (λ () (sync (say M 123))))
               (void (thread (λ () (sync (say M 123))))))
    (sync (hear M))
  ]
}

@defproc[(hear [m mediator?] [make-m* (-> mediator? mediator?) values]) evt?]{

  @(accept "m" "m0")
  @(offer "m0" "m*")
  @(get "m*" "vs")

  Returns a @rtech{synchronizable event} that performs the passive side of a
  simple @tech{push-based} @tech{exchange}. Accepts a base @tech{mediator}
  @racketid[m0] from @var[m], creates a final @tech{mediator} @racketid[m*] by
  applying @var[make-m*] to @racketid[m0], offers @var[m*] to @racketid[m0],
  and gets values from @racketid[m*]. Becomes @rtech{ready for
  synchronization} when the sender is ready to provide values through the data
  channel of @racketid[m*]. The @rtech{synchronization result} is the provided
  values.

}

@defproc[(ask [m mediator?] [m0 mediator? (make-mediator)]) evt?]{

  @(offer "m" "m0")
  @(accept "m0" "m*")
  @(get "m*" "vs")

  Returns a @rtech{synchronizable event} that performs the initiating side of
  a simple @tech{pull-based} @tech{exchange}. Offers a base @tech{mediator}
  @racketid[m0] to @var[m], accepts a final @tech{mediator} @racketid[m*] from
  @racketid[m0], and gets values from @racketid[m*]. Becomes @rtech{ready for
  synchronization} when the sender is ready to provide values through the data
  channel of the final @tech{mediator}. The @rtech{synchronization result} is
  the provided values.

  @example[
    (define M (make-mediator))
    (eval:alts (thread (λ () (sync (tell M 123))))
               (void (thread (λ () (sync (tell M 123))))))
    (sync (ask M))
  ]
}

@deftogether[(
  @defproc[(tell [m mediator?] [v any/c] ...) evt?]
  @defproc[(tell* [m mediator?] [vs (listof any/c)]) evy?]
)]{

  @(accept "m" "m0")
  @(offer "m0" "m*")
  @(put "m*" "vs")

  Returns a @rtech{synchronizable event} that performs the passive side of a
  simple @tech{pull-based} @tech{exchange}. Accepts a base @tech{mediator}
  @racketid[m0] from @var[m], creates a final @tech{mediator} @racketid[m*] by
  applying @var[make-m*] to @racketid[m0], offers @racketid[m*] to
  @racketid[m0], and puts @var[v]s into @racketid[m*]. Becomes @rtech{ready
  for synchronization} when the receiver is ready to accept @var[v]s from the
  data channel of @racketid[m*].

}

@subsection{Forwarding exchanges}

@defproc[(forward [m1 mediator?] [m2 mediator?]) evt?]{

  @(accept "m1" "m0")
  @(offer "m2" "m0")

  Returns a @rtech{synchronizable event} that accepts a base @tech{mediator}
  from @var[m1] and then offers it to @var[m2]. Becomes @rtech{ready for
  synchronization} when @var[m2] accepts the base @tech{mediator}, possibly
  before the exchange is completed.

  @example[
    (define M1 (make-mediator))
    (define M2 (make-mediator))
    (eval:alts (thread (λ () (sync (say M1 1))))
               (void (thread (λ () (sync (say M1 1))))))
    (eval:alts (thread (λ () (sync (forward M1 M2))))
               (void (thread (λ () (sync (forward M1 M2))))))
    (sync (hear M2))
  ]
}

@defproc[
  (dispatch [m mediator?]
            [ms (hash/c any/c mediator?)]
            [default mediator?])
  evt?
]{

  Returns a @rtech{synchronizable event} that forwards a set of values from
  @var[m] to an element of @var[ms]. If the first value is a key of @var[ms],
  the remaining values are delivered to the keyed element. Otherwise, the
  remaining values are forwarded to @var[default].

  The dispatch event may finish before the values are delivered, but the
  sender and receiver are guaranteed to synchronize on each other.

}

@subsection{Multi-sender exchanges}

@defproc[(collect [m mediator?] [N exact-nonnegative-integer?]) evt?]{

  Returns a @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} after receiving @var[N] values from @var[m]. The
  @rtech{synchronization result} is a list of the @var[N] values.

  @example[
    (define M (make-mediator))
    (for ([i 10]) (thread (λ () (sync (say M i)) (display i))))
    (sync (collect M 3))
  ]
}

@defproc[(quorum [m mediator?] [N exact-nonnegative-integer?]) evt?]{

  Returns a @rtech{synchronizable event} that performs the passive side of a
  simple @tech{push-based} @tech{exchange} many times concurrently. Blocks
  until @var[N] senders are ready to provide values. Becomes @rtech{ready for
  synchronization} when the exchange is complete. The @rtech{synchronization
  result} is the provided values.

  @example[
    (define M (make-mediator))
    (for ([i 10]) (thread (λ () (sync (say M i)))))
    (sync (quorum M 6))
  ]
}

@subsection{Multi-receiver exchanges}

@defproc[(broadcast [m mediator?] [ms (listof mediator?)]) evt?]{

  Returns a @rtech{synchronizable event} that performs the initiating side of
  a simple @tech{push-based} @tech{exchange} many times concurrently. Blocks
  the sender on @var[m] until all @var[ms] have a receiver ready. Becomes
  @rtech{ready for synchronization} when the exchange is complete.

  @example[
    (define M (make-mediator))
    (define M1 (make-mediator))
    (define M2 (make-mediator))
    (sync
     (async-void
      (thread (λ () (sync (say M 'X))))
      (thread (λ () (sync (broadcast M (list M1 M2)))))
      (thread (λ () (write (sync (hear M1)))))
      (thread (λ () (write (sync (hear M2)))))))
  ]
}

@defproc[
  (multicast [m mediator?]
             [ms (hash/c any/c mediator?)]
             [default (-> (listof (or/c mediator? #f)) list? list? mediator?)
                      (λ _ void-mediator)])
  evt?
]{

  Returns a @rtech{synchronizable event} that forwards an exchange on @var[m]
  to at least one of the @var[ms] or a @tech{mediator} created by
  @var[default]. If the value being exchanged is a list and its first element
  is a list of keys of @var[ms], the remaining elements are broadcasted to the
  keyed @tech{mediators}. Otherwise, the operation is restarted on a
  @tech{mediator} created by applying @var[default] to a list of the hash
  lookup results, a list of the lookup keys, and a list of the remaining
  elements of the value being exchanged.

  @example[
    (define M (make-mediator))
    (define Ms (for/hash ([i 10]) (values i (make-mediator))))
    (sync
     (async-void
      (thread (λ () (sync (say M '(1 3 4 7 8) 'X))))
      (thread (λ () (sync (multicast M Ms))))
      (thread (λ () (sync (hear (hash-ref Ms 1))) (write 1)))
      (thread (λ () (sync (hear (hash-ref Ms 3))) (write 3)))
      (thread (λ () (sync (hear (hash-ref Ms 4))) (write 4)))
      (thread (λ () (sync (hear (hash-ref Ms 7))) (write 7)))
      (thread (λ () (sync (hear (hash-ref Ms 8))) (write 8)))))
  ]
}

@section{Mediator}

@defmodule[cmx/mediator]

A @deftech{mediator} is an extensible synchronization primitive capable of
communicating over two distinct channels---one for establishing control and
another for transferring data. Concretely, a mediator is a set of handlers. A
@deftech{handler} is a function that implements, extends, or overrides one of
the four basic @deftech{mediated operations}: @racket[offer], @racket[accept],
@racket[put], and @racket[get]. A @deftech{hook} is a function that extends
the behavior of a mediated operation by transforming the values being
exchanged.

@defthing[
  handler/c contract?
  #:value (-> (unconstrained-domain-> evt?)
              (unconstrained-domain-> evt?))
]{

  A @rtech{flat contract} that accepts @tech{handler} functions.

}

@defstruct*[
  mediator ([offer-handler handler/c]
            [accept-handler handler/c]
            [put-handler handler/c]
            [get-handler handler/c])
]{

  A structure type for @tech{mediators}. The behavior of a mediator is
  determined by four @tech{handler} functions.

  @itemlist[
    @item{The @deftech{offer-handler} takes any number of arguments and
      returns a @rtech{synchronizable event} that puts the argument list into
      the control channel of the @tech{mediator}.}
    @item{The @deftech{accept-handler} takes no arguments and returns a
      @rtech{synchronizable event} that gets an argument list from the control
      channel of the @tech{mediator}. The @rtech{synchronization result} is
      the elements of the argument list.}
    @item{The @deftech{put-handler} takes any number of arguments and returns
      a @rtech{synchronizable event} that puts the argument list into the data
      channel of the @tech{mediator}.}
    @item{The @deftech{get-handler} takes no arguments and returns a
      @rtech{synchronizable event} that gets an argument list from the data
      channel of the @tech{mediator}. The @rtech{synchronization result} is
      the elements of the argument list.}
  ]

  These functions may be extended with @tech{hooks} and overridden with
  @racket[bind-offer], @racket[bind-accept], @racket[bind-put], and
  @racket[bind-get].

}

@defproc[(make-mediator) mediator?]{

  Creates and returns a new @tech{mediator} with default @tech{handlers} and
  no @tech{hooks}.

}

@defthing[void-mediator mediator?]{

  A mediator that does nothing and always returns @(values void-const).

}

@deftogether[(
  @defproc[(offer [m mediator?] [v any/c] ...) evt?]
  @defproc[(offer* [m mediator?] [vs (listof any/c)]) evt?]
)]{

  Applies the @tech{offer-handler} of @var[m] to @var[v]s and returns a
  @rtech{synchronizable event} which by default provides @var[v]s through the
  control channel of @var[m].

}

@defproc[(accept [m mediator?]) evt?]{

  Returns a @rtech{synchronizable event} which by default blocks until values
  are provided through the control channel of @var[m], applies the
  @tech{accept-handler} of @var[m] to the provided values, and produces the
  results as its @rtech{synchronization result}.

}

@deftogether[(
  @defproc[(put [m mediator?] [v any/c] ...) evt?]
  @defproc[(put* [m mediator?] [vs (listof any/c)]) evt?]
)]{

  Applies the @tech{put-handler} of @var[m] to @var[v]s and returns a
  @rtech{synchronizable event} which by default provides @var[v]s through the
  data channel of @var[m].

}

@defproc[(get [m mediator?]) evt?]{

  Returns a @rtech{synchronizable event} which by default blocks until values
  are provided through the data channel of @var[m], applies the
  @tech{get-handler} of @var[m] to the provided values, and produces the
  results as its @rtech{synchronization result}.

}

@subsection{Handlers}

@defproc[
  (bind-offer
   [m mediator?]
   [f (-> (unconstrained-domain-> evt?)
          (unconstrained-domain-> evt?))])
  mediator?
]{

  Returns a copy of @var[m] with a new @tech{offer-handler} created by
  applying @var[f] to the old @tech{offer-handler}.

  @example[
    (define M
      (bind-offer
       (make-mediator)
       (λ (next) (λ vs (apply next (map add1 vs))))))
    (eval:alts
     (thread (λ () (sync (offer M 1 2 3))))
     (void (thread (λ () (sync (offer M 1 2 3))))))
    (sync (accept M))
  ]

  @example[
    (define M (bind-offer (make-mediator) (λ _ (λ _ (pure 0)))))
    (code:line (sync (offer M 1 2 3)) (code:comment "no accept"))
  ]
}

@defproc[(bind-accept [m mediator?] [f (-> evt? evt?)]) mediator?]{

  Returns a copy of @var[m] with a new @tech{accept-handler} created by
  applying @var[f] to the old @tech{accept-handler}.

  @example[
    (define M
      (bind-accept
       (make-mediator)
       (λ (next) (λ () (fmap (λ vs (map add1 vs)) (next))))))
    (eval:alts
     (thread (λ () (sync (offer M 1 2 3))))
     (void (thread (λ () (sync (offer M 1 2 3))))))
    (sync (accept M))
  ]

  @example[
    (define M (bind-accept (make-mediator) (λ _ (λ _ (pure 0)))))
    (code:line (sync (accept M)) (code:comment "no offer"))
  ]
}

@defproc[
  (bind-put
   [m mediator?]
   [f (-> (unconstrained-domain-> evt?)
          (unconstrained-domain-> evt?))])
  mediator?
]{

  Returns a copy of @var[m] with a new @tech{put-handler} created by applying
  @var[f] to the old @tech{put-handler}.

  @example[
    (define M
      (bind-put
       (make-mediator)
       (λ (next) (λ vs (apply next (map add1 vs))))))
    (eval:alts
     (thread (λ () (sync (put M 1 2 3))))
     (void (thread (λ () (sync (put M 1 2 3))))))
    (sync (get M))
  ]

  @example[
    (define M (bind-put (make-mediator) (λ _ (λ _ (pure 0)))))
    (code:line (sync (put M 1 2 3)) (code:comment "no get"))
  ]
}

@defproc[(bind-get [m mediator?] [f (-> evt? evt?)]) mediator?]{

  Returns a copy of @var[m] with a new @tech{get-handler} created by applying
  @var[f] to the old @tech{get-handler}.

  @example[
    (define M
      (bind-get
       (make-mediator)
       (λ (next) (λ _ (fmap (λ vs (map add1 vs)) (next))))))
    (eval:alts (thread (λ () (sync (put M 1 2 3))))
               (void (thread (λ () (sync (put M 1 2 3))))))
    (sync (get M))
  ]

  @example[
    (define M (bind-get (make-mediator) (λ _ (λ _ (pure 0)))))
    (code:line (sync (get M)) (code:comment "no put"))
  ]
}

@subsection{Hooks}

@defproc[(on-offer [m mediator?] [f procedure?]) mediator?]{

  Returns a copy of @var[m] extended to apply @var[f] to any values offered
  through @var[m]. When a @tech{mediator} has more than one @racket[on-offer]
  hook, the hooks are applied in reverse order.

  @example[
    (define M (make-mediator))
    (set! M (on-offer M (curry cons 1)))
    (set! M (on-offer M (curry cons 2)))
    (eval:alts (thread (λ () (sync (offer M null))))
               (void (thread (λ () (sync (offer M null))))))
    (sync (accept M))
  ]
}

@defproc[(on-accept [m mediator?] [f procedure?]) mediator?]{

  Returns a copy of @var[m] extended to apply @var[f] to any values accepted
  through @var[m]. When a @tech{mediator} has more than one @racket[on-accept]
  hook, the hooks are applied in order.

  @example[
    (define M (make-mediator))
    (set! M (on-accept M (curry cons 1)))
    (set! M (on-accept M (curry cons 2)))
    (eval:alts (thread (λ () (sync (offer M null))))
               (void (thread (λ () (sync (offer M null))))))
    (sync (accept M))
  ]
}

@defproc[(on-put [m mediator?] [f procedure?]) mediator?]{

  Returns a copy of @var[m] extended to apply @var[f] to any values put
  through @var[m]. When a @tech{mediator} has more than one @racket[on-put]
  hook, the hooks are applied in reverse order.

  @example[
    (define M (make-mediator))
    (set! M (on-put M (curry cons 1)))
    (set! M (on-put M (curry cons 2)))
    (eval:alts (thread (λ () (sync (put M null))))
               (void (thread (λ () (sync (put M null))))))
    (sync (get M))
  ]
}

@defproc[(on-get [m mediator?] [f procedure?]) mediator?]{

  Returns a copy of @var[m] extended to apply @var[f] to any values gotten
  through @var[m]. When a @tech{mediator} has more than one @racket[on-get]
  hook, the hooks are applied in order.

  @example[
    (define M (make-mediator))
    (set! M (on-get M (curry cons 1)))
    (set! M (on-get M (curry cons 2)))
    (eval:alts (thread (λ () (sync (put M null))))
               (void (thread (λ () (sync (put M null))))))
    (sync (get M))
  ]
}
