#lang scribble/manual

@title{A calculus of mediated exchange (cmx)}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  "diagram.rkt"
  event/base
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

This package introduces a calculus of mediated exchange in which one can
naturally express synchronizable rendezvous events for any number of
participants. The participants of an exchange may be arbitrarily linked, and
an exchange may carry information which changes that linkage.

The calculus serves as a theoretical foundation for a set of exchangers
provided by the @racketmodname[cmx] module. The @racketmodname[cmx/mediator]
module provides support for creating new kinds of exchangers based on the
calculus.

@section{Mediator}

@defmodule[cmx/mediator]

A @deftech{mediator} is an extensible synchronization primitive capable of
communicating over two distinct channels---one for establishing control and
another for transferring data. Concretely, a mediator is a set of handlers. A
@deftech{handler} is a function that implements, extends, or overrides one of
the four basic @deftech{mediated operations}: @racket[offer], @racket[accept],
@racket[put], and @racket[get]. A @deftech{hook} is a function that extends
the behavior of a mediated operation by returning a value derived from its
input.

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
    @item{The @var[offer-handler] takes any number of arguments. Returns a
      @rtech{synchronizable event} that puts the argument list into the
      @emph{control channel} of the @tech{mediator}.}
    @item{The @var[accept-handler] takes no arguments. Returns a
      @rtech{synchronizable event} that gets an argument list from the
      @emph{control channel} of the @tech{mediator}. The
      @rtech{synchronization result} is the elements of the argument list.}
    @item{The @var[put-handler] takes any number of arguments. Returns a
      @rtech{synchronizable event} that puts the argument list into the
      @emph{data channel} of the @tech{mediator}.}
    @item{The @var[get-handler] takes no arguments. Returns a
      @rtech{synchronizable event} that gets an argument list from the
      @emph{data channel} of the @tech{mediator}. The @rtech{synchronization
      result} is the elements of the argument list.}
  ]
}

@defproc[(make-mediator) mediator?]{

  Creates and returns a new @tech{mediator} with default @tech{handlers} and
  no @tech{hooks}.

}

@deftogether[(
  @defproc[(offer [m mediator?] [v any/c] ...) evt?]
  @defproc[(offer* [m mediator?] [vs (listof any/c)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that applies the
  @racket[mediator-offer-handler] of @var[m] to @var[vs] and then blocks until
  a receiver is ready to accept the results through the control channel of
  @var[m].

}

@defproc[(accept [m mediator?]) evt?]{

  Returns a @rtech{synchronizable event} that blocks until a sender is ready
  to provide values through the control channel of @var[m], applies the
  @racket[mediator-accept-handler] of @var[m] to the provided values, and uses
  the results as its @rtech{synchronization result}.

}

@deftogether[(
  @defproc[(put [m mediator?] [v any/c] ...) evt?]
  @defproc[(put* [m mediator?] [vs (listof any/c)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that applies the
  @racket[mediator-put-handler] of @var[m] to @var[vs] and then blocks until a
  receiver is ready to accept the results through the data channel of @var[m].

}

@defproc[(get [m mediator?]) evt?]{

  Returns a @rtech{synchronizable event} that blocks until a sender is ready
  to provide values through the data channel of @var[m], applies the
  @racket[mediator-get-handler] of @var[m] to the provided values, and uses the results
  as its @rtech{synchronization result}.

}

@subsection{Handlers}

@defproc[
  (bind-offer
   [m mediator?]
   [f (-> (unconstrained-domain-> evt?)
          (unconstrained-domain-> evt?))])
  mediator?
]{

  Returns a copy of @var[m] with a new @racket[mediator-offer-handler] created
  by applying @var[f] to the old @racket[mediator-offer-handler].

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
    (define M
      (bind-offer
       (make-mediator)
       (λ _ (λ _ (handle-evt always-evt (λ _ 0))))))
    (code:line (sync (offer M 1 2 3)) (code:comment "no accept"))
  ]
}

@defproc[(bind-accept [m mediator?] [f (-> evt? evt?)]) mediator?]{

  Returns a copy of @var[m] with a new @racket[mediator-accept-handler]
  created by applying @var[f] to the old @racket[mediator-accept-handler].

  @example[
    (define M
      (bind-accept
       (make-mediator)
       (λ (next) (λ () (handle-evt (next) (λ vs (map add1 vs)))))))
    (eval:alts
     (thread (λ () (sync (offer M 1 2 3))))
     (void (thread (λ () (sync (offer M 1 2 3))))))
    (sync (accept M))
  ]

  @example[
    (define M
      (bind-accept
       (make-mediator)
       (λ _ (λ _ (handle-evt always-evt (λ _ 0))))))
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

  Returns a copy of @var[m] with a new @racket[mediator-put-handler] created
  by applying @var[f] to the old @racket[mediator-put-handler].

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
    (define M
      (bind-put
       (make-mediator)
       (λ _ (λ _ (handle-evt always-evt (λ _ 0))))))
    (code:line (sync (put M 1 2 3)) (code:comment "no get"))
  ]
}

@defproc[(bind-get [m mediator?] [f (-> evt? evt?)]) mediator?]{

  Returns a copy of @var[m] with a new @racket[mediator-get-handler] created
  by applying @var[f] to the old @racket[mediator-get-handler].

  @example[
    (define M
      (bind-get
       (make-mediator)
       (λ (next) (λ _ (handle-evt (next) (λ vs (map add1 vs)))))))
    (eval:alts (thread (λ () (sync (put M 1 2 3))))
               (void (thread (λ () (sync (put M 1 2 3))))))
    (sync (get M))
  ]

  @example[
    (define M
      (bind-get
       (make-mediator)
       (λ _ (λ _ (handle-evt always-evt (λ _ 0))))))
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

  Returns a copy of @var[m] extended to apply @var[f] to any values got
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

@section{Exchanges}

@defmodule[cmx]

An @deftech{exchange} is a process by which some number of threads transmit
information through one or more @tech{mediators}. An exchange is
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
  @item{One side puts a set of values into the final mediator as the other
    side gets them out.}
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

@subsection{Multiple senders}

@defproc[(collect [m mediator?] [N exact-nonnegative-integer?]) evt?]{

  Returns a @rtech{synchronizable event} that performs the passive side of a
  simple @tech{push-based} @tech{exchange} iteratively. Becomes @rtech{ready
  for synchronization} after @var[N] values are received. Uses a list of the
  received values as its @rtech{synchronization result}.

  @example[
    (define M (make-mediator))
    (for ([i 10]) (thread (λ () (sync (say M i)))))
    (sync (collect M 3))
  ]
}

@defproc[(quorum [m mediator?] [N exact-nonnegative-integer?]) evt?]{

  Returns a @rtech{synchronizable event} that performs the passive side of a
  simple @tech{push-based} @tech{exchange} multiple times simultaneously.
  Blocks until @var[N] senders are ready to provide values. Becomes
  @rtech{ready for synchronization} when the exchange is complete. The
  @rtech{synchronization result} is the provided values.

  @example[
    (define M (make-mediator))
    (for ([i 10]) (thread (λ () (sync (say M i)))))
    (sync (quorum M 6))
  ]
}

@subsection{Multiple receivers}

@defproc[(broadcast [m mediator?] [ms (listof mediator?)]) evt?]{

  Returns a @rtech{synchronizable event} that performs the initiating side of
  a simple @tech{push-based} @tech{exchange} multiple times simultaneously.
  Blocks a sender on @var[m] until all @var[ms] have a receiver ready. Becomes
  @rtech{ready for synchronization} when the exchange is complete.

  @example[
    (define M (make-mediator))
    (define M1 (make-mediator))
    (define M2 (make-mediator))
    (sync
     (async-void
      (thread (λ () (sync (say M 1))))
      (thread (λ () (sync (broadcast M (list M1 M2)))))
      (thread (λ () (write (sync (hear M1)))))
      (thread (λ () (write (sync (hear M2)))))))
  ]
}
