#lang scribble/manual

@title{A calculus of mediated exchange (cmx)}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  cmx/mediator
  racket/contract/base
  racket/sandbox
  scribble/examples
  (for-label cmx
             cmx/mediator
             racket/base
             racket/contract/base))

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define cmx-evaluator
   (parameterize
       ([sandbox-output 'string]
        [sandbox-error-output 'string]
        [sandbox-memory-limit 50]
        [sandbox-eval-limits '(30 50)]
        [sandbox-make-inspector current-inspector])
     (make-evaluator 'racket #:requires '(cmx cmx/mediator))))

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

@defproc[(mediator? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{mediator}, @racket[#f] otherwise.

}

@defproc[(make-mediator) mediator?]{

  Creates and returns a new @tech{mediator} with default @tech{handlers} and
  no @tech{hooks}.

}

@deftogether[(
  @defproc[(offer [m mediator?] [v any/c] ...) evt?]
  @defproc[(offer* [m mediator?] [vs (listof any/c)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that applies the offer-handler of
  @var[m] to @var[vs] and then blocks until a receiver is ready to accept the
  results through the control channel of @var[m].

}

@defproc[(accept [m mediator?]) evt?]{

  Returns a @rtech{synchronizable event} that blocks until a sender is ready
  to provide values through the control channel of @var[m], applies the
  accept-handler of @var[m] to the provided values, and uses the results as
  its @rtech{synchronization result}.

}

@deftogether[(
  @defproc[(put [m mediator?] [v any/c] ...) evt?]
  @defproc[(put* [m mediator?] [vs (listof any/c)]) evt?]
)]{

  Returns a @rtech{synchronizable event} that applies the put-handler of
  @var[m] to @var[vs] and then blocks until a receiver is ready to accept the
  results through the data channel of @var[m].

}

@defproc[(get [m mediator?]) evt?]{

  Returns a @rtech{synchronizable event} that blocks until a sender is ready
  to provide values through the data channel of @var[m], applies the
  get-handler of @var[m] to the provided values, and uses the results as its
  @rtech{synchronization result}.

}

@subsection{Handlers}

@defproc[
  (bind-offer
   [m mediator?]
   [f (-> (unconstrained-domain-> evt?)
          (unconstrained-domain-> evt?))])
  mediator?
]{

  Returns a copy of @var[m] with a new offer-@tech{handler} created by
  applying @var[f] to the old @tech{handler}.

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
    (sync (offer M 1 2 3))
  ]
}

@defproc[(bind-accept [m mediator?] [f (-> evt? evt?)]) mediator?]{

  Returns a copy of @var[m] with a new accept-@tech{handler} created by
  applying @var[f] to the old @tech{handler}.

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
    (sync (accept M))
  ]
}

@defproc[
  (bind-put
   [m mediator?]
   [f (-> (unconstrained-domain-> evt?)
          (unconstrained-domain-> evt?))])
  mediator?
]{

  Returns a copy of @var[m] with a new put-@tech{handler} created by applying
  @var[f] to the old @tech{handler}.

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
    (sync (put M 1 2 3))
  ]
}

@defproc[(bind-get [m mediator?] [f (-> evt? evt?)]) mediator?]{

  Returns a copy of @var[m] with a new get-@tech{handler} created by applying
  @var[f] to the old @tech{handler}.

  @example[
    (define M
      (bind-get
       (make-mediator)
       (λ (next) (λ _ (handle-evt (next) (λ vs (map add1 vs)))))))
    (eval:alts
     (thread (λ () (sync (put M 1 2 3))))
     (void (thread (λ () (sync (put M 1 2 3))))))
    (sync (get M))
  ]

  @example[
    (define M
      (bind-get
       (make-mediator)
       (λ _ (λ _ (handle-evt always-evt (λ _ 0))))))
    (sync (get M))
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
    (eval:alts
     (thread (λ () (sync (offer M null))))
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
    (eval:alts
     (thread (λ () (sync (offer M null))))
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
    (eval:alts
     (thread (λ () (sync (put M null))))
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
    (eval:alts
     (thread (λ () (sync (put M null))))
     (void (thread (λ () (sync (put M null))))))
    (sync (get M))
  ]
}

@section{Exchanges}

@defmodule[cmx]

@deftogether[(
)]
  @defproc[(say [m mediator?] [v any/c] ...) evt?]
  @defproc[(say* [m mediator?] [vs (listof any/c)]) evt?]


@defproc[(ask [m mediator?]) evt?]
@defproc[(hear [m mediator?]) evt?]{

}
@deftogether[(
  @defproc[(tell [m mediator?] [v any/c] ...) evt?]
  @defproc[(tell* [m mediator?] [vs (listof any/c)]) evy?]
)]

@defproc[(collect [m mediator?] [N exact-nonnegative-integer?]) evt?]

@defproc[(quorum [m mediator?] [N exact-nonnegative-integer?]) evt?]

@defproc[(forward [m1 mediator?] [m2 mediator?]) evt?]

@defproc[(broadcast [m mediator?] [ms (listof mediator?)]) evt?]
