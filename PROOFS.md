# Agenda s msg a as a monad functor

We show that `Agenda s msg a` is a monad functor with `return` given by `succeed` and composition given by

```elm
(>>=) : Agenda s msg a -> (a -> Agenda s msg b) -> Agenda s msg b
```

We prove the 3 monad laws.


## Left-Identity

```elm
succeed a >>= u   ===   u a
```

for all `u : a -> Agenda s msg b`.

Suppose `u = \a -> succeed (f a)`, for some `f : a -> b`, then

```elm
succeed a >>= \a ->
succeed (f a)

    ||

addStates [] <|
    succeed (f a)

    ||

u a
```

If `u = \a -> try (stepper a)`, for some `stepper : a -> msg -> Agenda s msg
b`, then

```elm
succeed a >>= \a ->
try (stepper a)

    ||

addStates [] <|
    try (stepper a)

    ||

u a
```

And finally if `u = \a -> fail`, we get

```elm
succeed a >>= \a ->
fail

    ||

addStates [] <|
    fail

fail
```

This proves the first law.


## Right-Identity

```elm
u >>= succeed   ===   u
```

for all `u : Agenda s msg a`.

For `u = succeed a`, we obtain

```elm
succeed a >>= \a ->
succeed a

    ||

addStates [] <|
    succeed a

    ||

succeed a
    
    ||

u
```

If `u = fail`, then

```elm
fail >>= \a ->
succeed a

    ||

fail
```

It remains to prove the identity for `u = try stepper`.  We get

```elm
try stepper >>= \a ->
succeed a

    ||

addStates [] <|
    try <|
        \msg ->
            stepper msg >>= \a ->
            succeed a

    ||

try <|
    \msg ->
        stepper msg >>= \a ->
        succeed a
```

We therefore have to show that

```elm
\msg ->
    stepper msg >>= \a ->
    succeed a

    ||

stepper
```

If `stepper msg` gives `Result a`, we have

```elm
stepper msg >>= \a ->
succeed a

    ||

succeed a >>= \a ->
succeed a

    ||

succeed a

    ||

stepper msg
```

by the previous computation.  We likewise get equality if `stepper msg ===
fail`.  If on the other hand `stepper msg` gives `try nextStepper`, this becomes

```elm
stepper msg >>= \a ->
succeed a

    ||

try nextStepper >>= \a ->
succeed a

    ||

addStates [] <|
    try <|
        \msg ->
            nextStepper msg >>= \a ->
            succeed a

    ||

try <|
    \msg ->
        nextStepper msg >>= \a ->
        succeed a
```

and we have reduced our claim to proving

```elm
\msg ->
    nextStepper msg >>= \a ->
    succeed a

    ||

nextStepper
```

since then

```elm
stepper msg >>= \a ->
succeed a
    
    ||

try <|
    \msg ->
        nextStepper msg >>= \a ->
        succeed a

    ||

try nextStepper

    ||

stepper msg
```

Thus if we assume that `try stepper` succeeds or fails after finitely many
steps, we inductively obtain the equality

```elm
try stepper >>= \a ->
succeed a

    ||

try stepper
```

This proves the second identity law.


## Composition

```elm
u >>= (\a -> v a >>= w)   ===   (u >>= v) >>= w
```

for all `u : Agenda s msg a`, `v : a -> Agenda s msg b` and `w : b -> Agenda
s msg c`.

First, we consider `u = succeed a`.  Then

```elm
succeed a >>= (\a ->
v a >>= w)

    ||

addStates [] <|
    v a >>= w

    ||

v a >>= w

    ||  by left-identity

(succeed a >>= v) >>= w
```

Certainly, for `u = fail`, we get equality.  Hence assume `u = try stepper`.
Then

```elm
try stepper >>= (\a ->
v a >>= w)

    ||

try <|
    \msg ->
        stepper msg >>= (\a ->
        v a >>= w)
```

and again assuming that `try stepper` succeeds or fails after finitely many
steps, we see that we can prove the identity inductively.



# `Agenda s msg a` as an applicative functor

We claim that `Agenda s msg a` is a functor with application, where embedding
of pure expressions is realized by `succeed`

```elm
succeed : a -> Agenda s msg a
succeed =
    Result []
```

and sequencing of computations and combining their results is given by `|=`

```elm
(|=) : Agenda s msg (a -> b) -> Agenda s msg a -> Agenda s msg b
```

The following applicative laws follow as `Agenda s msg a` is a monad.


## Identity

```elm
succeed identity |= u   ===   u
```

for all `u : Agenda s msg a`.


## Composition

```elm
succeed (<<) |= u |= v |= w   ===   u |= (v |= w)
```

for all `u : Agenda s msg (b -> c)`, `v : Agenda s msg (a -> b)` and `w
: Agenda s msg a`.


## Homomorphism

```elm
succeed f |= succeed a   ===   succeed (f a)
```

for all `f : a -> b`.


## Interchange

```elm
u |= succeed a   ===   succeed (\f -> f a) |= u
```

for all `u : Agenda s msg (a -> b)`.
