% Polysemy
% Ian Shipman
% October 16, 2019

# Pure and impure functions

An important distinction in functional programming:

- Pure functions: result depends deterministically on inputs and nothing else
- Impure functions: depend on and produce values not visible syntactically

Monads are the standard way to encapsulate side effects.

# Composing monads

- Monad transformers: extend a base monad with new side effects
- Algebraic effects: carefully track effects and pass off to explicit handlers

# Monad transformer example: MaybeT

`MaybeT` adds a simple kind of failure to a monad.  In terms of effects, we want

```haskell
iffy :: Maybe a -> MaybeT m a
```

It should behave like the `Maybe` monad in the sense that `iffy Nothing` does
no further computation, regardless of what is sequenced after it.


```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

Composition means extending the base monad instance:

```haskell
instance Monad m => Monad (MaybeT m) where
  return = return . Just
  MaybeT x >>= f = MaybeT $ x >>= maybe (return Nothing) (runMaybeT . f)
```

In order to reuse functions expressed in terms of the base monad we also need

```haskell
-- Part of the MonadTrans typeclass
lift :: Monad m => m a -> MaybeT m a
```

# Algebraic effects example

```haskell
data MaybeE where Iffy :: Maybe a -> MaybeE a

handler (Iffy x) = maybe (return ()) print x
```

# Sem

Effectful computations in `polysemy` occur in the `Sem` monad

# Built-in effects

Many of your favorite effects are included in `polysemy`:

- `Reader`
- `Writer`
- `State`
- `Trace`
- `Error`
- `Input`
- `Output`

# Custom effects

- `Sem`
- `interpret`
- `run` and `runM`

# Implementation

- The implementation of `polysemy` generally follows the paper _Effect handlers in scope_
- The monad `Sem` ends up being a carefully constructed free monad

# Sem

# Union

# Weaving

# References

- https://reasonablypolymorphic.com - Sandy Maguire's blog
- Effect handlers in scope - Wu, Schrijvers, & Hinze
- Algebraic operations and generic effects - Plotkin & Power (heavy category theory)
- Algebraic effects for functional programming - Leijen
- https://gitlab.haskell.org/ghc/ghc/issues/16473 (issue with specializer change proposal)
