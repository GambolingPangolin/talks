% Polysemy
% Ian Shipman, Bitnomial
% October 16, 2019

# Slides and code

> https://github.com/GambolingPangolin/talks/polysemy

# Pure and impure functions

An important distinction in functional programming:

::: incremental

- Pure functions: result depends deterministically on inputs and nothing else
- Impure functions: depend on and produce values not visible syntactically

:::

. . .

Monads are the standard way to encapsulate side effects.

# Composing monads

::: incremental

- Monad transformers: extend a base monad with new side effects
- Algebraic effects: carefully track effects and pass off to explicit handlers

:::

# Monad transformer example: MaybeT

`MaybeT` adds a simple kind of failure to a monad.  In terms of effects, we want

```haskell
iffy :: Maybe a -> MaybeT m a
```
. . .

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

# Algebraic effects

Effect systems work with a higher order type that includes a representation of a set of effects

```haskell
data Effect e a = _ -- details later
```
. . .

Effectful terms introduce specific effects

```haskell
iffy :: HasEffect e MaybeE => Maybe a -> Effect e a
```

Handlers remove effects

```haskell
handleMaybeE :: Effect (MaybeE ∪ e) a -> Effect e a
```

# Abstraction

::: incremental

- `mtl` provides typeclasses like `MonadReader` that abstract over the monad
- To handle some set of effects, a concrete monad transformer stack must implement the associated typeclasses
- This requires `m * n` new instances if `m` custom monad transformers enclose `n` `mtl` effects
- Algebraic effects run in a fixed monad, but the effects in scope vary

:::

# Free functors

Warm up for free constructions

Higher kinded type `f :: * -> *` => free functor

```haskell
data Coyoneda f a = CoYo (f x) (x -> a)

instance Functor (Coyoneda f) where
  fmap f (CoYo x f') = CoYo x (f . f')
```

Note that `Coyoneda f` satisfies the functor laws `fmap g . fmap h = fmap $ g .
h` and `fmap id  = id` by construction

# Free monads I

Recall the definition of a free monad

```haskell
data Free f a = R a | F (f (Free f a))

-- Free f a ~ a | f a | f (f a) | ...

instance Functor f => Monad (Free f) where
  return = R
  R x >>= f = f x
  F x >>= f = F $ (>>= f) <$> x -- substitution in place
```
. . .

This monad has the property that transformations `Free f -> m` that respect the
monad structure are naturally identified with ordinary natural transformations
`f -> m`

# Free monads II

Free monads capture syntax trees

```haskell
data Counter a = Inc a | Dec a
type Expr = Free Counter Int

incM x = F $ Inc (R x) :: Int -> Expr
decM x = F $ Dec (R x) :: Int -> Expr

interpret :: Monad a => (∀ x . f x -> m x) -> Free f a -> m a
interpret _  (R x) = return x
interpret nt (F x) = nt x >>= interpret nt

count :: Counter x -> State Int x
count = \case
  Inc x -> modify (+x)
  Dec x -> modify (-x)
```

# Free monads III

There is another encoding of free monads that will come up later

. . .

By the freeness property `interpret` is an isomorphism, so we can also define

```haskell
data Free f a = Free { runFree :: ∀ m . Monad m => (∀ x . f x -> m x) -> m a }

instance Functor f => Functor (Free f) where
  fmap f (Free m) = Free $ \k -> f <$> m k

instance Functor f => Monad (Free f) where
  return x = Free $ \_ -> return x
  Free m >>= f = Free $ \k -> m k >>= \a -> runFree (f a) k
```

# Data types à la carte

In order to use the power of the free monad construction, we need a way to compose the base functor.   It is easy to combine functors:

```haskell
data Or f g a = InL (f a) | InR (g a)

type Syntax = Or Console (Or Input Random)
program = Free Syntax ()
```
. . .

Syntax is extended like so:

```haskell
weakenL :: Free f a -> Free (Or f g) a
weakenL (R x) = R x
weakenL (F x) = F . InL $ weaken <$> x

-- weakenR similar
```

Exercise: define a typeclass `Member f f'` that you can use to implement `weaken :: Member f f' => Free f a -> Free f' a` when `f'` includes `f` as a summand.

# Example project

- [game (mark 0)][0]
- [game (mark 1)][1]
- [game (mark 2)][2]

[0]: game-0/src/Main.html
[1]: game-1/src/Main.html
[2]: game-2/src/Main.html

# Higher order effects

Effect theory so far is first order.  Higher order effects may enclose effectful computations.

```haskell
throw :: Has (Error e) effs => e -> Effect effs r
catch :: Has (Error e) effs => Effect effs r -> (e -> Effect effs r) -> Effect effs r
```

::: incremental

- Higher order effects have different requirements.
- Interpretation of `catch` has to have the handler available in case there is a `throw` in the try-branch

:::

# Polysemy implementation

The implementation of `polysemy` generally follows the paper _Effect handlers in scope_

. . .

Effects encapsulate an interpretation monad as well as an enclosed value:

```haskell
-- kind of effects
data SomeEffect (m :: * -> *) a
```

At a very high level, the effects monad is a free monad over a free functor over a sum of effect types

# Weaving I

The `Weaving` type is the workhorse in `polysemy`

. . .

This is the free functor, with some additional values to support threading state in complex ways

```haskell
data Weaving e m a where
  Weaving
    :: Functor f
       -- bundle some state e.g. `Either err` or `(,) state`
    => { weaveEffect :: e m a
       -- ^ data constructon which introduced the effect
       , weaveState :: f ()
       -- ^ state when the effect was introduced, depending on other effects in scope
       , weaveDistrib :: forall x. f (m x) -> n (f x)
       -- ^ transform the effect stack, threading state
       , weaveResult :: f a -> b
       -- ^ interpret the result in the context of state
       , weaveInspect :: forall x. f x -> Maybe x
       -- ^ extract value from stateful context
       }
    -> Weaving e n b
```

# Weaving II

Introduce an effect

```haskell
-- Member e r is satisfied if e is in the type-level list r
inj :: forall e r m a. (Functor m , Member e r) => e m a -> Union r m a
inj e = injWeaving $
  Weaving e (Identity ())
            (fmap Identity . runIdentity)
            runIdentity
            (Just . runIdentity)
```

. . .

Change the functor where the effects run

```haskell
hoist
    :: (Functor m, Functor n)
    => (∀ x. m x -> n x)
    -> Union r m a -> Union r n a
hoist f' (Union w (Weaving e s nt f v)) = Union w $ Weaving e s (f' . nt) f v
```

# Union

The `Union` type is the instantiation of Data types à carte in this case

```haskell
data Union (r :: '[Effect]) (m :: Type -> Type) a where
  Union
      ::
         SNat n
      -- ^ location of the desired effect
      -> Weaving (IndexOf r n) m a
      -> Union r m a
```

# Sem I

The `Sem` type is the second form of free monads from before

```haskell
data Sem r a = Sem { runSem :: ∀ m . Monad m => (∀ x . Union r (Sem r) x -> m x) -> m a }
```

. . .

Put effects into the `Sem` monad by wrapping them in a trivial `Union` as above, then enclosing the `Union` in a `Sem`

```haskell
liftSem :: Union r (Sem r) a -> Sem r a
liftSem u = Sem $ \k -> k u

send :: Member e r => e (Sem r) a -> Sem r a
send = liftSem . inj
```

# Interpretation of effects

How do we tear down our free monad?

. . .

First order effects: _a natural transformation for each effect type!_

. . .

```haskell
interpret
    :: (∀ x m . e m x -> Sem r x)
    -> Sem (e ': r) a
    -> Sem r a
interpret f (Sem m) = m $ \u ->
  -- Match the effect type in the union with the active effect type
  case decomp u of
    -- deal with this effect (if applicable) deeper in the syntax tree
    Left x -> liftSem $ hoist (interpret f) x
    -- calculate the result
    Right (Weaving e _ _ y _) -> do
      a <- f e
      pure $ y a
```

# Thank you!

You've been excellent, audience.

# References

- https://reasonablypolymorphic.com - Sandy Maguire's blog
- Effect handlers in scope - Wu, Schrijvers, & Hinze
- Algebraic operations and generic effects - Plotkin & Power (heavy category theory)
- Algebraic effects for functional programming - Leijen
- https://gitlab.haskell.org/ghc/ghc/issues/16473 (issue with specializer change proposal)
