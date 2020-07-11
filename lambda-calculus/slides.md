% Lambda Calculus
% Ian Shipman, Bitnomial
% July 11, 2020

# Welcome

Slides and code are available at 

[https://github.com/GambolingPangolin/talks/lambda-calculus](https://github.com/GambolingPangolin/talks/lambda-calculus)

# Lambda calculus

The lambda calculus (LC) is a model of computation introduced by Alonzo Church.  Lambda calculus programs can compute anything that any other computer can compute.

. . .

Expressions:

- variables `x, y, z, ...`
- abstraction `x => E`, where `E` is any LC expression
- application `A B`, where `A, B` are any LC expressions

. . .

We shall use some shorthands: 

- `(x, y, z) => E` means `x => (y => (z => E))`
- `(f x y z)` means `(((f x) y) z)`

# Examples

::: incremental

- identity function `x => x`
- `(x, y) => x`
- Y-combinator `f => ((x => f (x x)) (x => f (x x)))`
- `person x y` (`person` is just a variable, but might have some extra meaning in context)

::: 

# Structure of LC expressions

The definition of LC expressions implies a classification as well.  

. . .

Every LC expression is one of:

- a variable 
- an abstraction 
- an application

This is useful because analyzing LC expressions can proceed by a case analysis.  

. . .

Subexpressions of LC expressions have the natural meaning:

::: incremental
- `E` in `x => E`
- `A` and `B` in `A B`
:::

# Variable types

There are two kinds of variables in a LC expression.

- Bound variables which are named in an abstraction
- Free variables (not bound)

In the expression `(x, y) => z x` the variables `x` and `y` are bound and `z` is free.  It does not matter than `y` does not appear in the body of the funciton.

# Reduction

Computing with the lambda calculus _is_ reducing expressions.

. . .

A _reducible expression (redex)_ is a LC expression of the form `f x` where `f` is an abstraction. 

. . .

Beta reduction: `(x => E) z ---> E[x/z]` which means every free occurrance of `x` in `E` with `z`

::: incremental 
* `(f => x => f x) (y => y)`
* `(x => f x)[f / (y => y)]` equals `x => (y => y) x`
* This reduces again to `x => y[y/x]`  which equals `x => x`
:::

# Careful!

There is one complication with beta reduction: _accidental capture._

. . .

Consider `(x => (y => x)) (y z)`.  

::: incremental

* Naive beta reduction: `y => (y z)`
* Correct beta reduction: `y_1 => (y z)`.

:::

. . .

Renaming bound variables does not change the sense of the expression.  Before doing beta reduction, rename all bound variables in the function to be distinct from the variables in the argument.

# Booleans

Here is a standard encoding of booleans in lambda calculus.

::: incremental

- True: `(x, y) => x`
- False: `(x, y) => y`
- Not: `b => (x, y) => b y x`
- And: `(b1, b2) => (x, y) => b1 (b2 x y) y`
- Or: `(b1, b2) => (x, y) => b1 x (b2 x y)`
- If-then-else: `(b1, x, y) => b1 x y`

::: 

# Linked lists

It is also possible to have data structures in LC!

```haskell
data List a = Nil | Cons a (List a)
```

. . .

Idea: represent a linked list as a function which takes a callback and gives it 

1. whether the list is nil 
2. the head (first element) of the list, and 
3. the tail (remainder) of the list.

# Linked list: definition

Here is the implementation.  We use `True` and `False` for the LC expressions defined above and a special variable `undefined` which can act as a placeholder.

. . .

::: incremental

- Nil: `(f => (f True undefined undefined))`
- Cons: `(h, t) => (f => (f False h t))`

:::

# Linked lists: API

Now we can define some helper functions.

::: incremental

- `is_null = (list => (list ((z, h, t) => z)))`
- `head = (list => list ((z, h, t) => h))`
- `tail = (list => list ((z, h, t) => t))`

:::

. . .

Exercise:  Check that `head (cons True nil) == True` (after applying any number of reductions to the sides)

# Special properties of LC

::: incremental

- Stateless.  All state is explicitly passed around.
- Pure. LC expressions are self contained and evaluation has no side effects.
- Declarative.  Programs are formulae rather than instructions.
- Parallel.  We can reduce non-overlapping redexes at the same time.

::: 

# Functional programming languages

Any language which has higher order functions includes an embedding of LC.

. . . 

FP languages emphasize this embedding and provide constructs which make composing LC programs convenient.  

. . . 

FP language compilers use specialized techniques to make running LC programs performant.

. . . 

The goal is to capture the benefits on the previous slide.

# Numbers

There is a way to encode non-negative numbers called the Church encoding.

::: incremental

- Zero: `(f, x) => x`
- One: `(f, x) => f x`
- n: `(f, x) => f (f (... (f x)))` (`n` total applications)
- Next: `n => (f, x) => n f (f x)`
- Sum: `(n, m) => (f, x) => (n f) (m f x)`
- Product: `(n, m) => (f, x) => n (m f) x`

::: 

# Exercises

We can also encode numbers using lists of booleans, by interpreting a list as the bits of a number with the lowest bit at the head of the list.  We shall call this representation binary numbers.

Implement:

1. Addition for lists of booleans.
2. Right and left shift.
3. A length function for arbitrary lists that outputs a binary number.
4. A function to transform a church number to a binary number.
5. A function to transform a binary number to a church number.

