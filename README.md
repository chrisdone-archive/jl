# jl

jl ("JSON lambda") is a tiny functional language for querying and
manipulating JSON.

Example:

``` haskell
$ jl 'map $ \o -> { sha: o.sha, ps: map _.sha o.parents }' x.json
[{"sha":"7b81a836c31500e685d043729259affa8b670a87","ps":["c538237f4e4c381d35f1c15497c...
```

## Core syntax

Literals:

    123, 4.5, -6, "hi", null, true, false

Lambdas:

    \x -> y

Function application

    get "f" o

Arithmetic:

    x * (4 + 3)

Objects:

    {foo: 123, bar: 34.3, "a:b": "hi"}

Arrays:

    [1, 4 * 5, id 5]

Short-hand for fields:

    o.f  is sugar for         get "f" o
    _.f  is sugar for  (\o -> get "f" o)

For arrays:

    _[0] is sugar for   (\o -> get 0 o)

Function composition:

    a | b | c is sugar for `\x -> c (b (a x)`

## Mini tutorial

You do everything with usual functional programming functions.

Returning the same thing, aka identity. That's normal in functional
programming:

``` haskell
jl 'id'
```

Taking the first element of something, using syntax that looks like
regular array access. The `_` is a short-hand so that you don't need a
lambda:

``` haskell
jl '_[0]'
```

If you want to get what keys are available, you can run:

``` haskell
jl 'map keys | _[0]'
["sha","committer","url","comments_url","parents","author","html_url","commit"]
```

Taking the first element and then creating a record of some parts of it:

``` haskell
jl '_[0] | \o -> {msg: o.commit.message, n: o.commit.committer.name}'
```

Note the use of `|` to compose functions. Just like in the shell.

Applying a function to all elements in an array:

``` haskell
jl 'map _.commit.committer.name'
```

Note how you can nest property access easily.

Applying something more detailed, by constructing a record of our own

``` haskell
jl 'map $ \o -> {msg: o.commit.message, n: o.commit.committer.name}'
```

You can use `$` to avoid using parentheses on the right. That's a
trick from Haskell.

Applying functions to nested data structures:

``` haskell
jl '_[0] | \o -> {msg: o.commit.message, n: o.commit.committer.name, ps: map _.html_url o.parents }'
```

Notice the `ps` property comes by taking the `html_url` of all the parents.

Filtering is easy, simply write a function that returns true:

``` haskell
jl 'map (\o -> { sha: o.sha, ps: map _.sha o.parents }) | filter (\o -> length o.ps > 1)'
```

# Available functions

## Arithmetic operators

```haskell
* :: Value → Value → Value
```

a * b

```haskell
+ :: Value → Value → Value
```

a + b

```haskell
- :: Value → Value → Value
```

a - b

```haskell
/ :: Value → Value → Value
```

a / b


## Predicate operators

```haskell
/= :: Value → Value → Value
```

a /= b

```haskell
= :: Value → Value → Value
```

a = b


## Numeric predicate operators

```haskell
> :: Value → Value → Value
```

a > b

```haskell
< :: Value → Value → Value
```

a < b

```haskell
>= :: Value → Value → Value
```

a >= b

```haskell
<= :: Value → Value → Value
```

a <= b


## Function combinators

```haskell
id :: Value → Value
```

Identity function, returns its input unchanged

```haskell
compose :: (Value → Value) → (Value → Value) → Value → Value
```

Compose two functions

```haskell
flip :: (Value → Value → Value) → Value → Value → Value
```

Flips the argument order of a function of two or more arguments


## Record access

```haskell
get :: Value → Value → Value
```

Get the value at k from the object

```haskell
set :: Value → Value → Value → Value
```

Set the value k to v in object

```haskell
modify :: Value → (Value → Value) → Value → Value
```

Modify the object at k with function f


## Sequences

```haskell
map :: (Value → Value) → Value → Value
```

Apply a function to every element in the sequence

```haskell
filter :: (Value → Value) → Value → Value
```

Keep only items from the sequence for which p returns true

```haskell
takeWhile :: (Value → Value) → Value → Value
```

Take elements from a sequence while given predicate is true

```haskell
empty :: Value → Value
```

Is a sequence empty?

```haskell
length :: Value → Value
```

Get the length of a sequence

```haskell
reverse :: Value → Value
```

Reverse a sequence

```haskell
drop :: Value → Value → Value
```

Drop n items from the sequence

```haskell
elem :: Value → Value → Value
```

Is x an element of y?

```haskell
concat :: Value → Value
```

Concatenate a list of sequences into one sequence

```haskell
zipWith :: (Value → Value → Value) → Value → Value → Value
```

Zip two lists calling with each element to f x y

```haskell
take :: Value → Value → Value
```

Take n items from sequence

```haskell
fold :: (Value → Value → Value) → Value → Value → Value
```

Fold over a structure with a state.

```haskell
dropWhile :: (Value → Value) → Value → Value
```

Drop elements from a sequence while a predicate is true
