# jl [![Build Status](https://travis-ci.org/chrisdone/jl.svg)](https://travis-ci.org/chrisdone/jl) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/chrisdone/path?svg=true)](https://ci.appveyor.com/project/chrisdone/jl)

jl ("JSON lambda") is a tiny functional language for querying and
manipulating JSON.

Example:

``` haskell
$ jl 'map $ \o -> { sha: o.sha, ps: map _.sha o.parents }' x.json
[{"sha":"7b81a836c31500e685d043729259affa8b670a87","ps":["c538237f4e4c381d35f1c15497c...
```

## Installing

Binary releases for Linux and OS X are available [here](https://github.com/chrisdone/jl/releases).

Builds on Windows (see AppVeyor status), haven't added Windows
binaries to the releases yet.

Installing from source:

1. Get [stack](https://haskell-lang.org/get-started)
2. Run `stack install` in the repository directory.
3. Add `~/.local/bin/` to your `PATH`.

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

Conditionals:

    if x then y else z

Short-hand for fields:

    o.f  is sugar for         get "f" o
    _.f  is sugar for  (\o -> get "f" o)

For arrays:

    _[0] is sugar for   (\o -> get 0 o)

Or objects:

    _[k]     is sugar for   (\o -> get k o)
    _["foo"] is sugar for   (\o -> get "foo" o)

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

## Record access

```haskell
get :: JSON → JSON → JSON
```

Get the value at k from the object

```haskell
set :: JSON → JSON → JSON → JSON
```

Set the value k to v in object

```haskell
modify :: JSON → (JSON → JSON) → JSON → JSON
```

Modify the object at k with function f

```haskell
keys :: JSON → JSON
```

Get all keys of the object

```haskell
elems :: JSON → JSON
```

Get all elements of the object


## Sequences

```haskell
map :: (JSON → JSON) → JSON → JSON
```

Apply a function to every element in the sequence

```haskell
filter :: (JSON → JSON) → JSON → JSON
```

Keep only items from the sequence for which p returns true

```haskell
takeWhile :: (JSON → JSON) → JSON → JSON
```

Take elements from a sequence while given predicate is true

```haskell
empty :: JSON → JSON
```

Is a sequence empty?

```haskell
length :: JSON → JSON
```

Get the length of a sequence

```haskell
reverse :: JSON → JSON
```

Reverse a sequence

```haskell
drop :: JSON → JSON → JSON
```

Drop n items from the sequence

```haskell
elem :: JSON → JSON → JSON
```

Is x an element of y?

```haskell
concat :: JSON → JSON
```

Concatenate a list of sequences into one sequence

```haskell
zipWith :: (JSON → JSON → JSON) → JSON → JSON → JSON
```

Zip two lists calling with each element to f x y

```haskell
take :: JSON → JSON → JSON
```

Take n items from sequence

```haskell
fold :: (JSON → JSON → JSON) → JSON → JSON → JSON
```

Fold over a structure with a state.

```haskell
dropWhile :: (JSON → JSON) → JSON → JSON
```

Drop elements from a sequence while a predicate is true

```haskell
any :: (JSON → JSON) → JSON → JSON
```

Does p return true for any of the elements?

```haskell
all :: (JSON → JSON) → JSON → JSON
```

Does p return true for all of the elements?

```haskell
nub :: JSON → JSON
```

Return the sequence with no duplicates; the nub of it

```haskell
sort :: JSON → JSON
```

Return the sequence sorted

```haskell
append :: JSON → JSON
```

Append a sequence

```haskell
sum :: JSON → JSON
```

Get the sum of a sequence

```haskell
minimum :: JSON → JSON
```

Get the minimum of a sequence

```haskell
maximum :: JSON → JSON
```

Get the maximum of a sequence


## Strings

```haskell
words :: JSON → JSON
```

Split the string into a list of words

```haskell
unwords :: JSON → JSON
```

Join the list of strings into a string separated by spaces

```haskell
lines :: JSON → JSON
```

Split the string into a list of lines

```haskell
unlines :: JSON → JSON
```

Join the list of strings into a string separated by lines and terminated by a new line


## Predicate operators

```haskell
/= :: JSON → JSON → JSON
```

a /= b

```haskell
= :: JSON → JSON → JSON
```

a = b


## Boolean operators

```haskell
&& :: JSON → JSON → JSON
```

a && b

```haskell
|| :: JSON → JSON → JSON
```

a || b

```haskell
not :: JSON → JSON
```

not b


## Numeric operators

```haskell
> :: JSON → JSON → JSON
```

a > b

```haskell
< :: JSON → JSON → JSON
```

a < b

```haskell
>= :: JSON → JSON → JSON
```

a >= b

```haskell
<= :: JSON → JSON → JSON
```

a <= b

```haskell
* :: JSON → JSON → JSON
```

a * b

```haskell
+ :: JSON → JSON → JSON
```

a + b

```haskell
- :: JSON → JSON → JSON
```

a - b

```haskell
/ :: JSON → JSON → JSON
```

a / b

```haskell
min :: JSON → JSON → JSON
```

a min b

```haskell
max :: JSON → JSON → JSON
```

a max b

```haskell
abs :: JSON → JSON
```

abs b


## Function combinators

```haskell
id :: JSON → JSON
```

Identity function, returns its input unchanged

```haskell
compose :: (JSON → JSON) → (JSON → JSON) → JSON → JSON
```

Compose two functions

```haskell
flip :: (JSON → JSON → JSON) → JSON → JSON → JSON
```

Flips the argument order of a function of two or more arguments
