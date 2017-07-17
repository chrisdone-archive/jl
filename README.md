# jl

JSON Lambda is a tiny functional language for querying and
manipulating JSON.

Example:

``` haskell
$ cat ... | jl 'map $ \o -> { sha: o.sha, ps: map _.sha o.parents }'
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
$ cat .. | jl 'map (\o -> { sha: o.sha, ps: map _.sha o.parents }) | filter (\o -> length o.ps > 1)'
```
