# jl [alpha]

JSON lambda-calculus language.

The JL language is a simple language with syntactic sugar for querying
and manipulating JSON structures.

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

## Core functions

``` haskell
get :: String -> Value -> Value
set :: String -> Value -> Value -> Value
```

## Syntactic sugar

Short-hand for fields:

    o.f  is sugar for         get "f" o
    _.f  is sugar for  (\o -> get "f" o)
     .f  is sugar for             "f"

For arrays:

    _.0 is sugar for   (\o -> get "0" o)

Function composition:

    a | b | c is sugar for `\x -> c (b (a x)`

## jq comparison

``` haskell
jq '.'
jl 'id'
```

``` haskell
jq '.[0]'
jl '_[0]'
```

``` haskell
jq '[.[0] | {message: .commit.message, name: .commit.committer.name}]'
jl '_[0] | \o -> {message: o.commit.message, name: o.commit.committer.name}'
```

``` haskell
jq '[.[] | {message: .commit.message, name: .commit.committer.name}]'
jl 'map $ \o -> {message: o.commit.message, name: o.commit.committer.name}'
```

``` haskell
jq '[.[] | {message: .commit.message, name: .commit.committer.name, parents: [.parents[].html_url]}]'
jl '_[0] | \o -> {message: o.commit.message, name: o.commit.committer.name, parents: map _.html_url o.parents }'
```
