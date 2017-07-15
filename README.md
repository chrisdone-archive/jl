# jl [alpha]

JSON lambda-calculus language.

The JL language is a simple language with syntactic sugar for querying
and manipulating JSON structures.

Example:

``` haskell
$ cat github.json | jl 'map $ \o -> {sha:o.sha,ps:map _.sha o.parents}'
[{"sha":"7b81a836c31500e685d043729259affa8b670a87","ps":["c538237f4e4c381d35f1c15497c95f659fd55850","ca12bd9b5d15c0c4e5bd01d706ddbb3f4edefd36"]},{"sha":"c538237f4e4c381d35f1c15497c95f659fd55850","ps":["4a6241be0697bbe4ef420c43689c34af59e50330"]},{"sha":"4a6241be0697bbe4ef420c43689c34af59e50330","ps":["1900c7bcac76777782505c89a032c18a65fcc487"]},{"sha":"1900c7bcac76777782505c89a032c18a65fcc487","ps":["578d536233b62884764b3c5c6cd42077958d6a49"]},{"sha":"578d536233b62884764b3c5c6cd42077958d6a49","ps":["b0d6d283102a171c74db142b5b00bb6115287c7a"]}]
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

## Core functions

``` haskell
id :: Value -> Value
get :: Value -> Value -> Value
set :: Value -> Value -> Value -> Value
modify :: (Value -> Value) -> Value -> Value -> Value
map :: (Value -> Value) -> Value -> Value
filter :: (Value -> Value) -> Value -> Value
```

## Syntactic sugar

Short-hand for fields:

    o.f  is sugar for         get "f" o
    _.f  is sugar for  (\o -> get "f" o)
     .f  is sugar for             "f"

For arrays:

    _[0] is sugar for   (\o -> get 0 o)

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
