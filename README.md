# mimsa

very small language. pls be gentle.

```bash
stack install
stack exec mimsa
```

you should then see:

```bash
~~~ MIMSA ~~~
:help - this help screen
:info <expr> - get the type of <expr>
:bind <name> = <expr> - binds <expr> to <name> and saves it in the environment
:list - show a list of current bindings in the environment
:tree <expr> - draw a dependency tree for <expr>
<expr> - Evaluate <expr>, returning it's simplified form and type
:quit - give up and leave
```

syntax (incomplete):

literals:

```haskell
:> True
True :: Boolean

:> False
False :: Boolean

:> Unit
Unit :: Unit

:> 1
1 :: Int

:> 56
56 :: Int

:> 234234
234234 :: Int

:> "dog"
"dog" :: String

:> "horse"
"horse" :: String
```

if statements:

```haskell
:> if True then 1 else 2
1 :: Int

:> if False then 1 else 2
2 :: Int

-- returning type should always match on both sides
:> if False then 1 else "dog"
Unification error: Can't match MTInt with MTString
```

lambdas and function application:

```haskell
:> :bind id = \x -> x
Bound id to \x -> x :: U1 -> U1

:> id(1)
1 :: Int

:> :bind const = \x -> \y -> x
Bound const to \x -> (\y -> x) :: U1 -> (U2 -> U1)

:> const(2)("horse")
2 :: Int

:> :bind compose = \f -> \g -> \a -> f(g(a))
Bound compose to \f -> (\g -> (\a -> (f(g(a))))) :: (U5 -> U4) -> ((U3 -> U5) -> (U3 -> U4))
```

pairs:

```haskell
:> (1, "horse")
(1, "horse") :: (Int, String)

:> :bind fst = \x -> let (a, b) = x in a
Bound fst to \x -> let (a, b) = x in a :: (U2, U3) -> U2

:> fst((1,"horse"))
1 :: Int
```

sums:

sum types are either `left` or `right`.

```haskell
:> Left 1
Left 1 :: Sum Int U1

:> Right "dog"
Right "dog" :: Sum U1 String

:> \a -> if a then Left 1 else Right "dog"
\a -> (if a then Left 1 else Right "dog") :: Boolean -> (Sum Int String)

:> \sum -> case sum of Left (\l -> Left l) | Right (\r -> Right "horse")
\sum -> case sum of Left (\l -> Left l) | Right (\r -> Right "horse") :: (Sum U7 U3) -> (Sum U7 String)
```

lists:

lists are never empty.

```haskell
:> [1]
[1] :: List Int

:> [1,2,3,4]
[1, 2, 3, 4] :: List Int

:> []
Could not parse expression for >>>[]<<<

:> let [a,b] = ([1,2,3]) in a
1 :: Int

:> let [a,b] = ([1,2,3]) in b
Right [2, 3] :: Sum Unit (List Int)

:> let [a,b] = ([1]) in b
Unit :: Sum Unit (List Int)
```

records:

```haskell
:> { a: "Dog", b: 123, c: True }
{a: "Dog", b: 123, c: True} :: {a: String, b: Int, c: Boolean}

:> let record = ({ a: "Dog", b: 123, c: True }) in record.a
"Dog" :: String

:> let record = ({ a: "Dog", b: 123, c: True }) in record.b
123 :: Int

:> let record = ({ a: "Dog", b: 123, c: True }) in record.c
True :: Boolean

:> \i -> if i.predicate then 1 else 2
\i -> (if i.predicate then 1 else 2) :: {predicate: Boolean} -> Int
```
