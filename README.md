# mimsa

very small language. pls be gentle.

installation...

```bash
git clone https://github.com/danieljharvey/mimsa
cd mimsa
stack install
```

copying example project...

```bash
git clone https://github.com/danieljharvey/mimsa-test
cd mimsa-test
mimsa
```

you should then see:

```bash
~~~ MIMSA ~~~
:help - this help screen
:info <expr> - get the type of <expr>
:bind <name> = <expr> - binds <expr> to <name> and saves it in the environment
:bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment
:list - show a list of current bindings in the environment
:tree <expr> - draw a dependency tree for <expr>
:versions <name> - list all versions of a binding
:watch <name> - put <name> into 'scratch.mimsa' and bind any changes
:tui - launch terminal user interface for exploring project
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

type declarations:

```haskell
:bindType type Either e a = Left e | Right a

:> \a -> if a then Right a else Left "Oh no"
\a -> (if a then (Right a) else (Left "Oh no")) :: (7 -> Either String 7)
```

case matching:

```haskell
:> :bind eitherMap = \f -> \either -> case either of Left \e -> Left e | Right \a -> Right f(a)
Bound eitherMap to \f -> (\either -> case either of Left (\e -> (Left e)) | Right (\a -> (Right f(a)))) :: ((11 -> 15) -> (2 -> Either 13 15))

:> eitherMap(\a -> "dog")(Left "what")
Left "poo" :: Either 14 String -- you may notice this is wrong, see issues

:> eitherMap(\a -> "dog")(Right "poo")
Right "dog" :: Either 14 String -- this is OK though
```

