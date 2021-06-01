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
# start repl
mimsa repl  
```

you should then see:

```bash
~~~ MIMSA ~~~
:help - this help screen
:info <expr> - get the type of <expr>
:bind <name> = <expr> - binds <expr> to <name> and saves it in the environment
:bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment
:list - show a list of current bindings in the environment
:outputJS <expr> - show JS code for <expr>
:tree <expr> - draw a dependency tree for <expr>
:search <mt> - search for exprs that match type
:versions <name> - list all versions of a binding
:addTest "<test name>" <expr> - add a unit test
:listTests <optional name> - list tests for <name>
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

infix operators

```haskell
:> 1 == 1
True :: Boolean

:> 1 == 2
False :: Boolean

:> 1 + 1
2 :: Int

:> 100 - 1
99 :: Int

:> "dog" <> "log"
"doglog" :: String
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
Bound id to \x -> x :: A -> A

:> id(1)
1 :: Int

:> :bind const = \x -> \y -> x
Bound const to \x -> (\y -> x) :: A -> (B -> A)

:> const(2)("horse")
2 :: Int

:> :bind compose = \f -> \g -> \a -> f(g(a))
Bound compose to \f -> (\g -> (\a -> (f(g(a))))) :: (A -> B) -> ((C -> A) -> (C -> B))
```

pairs:

```haskell
:> (1, "horse")
(1, "horse") :: (Int, String)

:> :bind fst = \x -> let (a, b) = x in a
Bound fst to \x -> let (a, b) = x in a :: (A, B) -> A

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
\a -> (if a then (Right a) else (Left "Oh no")) :: (A -> Either String A)
```

pattern matching:

```haskell
\a ->
  match a with
      1 -> True
    | 2 -> False
    | 3 -> True
    | 4 -> False
    | _ -> False
:: Int -> Boolean

\maybe ->
  match maybe with
      (Just _) -> True
    | _ -> False
:: (Maybe a) -> Boolean

\maybe ->
  match maybe with
      (Just { age: b, name: a }) -> (a, b)
    | _ -> ("", 0)
:: (Maybe { age: Int, name: String  | a }) -> (String, Int)
```

case matching:

```haskell
:> :bind eitherMap = \f -> \either -> case either of Left \e -> Left e | Right \a -> Right f(a)
Bound eitherMap to \f -> (\either -> case either of Left (\e -> (Left e)) | Right (\a -> (Right f(a)))) :: ((A -> B) -> (Either C A -> Either C B))

:> eitherMap(\a -> "dog")(Left "what")
Left "poo" :: Either String String

:> eitherMap(\a -> "dog")(Right "poo")
Right "dog" :: Either A String
```

string matching:

```haskell
:> :bind stringHead = \str -> case str of StrHead \c -> \rest -> Just c |
StrEmpty None

:> stringHead "dog"
Some "d" :: Option String

:> stringHead ""
None :: Option String
```

typed holes:

```haskell
:> let map = \f -> \a -> f(a) in map(?dunno)(1)

repl:1:35:
  |
1 | let map = \f -> \a -> f(a) in map(?dunno)(1)
  |                                   ^^^^^^
Typed holes found:
?dunno : (Int -> A)
```

infix operators:

```haskell
:> :bind concatWithSpace = \a -> \b -> a <> " " <> b
Bound concatWithSpace.

:> infix <+> = concatWithSpace; "dog" <+> "cat"
"dog cat" :: String

:> infix <|> = id; "dog" <|> "cat"
repl:1:1:
  |
1 | infix <|> = id; "dog" <|> "cat"
  | ^
Function arity mismatch. Expected 2 but got f -> f
```
