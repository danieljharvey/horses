# horses

some sort of compiler

```haskell
def inc (a: Int): Int { 
    a + 1
}

class Eq a { equals : a -> a -> Bool }

instance Eq Int {
    \a -> \b -> a == b
}

class Functor f { fmap : (a -> b) -> f a -> f b }

/* Maybe */

type Maybe a = Just a | Nothing

instance (Eq a) => Eq (Maybe a) {
    \maybeA -> \maybeB -> 
        case (maybeA, maybeB) {
          (Just a, Just b) -> equals a b,
          (Nothing, Nothing) -> True,
          _ -> False
        }
}

instance Functor Maybe {
    \f -> \maybe -> case maybe { Just a -> Just (f a), Nothing -> Nothing }
}

test "fmap works with Just" {
    let result = fmap inc (Just 1);
    let expected = Just (2 : Int);
    equals result expected
}
```
