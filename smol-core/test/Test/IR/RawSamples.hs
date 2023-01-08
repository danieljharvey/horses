{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Test.IR.RawSamples
  ( print42,
    useId42,
    useAdd42,
    useConst42Curried,
    useBasicIf,
    oneTuple42,
    twoTuple42,
    nestedTuple42,
    either42,
  )
where

import IR.ToLLVM.Helpers
import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as Op
import LLVM.AST.Type as AST
import qualified LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import qualified LLVM.IRBuilder.Instruction as L
import LLVM.IRBuilder.Module
import qualified LLVM.IRBuilder.Monad as L

-- print the number 42
print42 :: Module
print42 = buildModule "exampleModule" $ mdo
  pInt <- getPrintInt

  function "main" [] AST.i32 $ \_ -> do
    _ <- call pInt [(C.int32 42, [])]
    ret (C.int32 0)

-- print the number 42 after using id function
useId42 :: Module
useId42 = buildModule "exampleModule" $ mdo
  pInt <- getPrintInt

  fnId <- function "id" [(AST.i32, "a")] AST.i32 $ \case
    [a] -> ret a
    other -> error (show other)

  function "main" [] AST.i32 $ \_ -> do
    a2 <- call fnId [(C.int32 42, [])]
    _ <- call pInt [(a2, [])]
    ret (C.int32 0)

-- print the number 42 after using the const function
-- this version passes the env but calls it immediately
useAdd42 :: Module
useAdd42 = buildModule "exampleModule" $ mdo
  pInt <- getPrintInt

  -- inputs for const2
  let const2Struct = AST.StructureType False [AST.i32]

  -- fn is (arg, env)
  -- this is the continuation of `const` below
  fnAdd2 <- function "add2" [(AST.i32, "b"), (pointerType const2Struct, "env")] AST.i32 $ \case
    [b, env] -> do
      -- %a = load i32, i32* %1
      a <- loadFromStruct env [0]
      -- add them
      res <- add a b
      -- return a
      ret res
    other -> error (show other)

  fnAdd <- function "add" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \case
    [a, b] -> do
      -- allocate room for a struct
      env2 <- allocLocal "const2-struct" const2Struct

      -- store a in slot1
      storePrimInStruct env2 [0] a

      -- run next function
      result <- call fnAdd2 [(b, []), (env2, [])]
      -- return result
      ret result
    other -> error (show other)

  function "main" [] AST.i32 $ \_ -> do
    a2 <-
      call
        fnAdd
        [ (C.int32 20, []),
          (C.int32 22, [])
        ]
    -- print output
    _ <- call pInt [(a2, [])]
    ret (C.int32 0)

-- make a one tuple, fetch from it, sum items
oneTuple42 :: Module
oneTuple42 = buildModule "exampleModule" $ do
  pInt <- getPrintInt

  let tyOneTuple = AST.StructureType False [AST.i32]

  mkTuple <- function "mkTuple" [(AST.i32, "a"), (pointerType tyOneTuple, "sret")] AST.void $ \case
    [a, sRet] -> do
      -- store a in slot 0
      storePrimInStruct sRet [0] a

      -- output nothing
      retVoid
    other -> error (show other)

  function "main" [] AST.i32 $ \_ -> do
    oneTuple <- callWithReturnStruct mkTuple tyOneTuple [C.int32 42]

    -- get 1st
    res <- loadFromStruct oneTuple [0]

    -- print output
    _ <- call pInt [(res, [])]
    ret (C.int32 0)

-- make a two tuple, fetch from it, sum items
twoTuple42 :: Module
twoTuple42 = buildModule "exampleModule" $ do
  pInt <- getPrintInt

  let tyTwoTuple = AST.StructureType False [AST.i32, AST.i32]

  mkTuple <- function
    "mkTuple"
    [ (AST.i32, "a"),
      (AST.i32, "b"),
      (pointerType tyTwoTuple, "sRet")
    ]
    AST.void
    $ \case
      [a, b, sRet] -> do
        llStruct <- allocLocal "mktuplestruct" tyTwoTuple
        -- store a in slot1
        storePrimInStruct llStruct [0] a

        -- store b in slot2
        storePrimInStruct llStruct [1] b

        moveToStruct llStruct sRet

        retVoid
      other -> error (show other)

  function "main" [] AST.i32 $ \_ -> do
    twoTuple <- callWithReturnStruct mkTuple tyTwoTuple [C.int32 20, C.int32 22]

    -- get 1st
    var1 <- loadFromStruct twoTuple [0]

    -- get 2nd
    var2 <- loadFromStruct twoTuple [1]

    -- sum the responses
    res <- add var1 var2

    -- print output
    _ <- call pInt [(res, [])]
    ret (C.int32 0)

-- make a nested tuple (10, (12, 20)) and adds it all
nestedTuple42 :: Module
nestedTuple42 = buildModule "exampleModule" $ do
  pInt <- getPrintInt

  let tyNested = struct [AST.i32, struct [AST.i32, AST.i32]]

  mkNestedTuple <- function
    "mkNestedTuple"
    [ (AST.i32, "a"),
      (AST.i32, "b"),
      (AST.i32, "c"),
      (pointerType tyNested, "sRet")
    ]
    AST.void
    $ \case
      [a, b, c, sRet] -> do
        -- store a in slot1
        storePrimInStruct sRet [0] a

        -- store b in slot2
        storePrimInStruct sRet [1, 0] b

        -- store c
        storePrimInStruct sRet [1, 1] c

        retVoid
      other -> error (show other)

  function "main" [] AST.i32 $ \_ -> do
    nestedTuple <-
      callWithReturnStruct
        mkNestedTuple
        tyNested
        [ C.int32 10,
          C.int32 12,
          C.int32 20
        ]

    -- get (a, (_,_))
    varA <- loadFromStruct nestedTuple [0]

    -- get (_, (b,_))
    varB <- loadFromStruct nestedTuple [1, 0]

    -- get (_, (_, c))
    varC <- loadFromStruct nestedTuple [1, 1]

    -- sum the responses
    res1 <- add varA varB
    res2 <- add res1 varC

    -- print output
    _ <- call pInt [(res2, [])]
    ret (C.int32 0)

-- if True then 1 else 2
useBasicIf :: Module
useBasicIf = buildModule "exampleModule" $ do
  pInt <- getPrintInt

  function "main" [] AST.i32 $ \_ -> mdo
    result <- alloca AST.i32 Nothing 0

    let predVal = C.bit 1 -- True
    L.condBr predVal thenBlock elseBlock

    thenBlock <- L.block `L.named` "then"
    -- set result to 1
    store result 0 (C.int32 1)
    L.br mergeBlock

    elseBlock <- L.block `L.named` "else"
    -- set result to 2
    store result 0 (C.int32 2)
    L.br mergeBlock

    mergeBlock <- L.block `L.named` "merge"
    -- do bothing

    finalResult <- load result 0

    _ <- call pInt [(finalResult, [])]
    ret (C.int32 0)

-- print the number 42 after using the const function
-- this version passes the env and returns the next function
useConst42Curried :: Module
useConst42Curried = buildModule "exampleModule" $ mdo
  pInt <- getPrintInt

  -- inputs for const2
  let const2Struct = AST.StructureType False [AST.i32]

  -- fn is (arg, env)
  -- this is the continuation of `const` below
  -- (int, [int]) -> int
  (_fnConst2, const2Func) <- functionAndType
    "const2"
    [ (AST.i32, "b"),
      (pointerType const2Struct, "env")
    ]
    AST.i32
    $ \case
      [_b, env] ->
        loadFromStruct env [0] >>= ret
      other -> error (show other)

  -- closure function type of const2 (fn*, env)
  let const2ClosureType =
        AST.StructureType
          False
          [ pointerType const2Func,
            const2Struct
          ]

  -- (int, (fn, [int])) -> void
  fnConst <- function
    "const"
    [ (AST.i32, "a"),
      (pointerType const2ClosureType, "sRet")
    ]
    AST.void
    $ \case
      [a, sRet] -> do
        -- store a in slot1 of env
        storePrimInStruct sRet [1, 0] a

        -- put fn in it
        storePrimInStruct
          sRet
          [0]
          ( Op.ConstantOperand
              (C.GlobalReference (pointerType const2Func) "const2")
          )

        -- return nothing
        retVoid
      other -> error (show other)

  function "main" [] AST.i32 $ \_ -> do
    closure <- callWithReturnStruct fnConst const2ClosureType [C.int32 42]

    -- call fn with env + arg
    a2 <- callClosure closure (C.int32 43)

    -- print output
    _ <- call pInt [(a2, [])]
    ret (C.int32 0)

-- https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/basic-constructs/unions.html?highlight=rust#tagged-unions
-- make a maybe, get the value out again
either42 :: Module
either42 = buildModule "exampleModule" $ do
  pInt <- getPrintInt

  -- this type is only here to stake out memory
  let tyEitherBoolInt = AST.StructureType False [AST.i32, AST.ArrayType 1 AST.i32]
      tyRightInt = pointerType (AST.StructureType False [AST.i32, AST.i32])
      tyLeftBool = pointerType (AST.StructureType False [AST.i32, AST.i1])

  function "main" [] AST.i32 $ \_ -> do
    -- CREATING RIGHT 41
    -- first we create an Either Bool Int and put Right Int in it
    rStruct <- allocLocal "mkrightstruct" tyEitherBoolInt

    -- store 0 (for "Right") in slot1
    storePrimInStruct rStruct [0] (C.int32 0)

    -- case to Right Int
    rStruct' <- bitcast rStruct tyRightInt

    -- store a in slot2
    storePrimInStruct rStruct' [1] (C.int32 41)

    -- turn it back into Right Int
    rStruct'' <- bitcast rStruct' (pointerType tyEitherBoolInt)

    -- CREATING LEFT 1
    -- first we create an Either Bool Int and put Right Int in it
    lStruct <- allocLocal "mkleftStruct" tyEitherBoolInt

    -- store 1 (for "Left") in slot1
    storePrimInStruct lStruct [0] (C.int32 1)

    -- cast to Left Bool
    lStruct' <- bitcast lStruct tyLeftBool

    -- store a in slot2
    storePrimInStruct lStruct' [1] (C.bit 1)

    -- turn it back into Right Int
    lStruct'' <- bitcast lStruct' (pointerType tyEitherBoolInt)

    -- (Either Bool Int -> Int)
    fnDeconstruct <- function
      "const"
      [ (pointerType tyEitherBoolInt, "either")
      ]
      AST.i32
      $ \case
        [input] -> mdo
          -- now we pattern match
          -- we're going to return this later
          result <- alloca AST.i32 Nothing 0

          -- get the constructor
          discriminator <- loadFromStruct input [0]

          L.switch discriminator rightBlock [(C.Int 32 0, rightBlock), (C.Int 32 1, leftBlock)]

          rightBlock <- L.block `L.named` "right"
          -- it's a Right Int
          casted <- bitcast input tyRightInt
          -- get the int out
          myInt <- loadFromStruct casted [1]
          -- set result to 1
          store result 0 myInt
          -- and merge!
          L.br mergeBlock

          leftBlock <- L.block `L.named` "left"
          lCasted <- bitcast input tyLeftBool
          myBool <- loadFromStruct lCasted [1]
          intFromBool <- zext myBool AST.i32
          store result 0 intFromBool
          L.br mergeBlock

          mergeBlock <- L.block `L.named` "merge"
          -- do bothing
          realResult <- load result 0
          ret realResult
        other -> error (show other)

    rStructPointer <- gep rStruct'' [C.int32 0]

    rightResult <- call fnDeconstruct [(rStructPointer, mempty)]

    lStructPointer <- gep lStruct'' [C.int32 0]

    leftResult <- call fnDeconstruct [(lStructPointer, mempty)]

    finalResult <- add leftResult rightResult
    _ <- call pInt [(finalResult, [])]
    ret (C.int32 0)
