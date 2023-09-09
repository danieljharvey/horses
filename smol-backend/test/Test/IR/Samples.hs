{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.IR.Samples
  ( irId42,
    irPrint42,
    irTwoTuple42,
    irBasicIf,
    irPatternMatch,
    irRecursive,
    irCurriedNoClosure,
    irCurried,
    irBoxedAddition,
    irBoxedSum,
    irPolymorphicId,
    irPolymorphicFst,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Smol.Backend.IR.IRExpr
import Smol.Backend.IR.ToLLVM.Patterns

irPrintInt :: IRModulePart
irPrintInt =
  IRExternDef
    ( IRExtern
        { ireName = "printint",
          ireArgs = [IRInt32],
          ireReturn = IRInt32
        }
    )

tyPrintInt :: IRType
tyPrintInt = IRFunctionType [IRInt32] IRInt32

irPrintBoxedInt :: IRModulePart
irPrintBoxedInt =
  IRExternDef
    ( IRExtern
        { ireName = "print_boxed_int",
          ireArgs = [IRPointer (IRStruct [IRInt32])],
          ireReturn = IRInt32
        }
    )

tyPrintBoxedInt :: IRType
tyPrintBoxedInt = IRFunctionType [IRStruct [IRInt32]] IRInt32

-- this should print the number 42
irPrint42 :: IRModule
irPrint42 =
  IRModule
    [ irPrintInt,
      IRFunctionDef
        ( IRFunction
            { irfName = "main",
              irfArgs = [],
              irfReturn = IRInt32,
              irfBody =
                [ IRDiscard (IRApply tyPrintInt (IRFuncPointer "printint") [IRPrim $ IRPrimInt32 42]),
                  IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                ]
            }
        )
    ]

-- print the number 42 after using id function
irId42 :: IRModule
irId42 =
  IRModule
    [ irPrintInt,
      IRFunctionDef
        ( IRFunction
            { irfName = "id",
              irfArgs = [(IRInt32, "a")],
              irfReturn = IRInt32,
              irfBody =
                [ IRRet IRInt32 (IRVar "a")
                ]
            }
        ),
      IRFunctionDef
        ( IRFunction
            { irfName = "main",
              irfArgs = [],
              irfReturn = IRInt32,
              irfBody =
                [ IRDiscard (IRApply tyPrintInt (IRFuncPointer "printint") [IRApply (IRFunctionType [IRInt32] IRInt32) (IRFuncPointer "id") [IRPrim $ IRPrimInt32 42]]),
                  IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                ]
            }
        )
    ]

-- make a two tuple, fetch from it, sum items
-- let tuple a = (a,1); case (tuple 41) of [b,c] -> b + c
irTwoTuple42 :: IRModule
irTwoTuple42 =
  let tyStruct = IRStruct [IRInt32, IRInt32]
   in IRModule
        [ irPrintInt,
          IRFunctionDef
            ( IRFunction
                { irfName = "tuple",
                  irfArgs = [(IRInt32, "a")],
                  irfReturn = tyStruct,
                  irfBody =
                    [ IRRet
                        tyStruct
                        ( IRInitialiseDataType
                            (IRAlloc tyStruct)
                            Nothing
                            [ IRSetTo
                                { irstPath = [0],
                                  irstType = IRInt32,
                                  irstExpr = IRVar "a"
                                },
                              IRSetTo
                                { irstPath = [1],
                                  irstType = IRInt32,
                                  irstExpr = IRPrim $ IRPrimInt32 1
                                }
                            ]
                        )
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "main",
                  irfArgs = [],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRDiscard
                        ( IRLet
                            "result"
                            ( IRApply
                                (IRFunctionType [IRInt32] (IRStruct [IRInt32, IRInt32]))
                                (IRFuncPointer "tuple")
                                [IRPrim (IRPrimInt32 41)]
                            )
                            ( IRLet
                                "fst"
                                (IRStructPath [0] (IRVar "result"))
                                ( IRLet
                                    "snd"
                                    (IRStructPath [1] (IRVar "result"))
                                    ( IRApply
                                        tyPrintInt
                                        (IRFuncPointer "printint")
                                        [ IRInfix
                                            IRAdd
                                            (IRVar "fst")
                                            (IRVar "snd")
                                        ]
                                    )
                                )
                            )
                        ),
                      IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                    ]
                }
            )
        ]

-- if True then 42 else 21
irBasicIf :: IRModule
irBasicIf =
  IRModule
    [ irPrintInt,
      IRFunctionDef
        ( IRFunction
            { irfName = "main",
              irfArgs = [],
              irfReturn = IRInt32,
              irfBody =
                [ IRDiscard
                    ( IRApply
                        tyPrintInt
                        (IRFuncPointer "printint")
                        [ IRMatch
                            (IRPrim (IRPrimInt2 True))
                            IRInt32
                            ( NE.fromList
                                [ IRMatchCase
                                    { irmcType = IRInt2,
                                      irmcPatternPredicate = [PathEquals (GetPath [] GetValue) (IRPrim $ IRPrimInt2 True)],
                                      irmcGetPath = mempty,
                                      irmcExpr = IRPrim (IRPrimInt32 42)
                                    },
                                  IRMatchCase
                                    { irmcType = IRInt2,
                                      irmcPatternPredicate = [PathEquals (GetPath [] GetValue) (IRPrim $ IRPrimInt2 False)],
                                      irmcGetPath = mempty,
                                      irmcExpr = IRPrim (IRPrimInt32 21)
                                    }
                                ]
                            )
                        ]
                    ),
                  IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                ]
            }
        )
    ]

-- case (Right 42) of Right 100 -> 41 | Right a -> a | Left e -> 69
irPatternMatch :: IRModule
irPatternMatch =
  let tyEitherBoolInt = IRStruct [IRInt32, IRArray 1 IRInt32] -- whole type
      tyRightInt = IRStruct [IRInt32, IRInt32]
      tyLeftBool = IRStruct [IRInt32, IRInt2]
   in IRModule
        [ irPrintInt,
          IRFunctionDef
            ( IRFunction
                { irfName = "main",
                  irfArgs = [],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRDiscard $
                        IRLet
                          "either"
                          ( IRInitialiseDataType
                              (IRAlloc tyEitherBoolInt)
                              (Just (tyRightInt, tyEitherBoolInt))
                              [ IRSetTo
                                  { irstPath = [0],
                                    irstType = IRInt32,
                                    irstExpr = IRPrim $ IRPrimInt32 0
                                  },
                                IRSetTo
                                  { irstPath = [1],
                                    irstType = IRInt32,
                                    irstExpr = IRPrim $ IRPrimInt32 42
                                  } -- 0 for Right, `a` is 42
                              ]
                          )
                          ( IRApply
                              tyPrintInt
                              (IRFuncPointer "printint")
                              [ IRMatch
                                  (IRVar "either")
                                  IRInt32
                                  ( NE.fromList
                                      [ IRMatchCase
                                          { irmcType = tyRightInt,
                                            irmcPatternPredicate =
                                              [ PathEquals (GetPath [0] GetValue) (IRPrim $ IRPrimInt32 0),
                                                PathEquals (GetPath [1] GetValue) (IRPrim $ IRPrimInt32 100)
                                              ],
                                            irmcGetPath = mempty,
                                            irmcExpr = IRPrim (IRPrimInt32 41)
                                          },
                                        IRMatchCase
                                          { irmcType = tyRightInt,
                                            irmcPatternPredicate = [PathEquals (GetPath [0] GetValue) (IRPrim $ IRPrimInt32 0)],
                                            irmcGetPath = M.singleton "a" (GetPath [1] GetValue),
                                            irmcExpr = IRVar "a"
                                          },
                                        IRMatchCase
                                          { irmcType = tyLeftBool,
                                            irmcPatternPredicate = [PathEquals (GetPath [0] GetValue) (IRPrim $ IRPrimInt32 1)],
                                            irmcGetPath = M.singleton "e" (GetPath [1] GetValue),
                                            irmcExpr = IRPrim (IRPrimInt32 69)
                                          }
                                      ]
                                  )
                              ]
                          ),
                      IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                    ]
                }
            )
        ]

-- a recursive function
-- let sum a = if a == 10 then 0 else a + sum (a +1 ); a 0;
irRecursive :: IRModule
irRecursive =
  IRModule
    [ irPrintInt,
      IRFunctionDef
        ( IRFunction
            { irfName = "sum",
              irfArgs = [(IRInt32, "a")],
              irfReturn = IRInt32,
              irfBody =
                [ IRRet
                    IRInt32
                    ( IRMatch
                        (IRVar "a")
                        IRInt32
                        ( NE.fromList
                            [ IRMatchCase
                                { irmcType = IRInt32,
                                  irmcPatternPredicate =
                                    [ PathEquals (GetPath [] GetValue) (IRPrim $ IRPrimInt32 10000)
                                    ],
                                  irmcGetPath = mempty,
                                  irmcExpr = IRPrim $ IRPrimInt32 0
                                },
                              IRMatchCase
                                { irmcType = IRInt32,
                                  irmcPatternPredicate = mempty,
                                  irmcGetPath = mempty,
                                  irmcExpr =
                                    IRInfix
                                      IRAdd
                                      (IRVar "a")
                                      ( IRApply
                                          (IRFunctionType [IRInt32] IRInt32)
                                          (IRFuncPointer "sum")
                                          [ IRInfix
                                              IRAdd
                                              (IRVar "a")
                                              (IRPrim $ IRPrimInt32 1)
                                          ]
                                      )
                                }
                            ]
                        )
                    )
                ]
            }
        ),
      IRFunctionDef
        ( IRFunction
            { irfName = "main",
              irfArgs = [],
              irfReturn = IRInt32,
              irfBody =
                [ IRDiscard
                    ( IRApply
                        tyPrintInt
                        (IRFuncPointer "printint")
                        [ IRApply
                            (IRFunctionType [IRInt32] IRInt32)
                            (IRFuncPointer "sum")
                            [IRPrim (IRPrimInt32 0)]
                        ]
                    ),
                  IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                ]
            }
        )
    ]

-- a flipped const function
-- let flipConst =\a -> \b -> b; flipConst 20 22
irCurriedNoClosure :: IRModule
irCurriedNoClosure =
  let func2Type = IRFunctionType [IRInt32] IRInt32
      add1ReturnType =
        IRStruct
          [ IRPointer func2Type
          ]
   in IRModule
        [ irPrintInt,
          IRFunctionDef
            ( IRFunction
                { irfName = "add2",
                  irfArgs = [(IRInt32, "b")],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRRet
                        IRInt32
                        (IRVar "b")
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "add1",
                  irfArgs = [(IRInt32, "a")],
                  irfReturn = add1ReturnType,
                  irfBody =
                    [ IRRet
                        add1ReturnType
                        ( IRLet
                            "struct"
                            (IRAlloc add1ReturnType)
                            ( IRStatements
                                [ IRSet
                                    [0]
                                    func2Type
                                    (IRFuncPointer "add2")
                                    (IRVar "struct") -- function pointer
                                ]
                                (IRVar "struct")
                            )
                        )
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "main",
                  irfArgs = [],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRDiscard
                        ( IRApply
                            tyPrintInt
                            (IRFuncPointer "printint")
                            [ IRLet
                                "closure"
                                (IRApply (IRFunctionType [IRInt32] add1ReturnType) (IRFuncPointer "add1") [IRPrim (IRPrimInt32 20)])
                                ( IRApply
                                    (IRFunctionType [IRInt32] IRInt32)
                                    (IRStructPath [0] (IRVar "closure"))
                                    [ IRPrim (IRPrimInt32 22)
                                    ]
                                )
                            ]
                        ),
                      IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                    ]
                }
            )
        ]

-- a curried add function
-- let add2 =\a -> \b -> a + b; add 20 22
irCurried :: IRModule
irCurried =
  let func2Env = IRStruct [IRInt32]
      func2Type = IRFunctionType [IRInt32, func2Env] IRInt32
      add1ReturnType =
        IRStruct
          [ IRPointer func2Type,
            func2Env
          ]
   in IRModule
        [ irPrintInt,
          IRFunctionDef
            ( IRFunction
                { irfName = "add2",
                  irfArgs = [(IRInt32, "b"), (func2Env, "env")],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRRet
                        IRInt32
                        (IRInfix IRAdd (IRVar "b") (IRStructPath [0] (IRVar "env")))
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "add1",
                  irfArgs = [(IRInt32, "a"), (IRStruct [], "env")],
                  irfReturn = add1ReturnType,
                  irfBody =
                    [ IRRet
                        add1ReturnType
                        ( IRInitialiseDataType
                            (IRAlloc add1ReturnType)
                            Nothing
                            [ IRSetTo
                                [0]
                                func2Type
                                (IRFuncPointer "add2"),
                              IRSetTo [1, 0] IRInt32 (IRVar "a")
                            ]
                        )
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "main",
                  irfArgs = [],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRDiscard
                        ( IRApply
                            tyPrintInt
                            (IRFuncPointer "printint")
                            [ IRLet
                                "emptyenv"
                                (IRAlloc (IRStruct []))
                                ( IRLet
                                    "closure"
                                    ( IRApply
                                        (IRFunctionType [IRInt32] add1ReturnType)
                                        (IRFuncPointer "add1")
                                        [IRPrim (IRPrimInt32 20), IRVar "emptyenv"]
                                    )
                                    ( IRApply
                                        func2Type
                                        (IRStructPath [0] (IRVar "closure"))
                                        [ IRPrim (IRPrimInt32 22),
                                          IRPointerTo [1] (IRVar "closure")
                                        ]
                                    )
                                )
                            ]
                        ),
                      IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                    ]
                }
            )
        ]

-- | place a single value in a single item tuple
irBox :: IRType -> IRExpr -> IRExpr
irBox irType irExpr =
  let tyBoxed = IRStruct [irType]
   in IRInitialiseDataType
        (IRAlloc tyBoxed)
        Nothing
        [ IRSetTo
            { irstPath = [0],
              irstType = irType,
              irstExpr = irExpr
            }
        ]

irUnbox :: IRExpr -> IRExpr
irUnbox = IRStructPath [0]

-- testing making the IR for boxing
-- 20 + 22
irBoxedAddition :: IRModule
irBoxedAddition =
  IRModule
    [ irPrintBoxedInt,
      IRFunctionDef
        ( IRFunction
            { irfName = "main",
              irfArgs = [],
              irfReturn = IRInt32,
              irfBody =
                [ IRDiscard
                    ( IRApply
                        tyPrintBoxedInt
                        (IRFuncPointer "print_boxed_int")
                        [ IRLet
                            "int_box_a"
                            (irBox IRInt32 (IRPrim $ IRPrimInt32 20))
                            ( IRLet
                                "int_box_b"
                                (irBox IRInt32 (IRPrim $ IRPrimInt32 22))
                                (irBox IRInt32 (IRInfix IRAdd (irUnbox (IRVar "int_box_a")) (irUnbox (IRVar "int_box_b"))))
                            )
                        ]
                    ),
                  IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                ]
            }
        )
    ]

-- testing making the IR for boxing
-- let sum a b = a + b; sum 20 22
-- (we're not bothering with currying sum here, so it won't _strictly_ match
-- the expr)
irBoxedSum :: IRModule
irBoxedSum =
  let sumReturnType = IRStruct [IRInt32]
      sumFunctionType = IRFunctionType [IRStruct [IRInt32], IRStruct [IRInt32]] sumReturnType
   in IRModule
        [ irPrintBoxedInt,
          IRFunctionDef
            ( IRFunction
                { irfName = "sum",
                  irfArgs = [(IRStruct [IRInt32], "a"), (IRStruct [IRInt32], "b")],
                  irfReturn = sumReturnType,
                  irfBody =
                    [ IRRet
                        sumReturnType
                        (irBox IRInt32 (IRInfix IRAdd (irUnbox (IRVar "a")) (irUnbox (IRVar "b"))))
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "main",
                  irfArgs = [],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRDiscard
                        ( IRApply
                            tyPrintBoxedInt
                            (IRFuncPointer "print_boxed_int")
                            [ IRLet
                                "int_box_a"
                                (irBox IRInt32 (IRPrim $ IRPrimInt32 20))
                                ( IRLet
                                    "int_box_b"
                                    (irBox IRInt32 (IRPrim $ IRPrimInt32 22))
                                    (IRApply sumFunctionType (IRFuncPointer "sum") [IRVar "int_box_a", IRVar "int_box_b"])
                                )
                            ]
                        ),
                      IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                    ]
                }
            )
        ]


-- we want a polymorphic `id` function
-- let id a = a; id 42
irPolymorphicId :: IRModule
irPolymorphicId =
  let tyAny = IRPointer IRInt32 -- pointer to anything, our polymorphic type
      tyBoxedInt = IRStruct [IRInt32]
      idFunctionType = IRFunctionType [tyAny] tyAny
      applyId a = IRApply idFunctionType (IRFuncPointer "id") [IRCast tyAny a]

   in IRModule
        [ irPrintBoxedInt,
          IRFunctionDef
            ( IRFunction
                { irfName = "id",
                  irfArgs = [(tyAny, "a")],
                  irfReturn = tyAny,
                  irfBody =
                    [ IRRet tyAny (IRVar "a")
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "main",
                  irfArgs = [],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRDiscard
                        ( IRApply
                            tyPrintBoxedInt
                            (IRFuncPointer "print_boxed_int")
                            [ IRCast tyBoxedInt
                                (applyId (irBox IRInt32
                                    $ IRPrim $ IRPrimInt32 42))
                            ]
                        ),
                      IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                    ]
                }
            )
        ]

-- we want a polymorphic `fst` function
-- let fst p = case p of (a,b) -> (b,a); fst (20,200) + fst (22, True)
irPolymorphicFst :: IRModule
irPolymorphicFst =
  let tyAny = IRPointer  IRInt32  -- pointer to anything, our polymorphic type
      tyAnyPair = IRStruct [tyAny, tyAny]
      tyBoxedInt = IRStruct [IRInt32]
      tyBoxedBool = IRStruct [IRInt2]
      fstFunctionType = IRFunctionType [tyAnyPair] tyAny
      applyFst a = IRApply fstFunctionType (IRFuncPointer "fst") [IRCast tyAnyPair a]
      -- | pair talks about the original types
      pair (tyA,a) (tyB,b) =
           IRInitialiseDataType
              (IRAlloc $ IRStruct [tyA, tyB])
              Nothing
              [ IRSetTo [0] tyA a,
                IRSetTo [1] tyB b
              ]

   in IRModule
        [ irPrintBoxedInt,
          IRFunctionDef
            ( IRFunction
                { irfName = "fst",
                  irfArgs = [(tyAnyPair, "pair")],
                  irfReturn = tyAny,
                  irfBody =
                    [ IRRet
                        tyAny
                        (IRStructPath [0] (IRVar "pair"))
                    ]
                }
            ),
          IRFunctionDef
            ( IRFunction
                { irfName = "main",
                  irfArgs = [],
                  irfReturn = IRInt32,
                  irfBody =
                    [ IRDiscard
                        ( IRApply
                            tyPrintBoxedInt
                            (IRFuncPointer "print_boxed_int")
                            [ irBox IRInt32 $ IRInfix
                                IRAdd
                                (irUnbox $ IRCast tyBoxedInt $ applyFst $ pair
                                    (tyBoxedInt, irBox IRInt32 (IRPrim $ IRPrimInt32 20))
                                    (tyBoxedInt, irBox IRInt32 (IRPrim $ IRPrimInt32 200))
                                )
                                (irUnbox $ IRCast tyBoxedInt $ applyFst $ pair
                                    (tyBoxedInt, irBox IRInt32 (IRPrim $ IRPrimInt32 22))
                                    (tyBoxedBool, irBox IRInt2 (IRPrim $ IRPrimInt2 True))
                                )
                            ]
                        ),
                      IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                    ]
                }
            )
        ]
