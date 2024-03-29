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
                            tyStruct
                            tyStruct
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
                              tyRightInt
                              tyEitherBoolInt
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
                            add1ReturnType
                            add1ReturnType
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
