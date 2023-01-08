{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
  {-# OPTIONS_GHC -fno-warn-orphans #-}
module LLVM.Pretty (
  ppllvm,
  ppll,
) where

import Prelude
import GHC.Word

import LLVM.Pretty.Typed

import LLVM.AST
import LLVM.AST.Global

import LLVM.DataLayout
import LLVM.AST.Attribute
import LLVM.AST.DataLayout
import LLVM.AST.COMDAT
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.InlineAssembly as IA
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.RMWOperation as RMW
import LLVM.AST.Operand hiding (GlobalVariable, Module, NoReturn, PointerType)
import qualified LLVM.AST.Operand as O
import LLVM.AST.ParameterAttribute as PA
import LLVM.AST.FunctionAttribute as FA

import Data.String

import Text.Printf
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (Text, pack )
import qualified Data.ByteString.Short as SBF
import Data.ByteString.Lazy (fromStrict)
import Prettyprinter ((<+>))
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P

import qualified Data.ByteString.Char8 as BL
import qualified Data.ByteString.Short as BS
import Data.Char (chr, ord, isAscii, isControl, isLetter, isDigit)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Maybe (isJust, mapMaybe)
import Numeric (showHex)

import Data.Array.Unsafe
import Data.Array.MArray hiding (index)
import Data.Array.ST hiding (index)
import Control.Monad.ST

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

commas :: [P.Doc ann] -> P.Doc ann
commas  = P.hsep . P.punctuate (P.pretty ',')

hlinecat :: [P.Doc ann] -> P.Doc ann
hlinecat = P.vcat . intersperse softbreak
  where
    softbreak = P.group P.hardline

wrapbraces :: P.Doc ann -> P.Doc ann -> P.Doc ann
wrapbraces leadIn x = (leadIn <> P.pretty '{') <> P.line' <> x <> P.line' <> P.pretty '}'

angleBrackets :: P.Doc ann -> P.Doc ann
angleBrackets x = P.pretty '<' <> x <> P.pretty '>'

spacedbraces :: P.Doc ann -> P.Doc ann
spacedbraces x = P.pretty '{' <+> x <+> P.pretty '}'

local' :: P.Doc ann -> P.Doc ann
local' a = "%" <> a

global :: P.Doc ann -> P.Doc ann
global a = "@" <> a

label :: P.Doc ann -> P.Doc ann
label a = "label" <+> "%" <> a

cma :: P.Doc ann -> P.Doc ann -> P.Doc ann -- <,> does not work :(
a `cma` b = a <> "," <+> b

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- class Pretty p where
--   P.pretty :: p -> P.Doc

ppMaybe :: P.Pretty a => Maybe a -> P.Doc ann
ppMaybe (Just x) = P.pretty x
ppMaybe Nothing = mempty

ppBool :: P.Doc ann -> Bool -> P.Doc ann
ppBool x True = x
ppBool _ False = mempty

-- XXX: horrible hack
unShort :: BS.ShortByteString -> [Char]
unShort xs = toEnum . fromIntegral <$> BS.unpack xs

short :: BS.ShortByteString -> P.Doc ann
short x = P.pretty (pack (unShort x))

decodeShortUtf8 :: SBF.ShortByteString -> Text
decodeShortUtf8 = decodeUtf8 . fromStrict . SBF.fromShort

-- instance Pretty Word8 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Word16 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Word32 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Word64 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Int32 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Int64 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Integer where
--   pretty = integer

instance P.Pretty BS.ShortByteString where
  pretty = P.pretty . unShort

-- instance P.Pretty [Char] where
--   pretty = text . pack

-- instance P.Pretty Bool where
--   pretty True = "true"
--   pretty False = "false"

ppBoolean :: Bool -> P.Doc ann
ppBoolean True = "true"
ppBoolean False = "false"

instance P.Pretty Name where
  pretty (Name nm)
    | BS.null nm = P.dquotes mempty
    | isFirst first && all isRest name = P.pretty (pack name)
    | otherwise = P.dquotes . P.hcat . map escape $ name
    where
        name = unShort nm
        first = head name
        isFirst c = isLetter c || c == '-' || c == '_' || c == '$' || c == '.'
        isRest c = isDigit c || isFirst c
  pretty (UnName x) = P.pretty ( fromIntegral x :: Int)

instance P.Pretty Parameter where
  pretty (Parameter ty (UnName _) attrs) = P.pretty ty <+> ppParamAttributes attrs
  pretty (Parameter ty name attrs) = P.pretty ty <+> ppParamAttributes attrs <+> local' (P.pretty name)

ppParamAttributes :: [ParameterAttribute] -> P.Doc ann
ppParamAttributes pas = P.hsep $ fmap P.pretty pas

-- TODO: Auto instance
-- instance P.Pretty [ParameterAttribute] where
--   pretty x = P.hsep $ fmap pretty x

-- instance P.Pretty ([Parameter], Bool) where
--   pretty (params, False) = commas (fmap pretty params)
--   pretty (params, True) = "TODO" XXX: variadic case

-- instance P.Pretty (Operand, [ParameterAttribute]) where
--   pretty (op, attrs) = pretty (typeOf op) <+> pretty attrs <+> pretty op

ppArguments :: (Operand, [ParameterAttribute]) -> P.Doc ann
ppArguments (op, attrs) = P.pretty (typeOf op) <+> ppParamAttributes attrs <+> P.pretty op

instance P.Pretty UnnamedAddr where
  pretty LocalAddr = "local_unnamed_addr"
  pretty GlobalAddr = "unnamed_addr"

instance P.Pretty Type where
  pretty (IntegerType width) = "i" <> P.pretty width
  pretty (FloatingPointType HalfFP)      = "half"
  pretty (FloatingPointType FloatFP )    = "float"
  pretty (FloatingPointType DoubleFP)    = "double"
  pretty (FloatingPointType FP128FP)     = "fp128"
  pretty (FloatingPointType X86_FP80FP)  = "x86_fp80"
  pretty (FloatingPointType PPC_FP128FP) = "ppc_fp128"

  pretty VoidType = "void"
  pretty (PointerType ref (AS.AddrSpace addr))
    | addr == 0 = P.pretty ref <> "*"
    | otherwise = P.pretty ref <+> "addrspace" <> P.parens (P.pretty addr) <> "*"
  pretty ft@(FunctionType {..}) = P.pretty resultType <+> ppFunctionArgumentTypes ft
  pretty (VectorType {..}) = "<" <> P.pretty nVectorElements <+> "x" <+> P.pretty elementType <> ">"
  pretty (StructureType {..}) = if isPacked
                               then "<{" <> commas (fmap P.pretty elementTypes ) <> "}>"
                               else  "{" <> commas ( fmap P.pretty elementTypes ) <> "}"
  pretty (ArrayType {..}) = P.brackets $ P.pretty nArrayElements <+> "x" <+> P.pretty elementType
  pretty (NamedTypeReference name) = "%" <> P.pretty name
  pretty MetadataType = "metadata"
  pretty TokenType = "token"
  pretty LabelType = "label"

instance P.Pretty Global where
  pretty Function {..} =
      case basicBlocks of
        [] ->
          "declare" <+> P.pretty linkage <+> P.pretty callingConvention
            <+> ppReturnAttributes returnAttributes <+> P.pretty returnType <+> global (P.pretty name)
            <> ppParams (P.pretty . typeOf) parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] ->
            "define" <+> P.pretty linkage <+> P.pretty callingConvention
              <+> ppReturnAttributes returnAttributes <+> P.pretty returnType <+> global (P.pretty name)
              <> ppParams P.pretty parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre
            `wrapbraces` P.indent 2 (ppSingleBlock b)

        bs ->
          ("define" <+> P.pretty linkage <+> P.pretty callingConvention
            <+> ppReturnAttributes returnAttributes <+> P.pretty returnType <+> global (P.pretty name)
            <> ppParams P.pretty parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre)
          `wrapbraces` P.vcat (fmap P.pretty bs)
    where
      pre = case prefix of
              Nothing  -> mempty
              Just con -> "prefix" <+> ppTyped con
      align | alignment == 0    = mempty
            | otherwise = "align" <+> P.pretty alignment
      gcName = maybe mempty (\n -> "gc" <+> P.dquotes (P.pretty $ pack n) . unShort) 

  pretty GlobalVariable {..} = global (P.pretty name) <+> "=" <+> ppLinkage hasInitializer linkage <+> ppMaybe unnamedAddr
                             <+> addrSpace' <+> kind <+> P.pretty type' <+> ppMaybe initializer <> ppAlign alignment
    where
      hasInitializer = isJust initializer
      addrSpace' =
        case addrSpace of
          AS.AddrSpace addr
            | addr == 0 -> mempty
            | otherwise -> "addrspace" <> P.parens (P.pretty addr)
      kind | isConstant = "constant"
           | otherwise  = "global"

  pretty GlobalAlias {..} = global (P.pretty name) <+> "=" <+> P.pretty linkage <+> ppMaybe unnamedAddr <+> "alias" <+> P.pretty type' `cma` ppTyped aliasee

ppFunctionAttribute :: Either GroupID FunctionAttribute -> P.Doc ann
ppFunctionAttribute (Left grpId) = P.pretty grpId
ppFunctionAttribute (Right fA) = P.pretty fA

ppFunctionAttributes :: [Either GroupID FunctionAttribute] -> P.Doc ann
ppFunctionAttributes attribs = P.hsep $ fmap ppFunctionAttribute attribs

ppMetadata :: Maybe Metadata -> P.Doc ann
ppMetadata Nothing = "null"
ppMetadata (Just m) = P.pretty m

instance P.Pretty Definition where
  pretty (GlobalDefinition x) = P.pretty x
  pretty (TypeDefinition nm ty) = local' (P.pretty nm) <+> "=" <+> "type" <+> maybe "opaque" P.pretty ty
  pretty (FunctionAttributes gid attrs) = "attributes" <+> P.pretty gid <+> "=" <+> P.braces (P.hsep (fmap ppAttrInGroup attrs))
  pretty (NamedMetadataDefinition nm meta) = "!" <> short nm <+> "=" <+> "!" <> P.braces (commas (fmap P.pretty meta))
  pretty (MetadataNodeDefinition node meta) = P.pretty node <+> "=" <+> P.pretty meta
  pretty (ModuleInlineAssembly asm) = "module asm" <+> P.dquotes (P.pretty (pack (BL.unpack asm)))
  pretty (COMDAT name selKind) = "$" <> short name <+> "=" <+> "comdat" <+> P.pretty selKind

instance P.Pretty SelectionKind where
  pretty Any = "any"
  pretty ExactMatch = "exactmatch"
  pretty Largest = "largest"
  pretty NoDuplicates = "noduplicates"
  pretty SameSize = "samesize"

ppAttrInGroup :: FunctionAttribute -> P.Doc ann
ppAttrInGroup = \case
  StackAlignment n -> "alignstack=" <> P.pretty n
  attr -> P.pretty attr

instance P.Pretty FunctionAttribute where
  pretty = \case
   NoReturn            -> "noreturn"
   NoUnwind            -> "nounwind"
   FA.ReadNone         -> "readnone"
   FA.ReadOnly         -> "readonly"
   FA.WriteOnly        -> "writeonly"
   NoInline            -> "noinline"
   AlwaysInline        -> "alwaysinline"
   MinimizeSize        -> "minsize"
   OptimizeForSize     -> "optsize"
   OptimizeNone        -> "optnone"
   SafeStack           -> "safestack"
   StackProtect        -> "ssp"
   StackProtectReq     -> "sspreq"
   StackProtectStrong  -> "sspstrong"
   NoRedZone           -> "noredzone"
   NoImplicitFloat     -> "noimplicitfloat"
   Naked               -> "naked"
   InlineHint          -> "inlinehint"
   StackAlignment n    -> "alignstack" <> P.parens (P.pretty n)
   ReturnsTwice        -> "returns_twice"
   UWTable             -> "uwtable"
   NonLazyBind         -> "nonlazybind"
   Builtin             -> "builtin"
   NoBuiltin           -> "nobuiltin"
   Cold                -> "cold"
   JumpTable           -> "jumptable"
   NoDuplicate         -> "noduplicate"
   SanitizeAddress     -> "sanitize_address"
   SanitizeThread      -> "sanitize_thread"
   SanitizeMemory      -> "sanitize_memory"
   SanitizeHWAddress   -> "sanitize_hwaddress"
   NoRecurse           -> "norecurse"
   Convergent          -> "convergent"
   ArgMemOnly          -> "argmemonly"
   InaccessibleMemOnly -> "inaccessiblememonly"
   AllocSize a Nothing -> "allocsize" <> P.parens (P.pretty a)
   AllocSize a (Just b) -> "allocsize" <> P.parens (commas [P.pretty a, P.pretty b])
   InaccessibleMemOrArgMemOnly -> "inaccessiblemem_or_argmemonly"
   FA.StringAttribute k v -> P.dquotes (short k) <> "=" <> P.dquotes (short v)
   Speculatable        -> "speculatable"
   StrictFP            -> "strictfp"

instance P.Pretty ParameterAttribute where
  pretty = \case
    ZeroExt                    -> "zeroext"
    SignExt                    -> "signext"
    InReg                      -> "inreg"
    SRet                       -> "sret"
    Alignment word             -> "align" <+> P.pretty word
    NoAlias                    -> "noalias"
    ByVal                      -> "byval"
    NoCapture                  -> "nocapture"
    Nest                       -> "nest"
    PA.ReadNone                -> "readnone"
    PA.ReadOnly                -> "readonly"
    PA.WriteOnly               -> "writeonly"
    InAlloca                   -> "inalloca"
    NonNull                    -> "nonnull"
    Dereferenceable word       -> "dereferenceable" <> P.parens (P.pretty word)
    DereferenceableOrNull word -> "dereferenceable_or_null" <> P.parens (P.pretty word)
    Returned                   -> "returned"
    SwiftSelf                  -> "swiftself"
    SwiftError                 -> "swifterror"
    ImmArg                     -> "imgarg"
    PA.StringAttribute k v -> P.dquotes (short k) <> "=" <> P.dquotes (short v)

instance P.Pretty CC.CallingConvention where
  pretty = \case
   CC.Numbered word -> "cc" <+> P.pretty word
   CC.C             -> "ccc"
   CC.Fast          -> "fastcc"
   CC.Cold          -> "coldcc"
   CC.GHC           -> "cc 10"
   CC.HiPE          -> "cc 11"
   CC.WebKit_JS     -> "webkit_jscc"
   CC.AnyReg        -> "anyregcc"
   CC.PreserveMost  -> "preserve_mostcc"
   CC.PreserveAll   -> "preserve_allcc"
   CC.Swift         -> "swiftcc"
   CC.CXX_FastTLS   -> "cxx_fast_tlscc"
   CC.X86_StdCall   -> "cc 64"
   CC.X86_FastCall  -> "cc 65"
   CC.ARM_APCS      -> "cc 66"
   CC.ARM_AAPCS     -> "cc 67"
   CC.ARM_AAPCS_VFP -> "cc 68"
   CC.MSP430_INTR   -> "cc 69"
   CC.X86_ThisCall  -> "cc 70"
   CC.PTX_Kernel    -> "cc 71"
   CC.PTX_Device    -> "cc 72"
   CC.SPIR_FUNC     -> "cc 75"
   CC.SPIR_KERNEL   -> "cc 76"
   CC.Intel_OCL_BI  -> "cc 77"
   CC.X86_64_SysV   -> "cc 78"
   CC.Win64         -> "cc 79"
   CC.X86_Intr      -> "x86_intrcc"
   CC.X86_RegCall   -> "x86_regcallcc"
   CC.X86_VectorCall -> "x86_vectorcallcc"
   CC.AVR_Intr      -> "avr_intrcc"
   CC.AVR_Signal    -> "avr_signalcc"
   CC.AVR_Builtin   -> "cc 86"
   CC.HHVM          -> "hhvmcc"
   CC.HHVM_C        -> "hhvm_ccc"
   CC.AMDGPU_VS     -> "amdgpu_vs"
   CC.AMDGPU_GS     -> "amdgpu_gs"
   CC.AMDGPU_PS     -> "amdgpu_ps"
   CC.AMDGPU_CS     -> "amdgpu_cs"
   CC.AMDGPU_HS     -> "amdgpu_hs"
   CC.AMDGPU_Kernel -> "amdgpu_kernel"
   CC.MSP430_Builtin -> "msp430"

instance P.Pretty L.Linkage where
    pretty = ppLinkage False

ppLinkage :: Bool -> L.Linkage -> P.Doc ann
ppLinkage omitExternal = \case
   L.External | omitExternal -> mempty
              | otherwise    -> "external"
   L.Private                 -> "private"
   L.Internal                -> "internal"
   L.ExternWeak              -> "extern_weak"
   L.AvailableExternally     -> "available_externally"
   L.LinkOnce                -> "linkonce"
   L.Weak                    -> "weak"
   L.Common                  -> "common"
   L.Appending               -> "appending"
   L.LinkOnceODR             -> "linkonce_odr"
   L.WeakODR                 -> "weak_odr"

ppInstructionMetadata :: InstructionMetadata -> P.Doc ann
ppInstructionMetadata meta = commas ["!" <> short x <+> P.pretty y | (x,y) <- meta]

instance P.Pretty MetadataNodeID where
  pretty (MetadataNodeID x) = "!" <> P.pretty (fromIntegral x :: Int)

instance P.Pretty GroupID where
  pretty (GroupID x) = "#" <> P.pretty (fromIntegral x :: Int)

instance P.Pretty BasicBlock where
  pretty (BasicBlock nm instrs term) =
    blockLabel <> P.line <> P.indent 2 (P.vcat $ fmap P.pretty instrs ++ [P.pretty term])
    where
      blockLabel = case nm of
        UnName _ -> "; <label>:" <> P.pretty nm <> ":"
        _ -> P.pretty nm <> ":"

instance P.Pretty Terminator where
  pretty = \case
    Br dest meta -> "br" <+> label (P.pretty dest) <+> ppInstrMeta meta

    Ret val meta -> "ret" <+> maybe "void" ppTyped val <+> ppInstrMeta meta

    CondBr cond tdest fdest meta ->
     "br" <+> ppTyped cond
     `cma` label (P.pretty tdest)
     `cma` label (P.pretty fdest)
     <+> ppInstrMeta meta

    Switch {..} -> "switch" <+> ppTyped operand0'
                 `cma` label (P.pretty defaultDest)
                 <+> P.brackets (P.hsep [ ppTyped v `cma` label (P.pretty l) | (v,l) <- dests ])
                 <+> ppInstrMeta metadata'

    Unreachable {..} -> "unreachable" <+> ppInstrMeta metadata'

    IndirectBr op dests meta -> "indirectbr" <+> ppTyped op `cma`
     P.brackets (P.hsep [ label (P.pretty l) | l <- dests ])
     <+> ppInstrMeta meta

    e@Invoke {..} ->
     ppInvoke e
     <+> "to" <+> label (P.pretty returnDest)
     <+> "unwind" <+> label (P.pretty exceptionDest)
     <+> ppInstrMeta metadata'

    Resume op meta -> "resume "<+> ppTyped op <+> ppInstrMeta meta

    CleanupRet pad dest meta ->
      "cleanupret" <+> "from" <+> P.pretty pad <+> "unwind" <+> maybe "to caller" (label . P.pretty) dest
      <+> ppInstrMeta meta

    CatchRet catchPad succ' meta ->
      "catchret" <+> "from" <+> P.pretty catchPad <+> "to" <+> label (P.pretty succ')
      <+> ppInstrMeta meta

    CatchSwitch {..} ->
      "catchswitch" <+> "within" <+> P.pretty parentPad' <+>
      P.brackets (commas (map (label . P.pretty) (toList catchHandlers))) <+>
      "unwind" <+> "to" <+> maybe "caller" P.pretty defaultUnwindDest
      <+> ppInstrMeta metadata'

instance P.Pretty Instruction where
  pretty = \case
    Add {..}    -> ppInstrWithNuwNsw "add" nuw nsw operand0 operand1 metadata
    Sub {..}    -> ppInstrWithNuwNsw "sub" nuw nsw operand0 operand1 metadata
    Mul {..}    -> ppInstrWithNuwNsw "mul" nuw nsw operand0 operand1 metadata
    Shl {..}    -> ppInstrWithNuwNsw "shl" nuw nsw operand0 operand1 metadata
    AShr {..}   -> ppInstrWithExact "ashr" exact operand0 operand1 metadata
    LShr {..}   -> ppInstrWithExact "lshr" exact operand0 operand1 metadata

    And {..}    -> "and"  <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    Or {..}     -> "or"   <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    Xor {..}    -> "xor"  <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    SDiv {..}   -> ppInstrWithExact "sdiv" exact operand0 operand1 metadata
    UDiv {..}   -> ppInstrWithExact "udiv" exact operand0 operand1 metadata
    SRem {..}   -> "srem"  <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    URem {..}   -> "urem"  <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata

    FAdd {..}   -> "fadd" <+> P.pretty fastMathFlags <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    FSub {..}   -> "fsub" <+> P.pretty fastMathFlags <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    FMul {..}   -> "fmul" <+> P.pretty fastMathFlags <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    FDiv {..}   -> "fdiv" <+> P.pretty fastMathFlags <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    FRem {..}   -> "frem" <+> P.pretty fastMathFlags <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata
    FCmp {..}   -> "fcmp" <+> P.pretty fpPredicate <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata

    Alloca {..} -> "alloca" <+> P.pretty allocatedType <> num <> ppAlign alignment <+> ppInstrMeta metadata
      where num   = case numElements of Nothing -> mempty
                                        Just o -> "," <+> ppTyped o
    Store {..}  -> "store" <+> ppMAtomicity maybeAtomicity <+> ppVolatile volatile <+> ppTyped value `cma` ppTyped address <+> ppMOrdering maybeAtomicity <> ppAlign alignment <+> ppInstrMeta metadata
    Load {..}   -> "load" <+> ppMAtomicity maybeAtomicity <+> ppVolatile volatile <+> P.pretty argTy `cma` ppTyped address <+> ppMOrdering maybeAtomicity <> ppAlign alignment <+> ppInstrMeta metadata
      where
        argTy = case typeOf address of
          PointerType argTy_ _ -> argTy_
          _ -> error "invalid load of non-pointer type. (Malformed AST)"
    Phi {..}    -> "phi" <+> P.pretty type' <+> commas (fmap phiIncoming incomingValues) <+> ppInstrMeta metadata

    ICmp {..}   -> "icmp" <+> P.pretty iPredicate <+> ppTyped operand0 `cma` P.pretty operand1 <+> ppInstrMeta metadata

    c@Call {..} -> ppCall c  <+> ppInstrMeta metadata
    Select {..} -> "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue] <+> ppInstrMeta metadata
    SExt {..}   -> "sext" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    ZExt {..}   -> "zext" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPExt {..}   -> "fpext" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    Trunc {..}  -> "trunc" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPTrunc {..}  -> "fptrunc" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata

    GetElementPtr {..} -> "getelementptr" <+> bounds inBounds <+> commas (P.pretty argTy : fmap ppTyped (address:indices)) <+> ppInstrMeta metadata
      where argTy = getElementType $ typeOf address
    ExtractValue {..} -> "extractvalue" <+> commas (ppTyped aggregate : fmap P.pretty indices') <+> ppInstrMeta metadata

    BitCast {..} -> "bitcast" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata
    FPToUI {..} -> "fptoui" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata
    FPToSI {..} -> "fptosi" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata
    UIToFP {..} -> "uitofp" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata
    SIToFP {..} -> "sitofp" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata
    PtrToInt {..} -> "ptrtoint" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata
    IntToPtr {..} -> "inttoptr" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata

    InsertElement {..} -> "insertelement" <+> commas [ppTyped vector, ppTyped element, ppTyped index] <+> ppInstrMeta metadata
    ShuffleVector {..} -> "shufflevector" <+> commas [ppTyped operand0, ppTyped operand1, ppTyped mask] <+> ppInstrMeta metadata
    ExtractElement {..} -> "extractelement" <+> commas [ppTyped vector, ppTyped index] <+> ppInstrMeta metadata
    InsertValue {..} -> "insertvalue" <+> commas (ppTyped aggregate : ppTyped element : fmap P.pretty indices') <+> ppInstrMeta metadata

    Fence {..} -> "fence" <+> ppAtomicity atomicity <+> ppInstrMeta metadata
    AtomicRMW {..} -> "atomicrmw" <+> ppVolatile volatile <+> P.pretty rmwOperation <+> ppTyped address `cma` ppTyped value <+> ppAtomicity atomicity  <+> ppInstrMeta metadata
    CmpXchg {..} -> "cmpxchg" <+> ppVolatile volatile <+> ppTyped address `cma` ppTyped expected `cma` ppTyped replacement
      <+> ppAtomicity atomicity <+> P.pretty failureMemoryOrdering <+> ppInstrMeta metadata

    AddrSpaceCast {..} -> "addrspacecast" <+> ppTyped operand0 <+> "to" <+> P.pretty type' <+> ppInstrMeta metadata
    VAArg {..} -> "va_arg" <+> ppTyped argList `cma` P.pretty type' <+> ppInstrMeta metadata

    LandingPad {..} ->
      "landingpad" <+> P.pretty type' <+> ppBool "cleanup" cleanup <+> ppInstrMeta metadata
      <+> commas (fmap P.pretty clauses)
    CatchPad {..} -> "catchpad" <+> "within" <+> P.pretty catchSwitch <+> P.brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata
    CleanupPad {..} -> "cleanuppad" <+> "within" <+> P.pretty parentPad <+> P.brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata

    where
      bounds True = "inbounds"
      bounds False = mempty

      ppInstrWithNuwNsw :: P.Doc ann -> Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> P.Doc ann
      ppInstrWithNuwNsw name nuw nsw op0 op1 metadata =
        name
        <+> ppBool "nuw" nuw
        <+> ppBool "nsw" nsw
        <+> ppTyped op0
        `cma` P.pretty op1
        <+> ppInstrMeta metadata

      ppInstrWithExact :: P.Doc ann -> Bool -> Operand -> Operand -> InstructionMetadata -> P.Doc ann
      ppInstrWithExact name exact op0 op1 metadata =
        name
        <+> ppBool "exact" exact
        <+> ppTyped op0
        `cma` P.pretty op1
        <+> ppInstrMeta metadata

instance P.Pretty CallableOperand where
  pretty (Left _asm) = error "CallableOperand"
  pretty (Right op) = P.pretty op

instance P.Pretty LandingPadClause where
  pretty = \case
    Catch c  -> "catch" <+> ppTyped c
    Filter c -> "filter" <+> ppTyped c

-- instance P.Pretty [Either GroupID FunctionAttribute] where
--   pretty x = P.hsep $ fmap pretty x

instance P.Pretty (Either GroupID FunctionAttribute) where
  pretty (Left gid) = P.pretty gid
  pretty (Right fattr) = P.pretty fattr

instance P.Pretty Operand where
  pretty (LocalReference _ nm) = local' (P.pretty nm)
  pretty (ConstantOperand con) = P.pretty con
  pretty (MetadataOperand mdata) = P.pretty mdata

instance P.Pretty Metadata where
  pretty (MDString str) = "!" <> P.dquotes (P.pretty (decodeShortUtf8 str))
  pretty (MDNode node) = P.pretty node
  pretty (MDValue operand) = ppTyped operand

ppDINode :: [Char] -> [([Char], Maybe (P.Doc ann))] -> P.Doc ann
ppDINode name attrs = "!" <> P.pretty name <> P.parens (commas (mapMaybe (\(k, mayV) -> fmap (\v -> P.pretty k <> ":" <+> v) mayV) attrs))

ppDIArray :: [P.Doc ann] -> Maybe (P.Doc ann)
ppDIArray [] = Nothing
ppDIArray xs = Just ("!" <> P.braces (commas xs))

instance P.Pretty a => P.Pretty (MDRef a) where
  pretty (MDInline a) = P.pretty a
  pretty (MDRef ref) = P.pretty ref

instance P.Pretty MDNode where
  pretty (MDTuple xs) = "!" <> P.braces (commas (map ppMetadata xs))
  pretty (DIExpression e) = P.pretty e
  pretty (DIGlobalVariableExpression e) = P.pretty e
  pretty (DILocation l) = P.pretty l
  pretty (DIMacroNode m) = P.pretty m
  pretty (DINode n) = P.pretty n

instance P.Pretty DIExpression where
  pretty (Expression os) = "!DIExpression" <> P.parens (commas (concatMap ppDWOp os))

ppDWOp :: DWOp -> [P.Doc ann]
ppDWOp o = case o of
  DwOpFragment DW_OP_LLVM_Fragment {..} -> ["DW_OP_LLVM_fragment", P.pretty offset, P.pretty size]
  DW_OP_StackValue -> ["DW_OP_stack_value"]
  DW_OP_Swap -> ["DW_OP_swap"]
  DW_OP_ConstU c -> ["DW_OP_constu", P.pretty c]
  DW_OP_PlusUConst c -> ["DW_OP_plus_uconst", P.pretty c]
  DW_OP_Plus -> ["DW_OP_plus"]
  DW_OP_Minus -> ["DW_OP_minus"]
  DW_OP_Mul -> ["DW_OP_mul"]
  DW_OP_Deref -> ["DW_OP_deref"]
  DW_OP_XDeref -> ["DW_OP_xderef"]
  DW_OP_Lit0 -> ["DW_OP_Lit0"]
  DW_OP_Div -> ["DW_OP_Div"]
  DW_OP_Mod -> ["DW_OP_Mod"]
  DW_OP_Not -> ["DW_OP_Not"]
  DW_OP_Or -> ["DW_OP_Or"]
  DW_OP_Xor -> ["DW_OP_Xor"]
  DW_OP_And -> ["DW_OP_And"]
  DW_OP_Shr -> ["DW_OP_Shr"]
  DW_OP_Shra -> ["DW_OP_Shra"]
  DW_OP_Shl -> ["DW_OP_Shl"]
  DW_OP_Dup -> ["DW_OP_Dup"]

instance P.Pretty DIGlobalVariableExpression where
  pretty e = ppDINode "DIGlobalVariableExpression"
    [ ("var", Just (P.pretty (var e)))
    , ("expr", Just (P.pretty (expr e)))
    ]

instance P.Pretty DILocation where
  pretty (Location line col scope) =
    ppDINode "DILocation" [("line", Just (P.pretty line)), ("column", Just (P.pretty col)), ("scope", Just (P.pretty scope))]

instance P.Pretty DIMacroNode where
  pretty DIMacro {..} = ppDINode "DIMacro"
    [("type", Just (P.pretty info)), ("line", Just (P.pretty line)), ("name", ppSbs name), ("value", ppSbs value)]
  pretty DIMacroFile {..} = ppDINode "DIMacroFile"
    [ ("line", Just (P.pretty line))
    , ("file", Just (P.pretty file))
    , ("nodes", ppDIArray (map P.pretty elements))
    ]

instance P.Pretty DIMacroInfo where
  pretty Define = "DW_MACINFO_define"
  pretty Undef = "DW_MACINFO_undef"

instance P.Pretty DINode where
  pretty (DIEnumerator e) = P.pretty e
  pretty (DIImportedEntity e) = P.pretty e
  pretty (DIObjCProperty p) = P.pretty p
  pretty (DIScope s) = P.pretty s
  pretty (DISubrange r) = P.pretty r
  pretty (DITemplateParameter p) = P.pretty p
  pretty (DIVariable v) = P.pretty v

instance P.Pretty DILocalScope where
  pretty (DILexicalBlockBase b) = P.pretty b
  pretty (DISubprogram p) = P.pretty p

instance P.Pretty DIEnumerator where
  pretty (Enumerator val unsigned name) =
    ppDINode "DIEnumerator"
      [ ("name", ppSbs name)
      , ("isUnsigned", if unsigned then Just "true" else Nothing)
      , ("value", Just (P.pretty val))]

instance P.Pretty DIImportedEntity where
  pretty ImportedEntity {..} = ppDINode "DIImportedEntity"
    [ ("tag", Just (P.pretty tag))
    , ("scope", Just (P.pretty scope))
    , ("name", ppSbs name)
    , ("entity", fmap P.pretty entity)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    ]

instance P.Pretty ImportedEntityTag where
  pretty ImportedModule = "DW_TAG_imported_module"
  pretty ImportedDeclaration = "DW_TAG_imported_declaration"

instance P.Pretty DIObjCProperty where
  pretty ObjCProperty {..} = ppDINode "DIObjCProperty"
    [ ("name", ppSbs name)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("setter", ppSbs getterName)
    , ("getter", ppSbs setterName)
    , ("attributes", Just (P.pretty attributes))
    , ("type", fmap P.pretty type')
    ]

instance P.Pretty DIScope where
  pretty (DICompileUnit cu) = P.pretty cu
  pretty (DIFile f) = P.pretty f
  pretty (DILocalScope s) = P.pretty s
  pretty (DIModule m) = P.pretty m
  pretty (DINamespace ns) = P.pretty ns
  pretty (DIType t) = P.pretty t

instance P.Pretty DISubrange where
  pretty Subrange {..} = ppDINode "DISubrange" [("count", Just (P.pretty count)), ("lowerBound", Just (P.pretty lowerBound))]

instance P.Pretty DICount where
  pretty (DICountConstant c) = P.pretty c
  pretty (DICountVariable v) = P.pretty v

instance P.Pretty DITemplateParameter where
  pretty DITemplateTypeParameter {..} = ppDINode "DITemplateTypeParameter"
    [ ("name", ppSbs name), ("type", Just (P.pretty type')) ]
  pretty DITemplateValueParameter {..} = ppDINode "DITemplateValueParameter"
   [ ("tag", ppTemplateValueParameterTag tag)
   , ("name", ppSbs name)
   , ("type", Just (P.pretty type'))
   , ("value", Just (P.pretty value))
   ]

ppTemplateValueParameterTag :: TemplateValueParameterTag -> Maybe (P.Doc ann)
ppTemplateValueParameterTag TemplateValueParameter = Nothing
ppTemplateValueParameterTag GNUTemplateTemplateParam = Just "DW_TAG_GNU_template_template_param"
ppTemplateValueParameterTag GNUTemplateParameterPack = Just "DW_TAG_GNU_template_parameter_pack"

instance P.Pretty DIVariable where
  pretty (DIGlobalVariable v) = P.pretty v
  pretty (DILocalVariable v) = P.pretty v

instance P.Pretty DICompileUnit where
  pretty CompileUnit {..} = "distinct" <+> ppDINode "DICompileUnit"
    [ ("language", Just (P.pretty language))
    , ("file", Just (P.pretty file))
    , ("producer", ppSbs producer)
    , ("isOptimized", Just (ppBoolean optimized))
    , ("flags", ppSbs flags)
    , ("runtimeVersion", Just (P.pretty runtimeVersion))
    , ("splitDebugFileName", ppSbs splitDebugFileName)
    , ("emissionKind", Just (P.pretty emissionKind))
    , ("enums", ppDIArray (map P.pretty enums))
    , ("retainedTypes", ppDIArray (map ppEither retainedTypes))
    , ("globals", ppDIArray (map P.pretty globals))
    , ("imports", ppDIArray (map P.pretty imports))
    , ("macros", ppDIArray (map P.pretty macros))
    , ("dwoId", Just (P.pretty dWOId))
    , ("splitDebugInlining", Just (ppBoolean splitDebugInlining))
    , ("debugInfoForProfiling", Just (ppBoolean debugInfoForProfiling))
    , ("nameTableKind", Just (P.pretty nameTableKind))
    , ("debugBaseAddress", Just (ppBoolean debugBaseAddress))
    ]

instance P.Pretty DebugEmissionKind where
  pretty NoDebug = "NoDebug"
  pretty FullDebug = "FullDebug"
  pretty LineTablesOnly = "LineTablesOnly"

instance P.Pretty DIFile where
  pretty (File {..}) = ppDINode "DIFile" $
    [ ("filename", Just (P.dquotes (P.pretty filename)))
    , ("directory", Just (P.dquotes (P.pretty directory)))
    ]
    <> ppDIChecksum checksum

ppDIChecksum :: Maybe ChecksumInfo -> [([Char], Maybe (P.Doc ann))]
ppDIChecksum Nothing = []
ppDIChecksum (Just (ChecksumInfo kind val)) = [("checksumkind", Just (P.pretty kind)), ("checksum", ppSbs val)]

instance P.Pretty DIModule where
  pretty O.Module {..} = ppDINode "DIModule"
    [ ("scope", Just (maybe "null" P.pretty scope))
    , ("name", ppSbs name)
    , ("configMacros", ppSbs configurationMacros)
    , ("includePath", ppSbs includePath)
    , ("isysroot", ppSbs isysRoot)
    ]

instance P.Pretty DINamespace where
  pretty Namespace {..} = ppDINode "DINamespace"
    [ ("name", ppSbs name)
    , ("scope", Just (maybe "null" P.pretty scope))
    , ("exportSymbols", Just (ppBoolean exportSymbols))
    ]

instance P.Pretty DIType where
  pretty (DIBasicType t) = P.pretty t
  pretty (DICompositeType t) = P.pretty t
  pretty (DIDerivedType t) = P.pretty t
  pretty (DISubroutineType t) = P.pretty t

instance P.Pretty DILexicalBlockBase where
  pretty DILexicalBlock {..} = ppDINode "DILexicalBlock"
    [ ("scope", Just (P.pretty scope))
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("column", Just (P.pretty column))
    ]
  pretty DILexicalBlockFile {..} = ppDINode "DILexicalBlockFile"
    [ ("scope", Just (P.pretty scope)), ("file", fmap P.pretty file), ("discriminator", Just (P.pretty discriminator)) ]

ppSbs :: BS.ShortByteString -> Maybe (P.Doc ann)
ppSbs s
  | SBF.null s = Nothing
  | otherwise = Just (P.dquotes (P.pretty s))

instance P.Pretty DISubprogram where
  pretty Subprogram {..} = ppMaybe (if definition then Just ("distinct " :: [Char]) else Nothing) <>
   ppDINode "DISubprogram"
   [ ("name", ppSbs name)
   , ("linkageName", ppSbs linkageName)
   , ("scope", fmap P.pretty scope)
   , ("file", fmap P.pretty file)
   , ("line", Just (P.pretty line))
   , ("type", fmap P.pretty type')
   , ("isLocal", Just (ppBoolean localToUnit))
   , ("isDefinition", Just (ppBoolean definition))
   , ("scopeLine", Just (P.pretty scopeLine))
   , ("containingType", fmap P.pretty containingType)
   , ("virtuality", ppVirtuality virtuality)
   , ("virtualIndex", Just (P.pretty virtualityIndex))
   , ("thisAdjustment", Just (P.pretty thisAdjustment))
   , ("flags", ppDIFlags flags)
   , ("isOptimized", Just (ppBoolean optimized))
   , ("unit", fmap P.pretty unit)
   , ("templateParams", ppDIArray (map P.pretty templateParams))
   , ("declaration", fmap P.pretty declaration)
   , ("retainedNodes", ppDIArray (map P.pretty retainedNodes))
   , ("thrownTypes", ppDIArray (map P.pretty thrownTypes))
   ]

ppVirtuality :: Virtuality -> Maybe (P.Doc ann)
ppVirtuality NoVirtuality = Nothing
ppVirtuality Virtual = Just "DW_VIRTUALITY_virtual"
ppVirtuality PureVirtual = Just "DW_VIRTUALITY_pure_virtual"

instance P.Pretty ChecksumKind where
  pretty MD5 = "CSK_MD5"
  pretty SHA1 = "CSK_SHA1"

instance P.Pretty DIBasicType where
  pretty (BasicType {..}) = ppDINode "DIBasicType"
    [ ("tag", Just (P.pretty tag))
    , ("name", ppSbs name)
    , ("size", Just (P.pretty sizeInBits))
    , ("align", Just (P.pretty alignInBits))
    , ("encoding", fmap P.pretty encoding)
    ]

instance P.Pretty BasicTypeTag where
  pretty BaseType = "DW_TAG_base_type"
  pretty UnspecifiedType = "DW_TAG_unspecified_type"

instance P.Pretty Encoding where
  pretty e = case e of
    AddressEncoding -> "DW_ATE_address"
    BooleanEncoding -> "DW_ATE_boolean"
    UTFEncoding -> "DW_ATE_UTF"
    FloatEncoding -> "DW_ATE_float"
    SignedEncoding -> "DW_ATE_signed"
    SignedCharEncoding -> "DW_ATE_signed_char"
    UnsignedEncoding -> "DW_ATE_unsigned"
    UnsignedCharEncoding -> "DW_ATE_unsigned_char"

ppDIFlags :: [DIFlag] -> Maybe (P.Doc ann)
ppDIFlags [] = Nothing
ppDIFlags flags = Just (P.hsep (P.punctuate (P.pretty '|') (map P.pretty flags)))

instance P.Pretty DIFlag where
  pretty flag = "DIFlag" <> fromString (flagName flag)
    where
      flagName (Accessibility f) = show f
      flagName (InheritanceFlag f) = show f
      flagName VirtualFlag = "Virtual"
      flagName f = show f


ppEither :: (P.Pretty a, P.Pretty b) => MDRef (Either a b) -> P.Doc ann
ppEither (MDRef r) = P.pretty r
ppEither (MDInline e) = either P.pretty P.pretty e

instance P.Pretty DICompositeType where
  pretty DIArrayType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_array_type")
    , ("elements", ppDIArray (map P.pretty subscripts))
    , ("baseType", fmap P.pretty elementTy)
    , ("size", Just (P.pretty sizeInBits))
    , ("align", Just (P.pretty alignInBits))
    , ("flags", ppDIFlags flags)
    ]
  pretty DIClassType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_class_type")
    , ("scope", fmap P.pretty scope)
    , ("name", ppSbs name)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("flags", ppDIFlags flags)
    , ("baseType", fmap P.pretty derivedFrom)
    , ("elements", ppDIArray (map ppEither elements))
    , ("vtableHolder", fmap P.pretty vtableHolder)
    , ("templateParams", ppDIArray (map P.pretty templateParams))
    , ("identifier", ppSbs identifier)
    , ("size", Just (P.pretty sizeInBits))
    , ("align", Just (P.pretty alignInBits))
    ]
  pretty DIEnumerationType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_enumeration_type")
    , ("name", ppSbs name)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("size", Just (P.pretty sizeInBits))
    , ("align", Just (P.pretty alignInBits))
    , ("elements", Just ("!" <> P.braces (commas (map P.pretty values))))
    , ("scope", fmap P.pretty scope)
    , ("identifier", ppSbs identifier)
    , ("baseType", fmap P.pretty baseType)
    ]
  pretty DIStructureType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_structure_type")
    , ("scope", fmap P.pretty scope)
    , ("name", ppSbs name)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("flags", ppDIFlags flags)
    , ("baseType", fmap P.pretty derivedFrom)
    , ("elements", ppDIArray (map ppEither elements))
    , ("runtimeLang", Just (P.pretty runtimeLang))
    , ("vtableHolder", fmap P.pretty vtableHolder)
    , ("identifier", ppSbs identifier)
    , ("size", Just (P.pretty sizeInBits))
    , ("align", Just (P.pretty alignInBits))
    ]
  pretty DIUnionType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_union_type")
    , ("name", ppSbs name)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("flags", ppDIFlags flags)
    , ("elements", ppDIArray (map ppEither elements))
    , ("runtimeLang", Just (P.pretty runtimeLang))
    , ("identifier", ppSbs identifier)
    , ("size", Just (P.pretty sizeInBits))
    , ("align", Just (P.pretty alignInBits))
    ]

instance P.Pretty DIDerivedType where
  pretty DerivedType {..} = ppDINode "DIDerivedType"
    [ ("tag", Just (P.pretty tag))
    , ("name", ppSbs name)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("scope", fmap P.pretty scope)
    , ("baseType", Just (P.pretty baseType))
    , ("size", Just (P.pretty sizeInBits))
    , ("align", Just (P.pretty alignInBits))
    , ("offset", Just (P.pretty offsetInBits))
    , ("flags", ppDIFlags flags)
    , ("dwarfAddressSpace", fmap P.pretty addressSpace)
    ]

instance P.Pretty DerivedTypeTag where
  pretty t =
    case t of
      Typedef -> "DW_TAG_typedef"
      O.PointerType -> "DW_TAG_pointer_type"
      PtrToMemberType -> "DW_TAG_ptr_to_member_type"
      ReferenceType -> "DW_TAG_reference_type"
      RValueReferenceType -> "DW_TAG_rvalue_reference_type"
      ConstType -> "DW_TAG_const_type"
      VolatileType -> "DW_TAG_volatile_type"
      RestrictType -> "DW_TAG_restrict_type"
      AtomicType -> "DW_TAG_atomic_type"
      Member -> "DW_TAG_member"
      Inheritance -> "DW_TAG_inheritance"
      Friend -> "DW_TAG_friend"

instance P.Pretty DISubroutineType where
  pretty SubroutineType {..} = ppDINode "DISubroutineType"
    [ ("flags", ppDIFlags flags)
    , ("cc", Just (P.pretty cc))
    , ("types", Just ("!" <> P.braces (commas (map ppTy typeArray))))
    ]
    where ppTy Nothing = "null"
          ppTy (Just t) = P.pretty t

instance P.Pretty DIGlobalVariable where
  pretty O.GlobalVariable {..} = ppDINode "DIGlobalVariable"
    [ ("name", ppSbs name)
    , ("scope", fmap P.pretty scope)
    , ("linkageName", ppSbs linkageName)
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("type", fmap P.pretty type')
    , ("isLocal", Just (ppBoolean local))
    , ("isDefinition", Just (ppBoolean definition))
    , ("declaration", fmap P.pretty staticDataMemberDeclaration)
    , ("align", Just (P.pretty alignInBits))
    ]

instance P.Pretty DILocalVariable where
  pretty LocalVariable {..} = ppDINode "DILocalVariable"
    [ ("name", ppSbs name)
    , ("scope", Just (P.pretty scope))
    , ("file", fmap P.pretty file)
    , ("line", Just (P.pretty line))
    , ("type", fmap P.pretty type')
    , ("flags", ppDIFlags flags)
    , ("arg", Just (P.pretty arg))
    , ("align", Just (P.pretty alignInBits))
    ]

instance P.Pretty C.Constant where
  pretty (C.Int _width val) = P.pretty val
  pretty (C.Float (F.Double val))      =
    if specialFP val
      then "0x" <> (P.pretty . pack) (showHex (doubleToWord val) "")
      else P.pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.Single val))      =
    if specialFP val
      then "0x" <> (P.pretty . pack) (showHex (floatToWord val) "")
      else P.pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.Half val))        = P.pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.Quadruple val _)) = P.pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.X86_FP80 val _))  = P.pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.PPC_FP128 val _)) = P.pretty $ pack $ printf "%6.6e" val

  pretty (C.GlobalReference _ty nm) = "@" <> P.pretty nm
  pretty (C.Vector args) = "<" <+> commas (fmap ppTyped args) <+> ">"

  pretty (C.Add {..})    = "add"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.Sub {..})    = "sub"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.Mul {..})    = "mul"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.Shl {..})    = "shl"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.AShr {..})   = "ashr" <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.LShr {..})   = "lshr" <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.And {..})    = "and"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.Or {..})     = "or"   <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.Xor {..})    = "xor"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.SDiv {..})   = "sdiv"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.UDiv {..})   = "udiv"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.SRem {..})   = "srem"  <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.URem {..})   = "urem"  <+> ppTyped operand0 `cma` P.pretty operand1

  pretty (C.FAdd {..})   = "fadd" <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.FSub {..})   = "fsub" <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.FMul {..})   = "fmul" <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.FDiv {..})   = "fdiv" <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.FRem {..})   = "frem" <+> ppTyped operand0 `cma` P.pretty operand1
  pretty (C.FCmp {..})   = "fcmp" <+> P.pretty fpPredicate <+> ppTyped operand0 `cma` P.pretty operand1
  pretty C.ICmp {..}     = "icmp" <+> P.pretty iPredicate <+> ppTyped operand0 `cma` P.pretty operand1

  pretty (C.Select {..})  = "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue]
  pretty (C.SExt {..})    = "sext" <+> ppTyped operand0 <+> "to" <+> P.pretty type'
  pretty (C.ZExt {..})    = "zext" <+> ppTyped operand0 <+> "to" <+> P.pretty type'
  pretty (C.FPExt {..})   = "fpext" <+> ppTyped operand0 <+> "to" <+> P.pretty type'
  pretty (C.Trunc {..})   = "trunc" <+> ppTyped operand0 <+> "to" <+> P.pretty type'
  pretty (C.FPTrunc {..}) = "fptrunc" <+> ppTyped operand0 <+> "to" <+> P.pretty type'

  pretty C.FPToUI {..} = "fptoui" <+> ppTyped operand0 <+> "to" <+> P.pretty type'
  pretty C.FPToSI {..} = "fptosi" <+> ppTyped operand0 <+> "to" <+> P.pretty type'
  pretty C.UIToFP {..} = "uitofp" <+> ppTyped operand0 <+> "to" <+> P.pretty type'
  pretty C.SIToFP {..} = "sitofp" <+> ppTyped operand0 <+> "to" <+> P.pretty type'

  pretty (C.Struct _ packed elems) =
    let struct = spacedbraces $ commas $ fmap ppTyped elems
    in if packed
         then angleBrackets struct
         else struct

  pretty (C.Null constantType) = ppNullInitializer constantType
  pretty (C.AggregateZero _constantType) = "zeroinitializer"
  pretty (C.Undef {}) = "undef"
  pretty (C.TokenNone {}) = "none"
  pretty (C.BlockAddress fn blk) = "blockaddress" <> P.parens (commas [ global (P.pretty fn), local' (P.pretty blk) ])

  pretty C.Array {..}
    | memberType == (IntegerType 8) = "c" <> P.dquotes (P.hcat [ppIntAsChar val | C.Int _ val <- memberValues])
    | otherwise = P.brackets $ commas $ fmap ppTyped memberValues

  pretty C.GetElementPtr {..} = "getelementptr" <+> bounds inBounds <+> P.parens (commas (P.pretty argTy : fmap ppTyped (address:indices)))
    where
      argTy = case typeOf address of
        PointerType argTy_ _ -> argTy_
        _ -> error "invalid load of non-pointer type. (Malformed AST)"
      bounds True = "inbounds"
      bounds False = mempty

  pretty C.BitCast {..} = "bitcast" <+> P.parens (ppTyped operand0 <+> "to" <+> P.pretty type')
  pretty C.PtrToInt {..} = "ptrtoint" <+> P.parens (ppTyped operand0 <+> "to" <+> P.pretty type')
  pretty C.IntToPtr {..} = "inttoptr" <+> P.parens (ppTyped operand0 <+> "to" <+> P.pretty type')
  pretty C.AddrSpaceCast {..} = "addrspacecast" <+> P.parens (ppTyped operand0 <+> "to" <+> P.pretty type')
  pretty _ = error "Non-function argument. (Malformed AST)"

instance P.Pretty a => P.Pretty (Named a) where
  pretty (nm := a) = "%" <> P.pretty nm <+> "=" <+> P.pretty a
  pretty (Do a) = P.pretty a

instance P.Pretty Module where
  pretty Module {..} =
    let header = printf "; ModuleID = '%s'" (unShort moduleName) in
    let target = case moduleTargetTriple of
                      Nothing -> mempty
                      Just target' -> "target triple =" <+> P.dquotes (short target') in
    let layout = case moduleDataLayout of
                      Nothing     -> mempty
                      Just layout' -> "target datalayout =" <+> P.dquotes (P.pretty layout') in
    hlinecat (fromString header : (layout <> P.softline <> target) : (fmap P.pretty moduleDefinitions))

instance P.Pretty FP.FloatingPointPredicate where
  pretty op = case op of
   FP.False -> "false"
   FP.OEQ   -> "oeq"
   FP.OGT   -> "ogt"
   FP.OGE   -> "oge"
   FP.OLT   -> "olt"
   FP.OLE   -> "ole"
   FP.ONE   -> "one"
   FP.ORD   -> "ord"
   FP.UEQ   -> "ueq"
   FP.UGT   -> "ugt"
   FP.UGE   -> "uge"
   FP.ULT   -> "ult"
   FP.ULE   -> "ule"
   FP.UNE   -> "une"
   FP.UNO   -> "uno"
   FP.True  -> "true"

instance P.Pretty IP.IntegerPredicate where
  pretty op = case op of
   IP.EQ  -> "eq"
   IP.NE  -> "ne"
   IP.UGT -> "ugt"
   IP.UGE -> "uge"
   IP.ULT -> "ult"
   IP.ULE -> "ule"
   IP.SGT -> "sgt"
   IP.SGE -> "sge"
   IP.SLT -> "slt"
   IP.SLE -> "sle"

-- instance P.Pretty Atomicity where
--   pretty (scope, order) =
--     pretty scope <+> pretty order

ppAtomicity :: Atomicity -> P.Doc ann
ppAtomicity (scope, order) = P.pretty scope <+> P.pretty order

ppMAtomicity :: Maybe Atomicity -> P.Doc ann
ppMAtomicity (Just _m) = "atomic"
ppMAtomicity Nothing = mempty

ppMOrdering :: Maybe Atomicity -> P.Doc ann
ppMOrdering (Just (_scope, order)) = P.pretty order
ppMOrdering Nothing = mempty

instance P.Pretty SynchronizationScope where
  pretty = \case
    SingleThread -> "syncscope(\"singlethread\")"
    System -> mempty

instance P.Pretty MemoryOrdering where
  pretty = \case
    Unordered              -> "unordered"
    Monotonic              -> "monotonic"
    Acquire                -> "acquire"
    Release                -> "release"
    AcquireRelease         -> "acq_rel"
    SequentiallyConsistent -> "seq_cst"

instance P.Pretty RMW.RMWOperation where
  pretty = \case
    RMW.Xchg -> "xchg"
    RMW.Add -> "add"
    RMW.Sub -> "sub"
    RMW.And -> "and"
    RMW.Nand -> "nand"
    RMW.Or -> "or"
    RMW.Xor -> "xor"
    RMW.Max -> "max"
    RMW.Min -> "min"
    RMW.UMax -> "umax"
    RMW.UMin -> "umin"

instance P.Pretty DataLayout where
  pretty x = P.pretty (BL.unpack (dataLayoutToString x))

instance P.Pretty DebugNameTableKind where
  pretty = \case
    NameTableKindDefault -> "Default"
    NameTableKindGNU -> "GNU"
    NameTableKindNone -> "None"

instance P.Pretty FastMathFlags where
  pretty FastMathFlags {..} =
        if allowReassoc then "reassoc" else ""
    <+> if noNaNs then "nnan" else ""
    <+> if noInfs then "ninf" else ""
    <+> if noSignedZeros then "nsz" else ""
    <+> if allowReciprocal then "arcp" else ""
    <+> if allowContract then "contract" else ""
    <+> if approxFunc then "afn" else ""


-- -------------------------------------------------------------------------------
-- -- Special Case Hacks
-- -------------------------------------------------------------------------------

escape :: Char -> P.Doc ann
escape '"'  = P.pretty ("\\22" :: String)
escape '\\' = P.pretty ("\\\\" :: String)
escape c    = if isAscii c && not (isControl c)
              then P.pretty c
              else P.pretty ("\\" :: String) <> hex c
    where
        hex :: Char -> P.Doc ann
        hex = pad0 . ($ []) . showHex . ord
        pad0 :: String -> P.Doc ann
        pad0 [] = "00"
        pad0 [x] = "0" <> P.pretty x
        pad0 xs = P.pretty (pack xs)

ppVolatile :: Bool -> P.Doc ann
ppVolatile True = "volatile"
ppVolatile False = mempty

ppIntAsChar :: Integral a => a -> P.Doc ann
ppIntAsChar = escape . chr . fromIntegral

ppAlign :: Word32 -> P.Doc ann
ppAlign x | x == 0    = mempty
          | otherwise = ", align" <+> P.pretty x

-- print an operand and its type
ppTyped :: (P.Pretty a, Typed a) => a -> P.Doc ann
ppTyped a = P.pretty (typeOf a) <+> P.pretty a

phiIncoming :: (Operand, Name) -> P.Doc ann
phiIncoming (op, nm) = P.brackets (P.pretty op `cma` local' (P.pretty nm))

ppParams :: (a -> P.Doc ann) -> ([a], Bool) -> P.Doc ann
ppParams ppParam (ps, varrg) = P.parens . commas $ fmap ppParam ps ++ vargs
    where
        vargs =  ["..." | varrg] 

ppFunctionArgumentTypes :: Type -> P.Doc ann
ppFunctionArgumentTypes FunctionType {..} = ppParams P.pretty (argumentTypes, isVarArg)
ppFunctionArgumentTypes _ = error "Non-function argument. (Malformed AST)"

ppNullInitializer :: Type -> P.Doc ann
ppNullInitializer PointerType {} = "zeroinitializer"
ppNullInitializer StructureType {} = "zeroinitializer"
ppNullInitializer FunctionType {} = "zeroinitializer"
ppNullInitializer ArrayType {} = "zeroinitializer"
ppNullInitializer _ = error "Non-pointer argument. (Malformed AST)"

ppCall :: Instruction -> P.Doc ann
ppCall Call { function = Right f,..}
  = tail' <+> "call" <+> P.pretty callingConvention <+> ppReturnAttributes returnAttributes <+> P.pretty resultType <+> ftype
    <+> P.pretty f <> P.parens (commas $ fmap ppArguments arguments) <+> ppFunctionAttributes functionAttributes
    where
      (functionType@FunctionType {..}) = case referencedType (typeOf f) of
                                           fty@FunctionType {} -> fty
                                           _ -> error "Calling non function type. (Malformed AST)"
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t

      tail' = case tailCallKind of
        Just Tail -> "tail"
        Just MustTail -> "musttail"
        Just NoTail -> "notail"
        Nothing -> mempty
ppCall Call { function = Left (IA.InlineAssembly {..}), ..}
  = tail' <+> "call" <+> P.pretty callingConvention <+> ppReturnAttributes returnAttributes <+> P.pretty type'
    <+> "asm" <+> sideeffect' <+> align' <+> dialect' <+> P.dquotes (P.pretty (pack (BL.unpack assembly))) <> ","
    <+> P.dquotes (P.pretty constraints) <> P.parens (commas $ fmap ppArguments arguments) <+> ppFunctionAttributes functionAttributes
    where
      tail' = case tailCallKind of
        Just Tail -> "tail"
        Just MustTail -> "musttail"
        Just NoTail -> "notail"
        Nothing -> mempty
      -- If multiple keywords appear the ‘sideeffect‘ keyword must come first,
      -- the ‘alignstack‘ keyword second and the ‘inteldialect‘ keyword last.
      sideeffect' = if hasSideEffects then "sideeffect" else ""
      align' = if alignStack then "alignstack" else ""
      -- ATTDialect is assumed if not specified
      dialect' = case dialect of IA.ATTDialect -> ""; IA.IntelDialect -> "inteldialect"
ppCall _ = error "Non-callable argument. (Malformed AST)"

ppReturnAttributes :: [ParameterAttribute] -> P.Doc ann
ppReturnAttributes pas = P.hsep $ fmap P.pretty pas

-- Differs from Call in record name conventions only so needs a seperate almost
-- identical function. :(
ppInvoke :: Terminator -> P.Doc ann
ppInvoke Invoke { function' = Right f,..}
  = "invoke" <+> P.pretty callingConvention' <+> P.pretty resultType <+> ftype
    <+> P.pretty f <> P.parens (commas $ fmap ppArguments arguments') <+> ppFunctionAttributes functionAttributes'
    where
      functionType@FunctionType {..} =
        case referencedType (typeOf f) of
          fty@FunctionType{} -> fty
          _ -> error "Invoking non-function type. (Malformed AST)"
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t
ppInvoke _ = error "Non-callable argument. (Malformed AST)"

ppSingleBlock :: BasicBlock -> P.Doc ann
ppSingleBlock (BasicBlock _nm instrs term) = P.vcat $ fmap P.pretty instrs ++ [P.pretty term]

-- According to <https://stackoverflow.com/a/7002812/3877993> this is
-- the best way to cast floats to words.

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

specialFP :: RealFloat a => a -> Bool
specialFP f = isNaN f || f == 1 / 0 || f == - 1 / 0

ppInstrMeta :: InstructionMetadata -> P.Doc ann
ppInstrMeta [] = mempty
ppInstrMeta xs = "," <> ppInstructionMetadata xs

ppLayoutOptions :: P.LayoutOptions
ppLayoutOptions = P.LayoutOptions (P.AvailablePerLine 100 0.5)

-- -------------------------------------------------------------------------------
-- -- Toplevel
-- -------------------------------------------------------------------------------

-- | P.Pretty print a LLVM module
ppllvm :: Module -> Text
ppllvm = P.renderLazy . P.layoutPretty ppLayoutOptions . P.pretty

-- | P.Pretty print a printable LLVM expression
ppll :: P.Pretty a => a -> Text
ppll = P.renderLazy . P.layoutPretty ppLayoutOptions . P.pretty

