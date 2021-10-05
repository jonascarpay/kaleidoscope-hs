{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.Bindings
  ( ContextRef,
    contextCreate,
    contextDispose,

    ModuleRef,
    moduleCreate,
    moduleDispose,
    moduleDump,

    BuilderRef,
    builderCreate,
    builderDispose,
    builderSetInsertPoint,

    TypeRef,
    typeDouble,
    typeFunction,

    ValueRef,
    constReal,
    valueSetName,
    valueDump,

    buildFAdd,
    buildFSub,
    buildFMul,
    buildRetVoid,
    buildRet,
    buildCall,

    RealPredicate(..),

    buildFCmp,
    buildUIToFP,

    functionLookup,
    functionAdd,
    functionCountBasicBlocks,
    functionCountParams,
    functionGetParam,
    functionVerify,

    VerifierFailureAction (..),

    Linkage(..),
    linkageSet,

    BasicBlockRef,
    basicBlockAppend,

    PassManagerRef,
    fpmCreate,
    fpmInitialize,
    fpmDispose,
    fpmRun,
    fpmAddInstructionCombining,
    fpmAddReassociate,
    fpmAddGVN,
    fpmAddCFGSimplification,
  )
where

-- https://github.com/haskell/c2hs/wiki/Implementation-of-Haskell-Binding-Modules

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/InstCombine.h>
#include <llvm-c/Transforms/Scalar.h>

import Foreign
import Foreign.C

{#context prefix = "LLVM" #}

guardNull :: Ptr a -> Maybe (Ptr a)
guardNull ptr = if ptr == nullPtr then Nothing else Just ptr

{-# INLINE withArrayLen' #-}
withArrayLen' :: Storable a => [a] -> ( (Ptr a, CUInt) -> IO b ) -> IO b
withArrayLen' as k = withArrayLen as $ \len ptr -> k (ptr, fromIntegral len)

withCStringLen' :: String -> ( (Ptr CChar, CULong) -> IO a ) -> IO a
withCStringLen' s k = withCStringLen s $ \ (ptr, len) -> k (ptr, (fromIntegral len))

{#pointer LLVMContextRef as ContextRef newtype#}
{#fun LLVMContextCreate as contextCreate {} -> `ContextRef' #}
{#fun LLVMContextDispose as contextDispose {`ContextRef'} -> `()' #}

{#pointer LLVMModuleRef as ModuleRef newtype#}
{#fun LLVMModuleCreateWithNameInContext as moduleCreate {`String', `ContextRef'} -> `ModuleRef' #}
{#fun LLVMDisposeModule as moduleDispose {`ModuleRef'} -> `()' #}
{#fun LLVMDumpModule as moduleDump {`ModuleRef'} -> `()' #}

{#pointer LLVMBuilderRef as BuilderRef newtype#}
{#fun LLVMCreateBuilderInContext as builderCreate {`ContextRef'} -> `BuilderRef' #}
{#fun LLVMDisposeBuilder as builderDispose {`BuilderRef'} -> `()' #}

{#pointer LLVMTypeRef as TypeRef newtype#}
deriving newtype instance Storable TypeRef

{#fun LLVMDoubleTypeInContext as typeDouble {`ContextRef'} -> `TypeRef' #}
-- TODO there is apparently no FunctionTypeInContext, so maybe we shouldn't bother getting the Double from the context either.
-- The tutorial does get the doubles from a specified context though.
{#fun LLVMFunctionType as typeFunction
  { `TypeRef'
  , withArrayLen'* `[TypeRef]'&
  , `Bool'
  } -> `TypeRef' #}

{#pointer LLVMValueRef as ValueRef newtype#}
deriving newtype instance Storable ValueRef

guardNullValue :: ValueRef -> Maybe ValueRef
guardNullValue (ValueRef ptr) = ValueRef <$> guardNull ptr

{#fun LLVMConstReal as constReal {`TypeRef', `Double'} -> `ValueRef' #}
{#fun LLVMSetValueName2 as valueSetName
  { `ValueRef'
  , withCStringLen'* `String'&
  } -> `()'
#}
{#fun LLVMDumpValue as valueDump {`ValueRef'} -> `()' #}

{#fun LLVMBuildFAdd as buildFAdd {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}
{#fun LLVMBuildFSub as buildFSub {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}
{#fun LLVMBuildFMul as buildFMul {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}
{#fun LLVMBuildRetVoid as buildRetVoid {`BuilderRef'} -> `ValueRef' #}
{#fun LLVMBuildRet as buildRet {`BuilderRef', `ValueRef'} -> `ValueRef' #}

{#fun LLVMGetNamedFunction as functionLookup {`ModuleRef', `String'} -> `Maybe ValueRef' guardNullValue #}
{#fun LLVMCountParams as functionCountParams {`ValueRef'} -> `Int' #}
{#fun LLVMCountBasicBlocks as functionCountBasicBlocks {`ValueRef'} -> `Int' #}
{#fun LLVMGetParam as functionGetParam {`ValueRef', `Int'} -> `ValueRef' #}
{#fun LLVMAddFunction as functionAdd {`ModuleRef', `String', `TypeRef'} -> `ValueRef' #}

{#fun LLVMBuildCall2 as buildCall
  { `BuilderRef'
  , `TypeRef'
  , `ValueRef'
  , withArray* `[ValueRef]'
  , `Int'
  , `String'
  } -> `ValueRef'
#}

{#enum LLVMRealPredicate as RealPredicate {upcaseFirstLetter} deriving (Show) #}
{#fun LLVMBuildFCmp as buildFCmp {`BuilderRef', `RealPredicate', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}

{#fun LLVMBuildUIToFP as buildUIToFP {`BuilderRef', `ValueRef', `TypeRef', `String'} -> `ValueRef' #}

{#enum LLVMLinkage as Linkage {upcaseFirstLetter} deriving (Show) #}
{#fun LLVMSetLinkage as linkageSet {`ValueRef', `Linkage'} -> `()' #}

{#pointer LLVMBasicBlockRef as BasicBlockRef newtype#}
{#fun LLVMAppendBasicBlockInContext as basicBlockAppend
  { `ContextRef', `ValueRef', `String' } -> `BasicBlockRef' #}
{#fun LLVMPositionBuilderAtEnd as builderSetInsertPoint
  { `BuilderRef', `BasicBlockRef' } -> `()' #}

-- toLLVMBool :: Bool -> CInt
-- toLLVMBool =  fromIntegral . fromEnum

fromLLVMBool :: CInt -> Bool
fromLLVMBool = toEnum . fromIntegral

{# enum LLVMVerifierFailureAction as VerifierFailureAction {upcaseFirstLetter} deriving (Show) #}

{# fun LLVMVerifyFunction as functionVerify
  { `ValueRef', `VerifierFailureAction' } -> `Bool' fromLLVMBool
#}

{# pointer LLVMPassManagerRef as PassManagerRef newtype #}
{# fun LLVMCreateFunctionPassManagerForModule as fpmCreate { `ModuleRef' } -> `PassManagerRef' #}
{# fun LLVMDisposePassManager as fpmDispose { `PassManagerRef' } -> `()' #}
{# fun LLVMInitializeFunctionPassManager as fpmInitialize { `PassManagerRef' } -> `Bool' fromLLVMBool #}
{# fun LLVMRunFunctionPassManager as fpmRun { `PassManagerRef', `ValueRef' } -> `Bool' fromLLVMBool #}

{# fun LLVMAddInstructionCombiningPass as fpmAddInstructionCombining { `PassManagerRef' } -> `()' #}
{# fun LLVMAddReassociatePass as fpmAddReassociate { `PassManagerRef' } -> `()' #}
-- GVN: https://en.wikipedia.org/wiki/Value_numbering
{# fun LLVMAddGVNPass as fpmAddGVN { `PassManagerRef' } -> `()' #}
{# fun LLVMAddCFGSimplificationPass as fpmAddCFGSimplification { `PassManagerRef' } -> `()' #}
