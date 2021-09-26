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

    BuilderRef,
    builderCreate,
    builderDispose,

    TypeRef,
    typeDouble,

    ValueRef,
    constReal,

    buildFAdd,
    buildFSub,
    buildFMul,
    buildCall,

    RealPredicate(..),

    buildFCmp,
    buildUIToFP,

    functionLookup,
    functionCountParams,
  )
where

-- https://github.com/haskell/c2hs/wiki/Implementation-of-Haskell-Binding-Modules

#include <llvm-c/Core.h>

import Foreign

{#context prefix = "LLVM" #}

guardNull :: Ptr a -> Maybe (Ptr a)
guardNull ptr = if ptr == nullPtr then Nothing else Just ptr

{#pointer LLVMContextRef as ContextRef newtype#}
{#fun LLVMContextCreate as contextCreate {} -> `ContextRef' #}
{#fun LLVMContextDispose as contextDispose {`ContextRef'} -> `()' #}

{#pointer LLVMModuleRef as ModuleRef newtype#}
{#fun LLVMModuleCreateWithNameInContext as moduleCreate {`String', `ContextRef'} -> `ModuleRef' #}
{#fun LLVMDisposeModule as moduleDispose {`ModuleRef'} -> `()' #}

{#pointer LLVMBuilderRef as BuilderRef newtype#}
{#fun LLVMCreateBuilderInContext as builderCreate {`ContextRef'} -> `BuilderRef' #}
{#fun LLVMDisposeBuilder as builderDispose {`BuilderRef'} -> `()' #}

{#pointer LLVMTypeRef as TypeRef newtype#}
{#fun LLVMDoubleTypeInContext as typeDouble {`ContextRef'} -> `TypeRef' #}

{#pointer LLVMValueRef as ValueRef newtype#}

deriving newtype instance Storable ValueRef

guardNullValue :: ValueRef -> Maybe ValueRef
guardNullValue (ValueRef ptr) = ValueRef <$> guardNull ptr

{#fun LLVMConstReal as constReal {`TypeRef', `Double'} -> `ValueRef' #}

{#fun LLVMBuildFAdd as buildFAdd {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}
{#fun LLVMBuildFSub as buildFSub {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}
{#fun LLVMBuildFMul as buildFMul {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}

{#fun LLVMGetNamedFunction as functionLookup {`ModuleRef', `String'} -> `Maybe ValueRef' guardNullValue #}
{#fun LLVMCountParams as functionCountParams {`ValueRef'} -> `Int' #}
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
