module LLVM.Bindings
  ( ContextRef,
    contextCreate,
    contextDispose,

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

    RealPredicate(..),

    buildFCmp,
    buildUIToFP,
  )
where

#include <llvm-c/Core.h>

{#context prefix = "LLVM" #}


{#pointer LLVMContextRef as ContextRef newtype#}
{#fun LLVMContextCreate as contextCreate {} -> `ContextRef' #}
{#fun LLVMContextDispose as contextDispose {`ContextRef'} -> `()' #}

{#pointer LLVMBuilderRef as BuilderRef newtype#}
{#fun LLVMCreateBuilderInContext as builderCreate {`ContextRef'} -> `BuilderRef' #}
{#fun LLVMDisposeBuilder as builderDispose {`BuilderRef'} -> `()' #}

{#pointer LLVMTypeRef as TypeRef newtype#}
{#fun LLVMDoubleTypeInContext as typeDouble {`ContextRef'} -> `TypeRef' #}

{#pointer LLVMValueRef as ValueRef newtype#}
{#fun LLVMConstReal as constReal {`TypeRef', `Double'} -> `ValueRef' #}

{#fun LLVMBuildFAdd as buildFAdd {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}
{#fun LLVMBuildFSub as buildFSub {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}
{#fun LLVMBuildFMul as buildFMul {`BuilderRef', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}

{#enum LLVMRealPredicate as RealPredicate {upcaseFirstLetter} deriving (Show) #}
{#fun LLVMBuildFCmp as buildFCmp {`BuilderRef', `RealPredicate', `ValueRef', `ValueRef', `String'} -> `ValueRef' #}

{#fun LLVMBuildUIToFP as buildUIToFP {`BuilderRef', `ValueRef', `TypeRef', `String'} -> `ValueRef' #}
