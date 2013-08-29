module Compiler.Pretty
    (

    )
    where

import Data.List

import Compiler.AST

lbrace :: String
lbrace = "\n{\n"

rbrace :: String
rbrace = "\n};\n"

struct :: String  -- ^ Name of the @struct@.
       -> String  -- ^ Content of the @struct@.
       -> String
struct name content = "struct " ++ name ++ lbrace ++ content ++ rbrace

prettyModule :: Module -> String
prettyModule (Module m) = intercalate sep $ map prettyTopLevel m
  where
    sep = "\n\n"

prettyTopLevel :: TopLevel -> String
prettyTopLevel tl = case tl of
    Data  dd -> prettyDataDef dd
    Value vd -> prettyValDef vd
    Type  ts -> ""  -- Types are erased.

prettyDataDef :: DataDef -> String
prettyDataDef = error "prettyDataDef: TODO"

prettyValDef :: ValueDef -> String
prettyValDef (ValueDef name expr) =
    struct name $ prettyExpr "__def" expr ++
        "typedef typename __def::type type;"

prettyExpr :: String  -- ^ Name of the @struct@.
           -> Expr
           -> String
prettyExpr = go 0
  where
    go u sname (Var v) =
        struct sname $ "typedef typename " ++ v ++ "::type type;"
    go u sname (Lam x expr) =
        struct sname . struct "type" $
            "template <typename " ++ x ++ ">\n" ++ (struct "apply" $
                go (u + 1) ("__local" ++ show u) expr ++
                "typedef typename __local" ++ show u ++ "::type type;")
    go u sname (App e1 e2) =
        struct sname $
            go (u + 1) ("__left" ++ show u) e1 ++
            go (u + 1) ("__right" ++ show u) e2 ++
            "typedef typename __left" ++ show u
                ++ "::type::template apply<__right"
                ++ show u ++ ">::type type;"
    go u sname (Let decls expr) = error "prettyExpr: TODO"

