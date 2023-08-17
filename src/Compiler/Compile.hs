-- | Allows the abstract syntax tree to be compiled into C++ template
--   metaprogram.
module Compiler.Compile
    (
    -- * Module compiling
      compileModule
    , compileTopLevel

    -- * Top level entities compiling
    , compileType
    , compileDataDef
    , compileValDef

    -- * Expression compiling
    , compileExpr
    )
    where

import Data.List

import Compiler.AST
import Utility

-- | Left opening brace surrounded by newlines.
lbrace :: String
lbrace = "\n{\n"

-- | Right closing brace surrounded by newlines.
rbrace :: String
rbrace = "\n};\n"

-- | Print a @struct@ given its name and contents.
--
-- >>> putStrLn $ struct "s" "int x;"
-- struct s
-- {
-- int x;
-- };
struct :: String  -- ^ Name of the @struct@.
       -> String  -- ^ Content of the @struct@.
       -> String
struct name content = concat ["struct ", name, lbrace, content, rbrace]

-- | Print a @template@ given its name, contents and template
--   parameters.
--
-- >>> putStrLn $ template ["x"] "s" "int y;"
-- template <typename x>
-- struct s
-- {
-- int y;
-- };
template :: [String]  -- ^ Names of the template parameters.
         -> String    -- ^ Name of the @struct@.
         -> String    -- ^ Content of the @struct@.
         -> String
template params name content
    = "template <" ++ intercalate ", " (map ("typename " ++) params) ++ ">\n" ++ struct name content

-- | Print a @template@ @struct@ forward declaration.
--
-- >>> putStrLn $ fwdTemplate 1 "s"
-- template <typename>
-- struct s;
fwdTemplate :: Int     -- ^ Number of template parameters.
            -> String  -- ^ Name of the @struct@.
            -> String
fwdTemplate n name
    = "template <" ++ intercalate ", " (replicate n "typename") ++ ">\nstruct " ++ name ++ ";\n"

-- | Print a @using@ declaration which identifies type expression @what@ with @type@.
--
-- >>> putStrLn $ using "int"
-- using type = int;
using :: String  -- ^ Type expression.
      -> String
using what = "using " ++ ty ++ " = " ++ what ++ ";"

-- | Print a type expression extracting inner @type@ from another type.
--
-- >>> putStrLn $ innerType "vector"
-- typename vector::type
innerType :: String  -- ^ Type expression.
          -> String
innerType what = "typename " ++ what ++ "::" ++ ty

-- | Print a nested hierarchy of @struct@ures used for lambda abstraction.
--
-- >>> putStrLn $ innerApply "x" "int x;"
-- struct type
-- {
-- template <typename x>
-- struct app
-- {
-- int x;
-- };
-- };
innerApply :: String  -- ^ Name of the template parameter.
           -> String  -- ^ Content of the @struct@.
           -> String
innerApply param = struct ty . template [param] apply

-- | Print a list of declarations.
--
--   This is just a name-flavored 'concat'.
decls :: [String]  -- ^ List of declarations.
      ->  String
decls = concat

-- | 'String' constant for inner @template@ used for lambda abstractions.
apply :: String
apply = "app"

-- | 'String' constant for inner @using@s.
ty :: String
ty = "type"

-- | Compile whole module.
--
--   Compiles all top level entities contained in module.
compileModule :: Module -> String
compileModule (Module m) = intercalate sep $ map compileTopLevel m
  where
    sep = "\n\n"

-- | Compile a top level declaration.
compileTopLevel :: TopLevel -> String
compileTopLevel tl = case tl of
    Data  dd -> compileDataDef dd
    Value vd -> compileValDef vd
    Type   _ -> ""  -- Types are erased.
    Assume _ -> ""  -- So are type assumptions.

-- | Compile a type signature.
--
--   Since type signatures have no correspondence in the template C++ code,
--   no C++ code is produced.
compileType :: TypeSig -> String
compileType _ = ""

-- | Compile a data definition.
--
--   Note that since this language doesn't allow pattern matching, this
--   function will automatically define an appropriate eliminator for the
--   data type.
compileDataDef :: DataDef -> String
compileDataDef (DataDef (TyCon tyConName _) variants) = decls
    [ intercalate sep $ zipWith defineCtor variants [0 ..]
    , defineElim variants
    ]
  where
    sep = "\n\n"

    elimStruct     = "__elim"
    primDataStruct = "__data"
    localParam n   = "__param" ++ show n

    -- Compile a single data constructor.
    defineCtor :: Variant  -- ^ Data constructor.
               -> Int      -- ^ Numeric suffix for @struct@s.
               -> String
    defineCtor (DataCon cname ts) n = struct cname $ go 0 [] ts
      where
        go :: Int -> [String] -> [Type] -> String
        go _ params [] = using . concat $
            [ primDataStruct
            , "<"
            , show n
            , concatMap ((", " ++) . innerType) (reverse params)
            , ">"
            ]

        go u params (_:rest) = innerApply local $ go (u + 1) (local:params) rest
          where
            local = localParam u

    -- Compile an eliminator for the whole data type.
    defineElim :: [Variant] -> String
    defineElim vs = struct (uncap tyConName) $ go 0 [] vs
      where
        go :: Int        -- ^ Numeric suffix for @struct@s.
           -> [String]   -- ^ Names of eliminator parameters.
           -> [Variant]  -- ^ Data constructors.
           -> String
        go _ params [] =
            decls . intersperse "\n" $
                [ fwdTemplate 2 elimStruct
                ]
             ++   zipWith3 handleCase vs (reverse params) [0 ..]
             ++ [ innerApply elimParam . using . innerType . concat $
                    [ elimStruct
                    , "<void, "
                    , innerType elimParam
                    , ">"
                    ]
                ]
          where
            elimParam  = "__elim_input"

        go u params (_:rest) = innerApply local $ go (u + 1) (local:params) rest
          where
            local = localParam u

        -- Compile a @template@ specialization which deconstructs @n@-th
        -- constructor and applies the corresponding elimination function to
        -- all its fields.
        handleCase :: Variant  -- ^ 'Variant' to be compiled.
                   -> String   -- ^ Parameter name.
                   -> Int      -- ^ Parameter position.
                   -> String
        handleCase (DataCon _ ts) param n = concat
            [ "template <typename "
            , dummy
            , concatMap (", typename " ++) params
            , ">\nstruct "
            , elimStruct
            , "<"
            , dummy
            , ", "
            , primDataStruct
            , "<"
            , show n
            , concatMap (", " ++) params
            , ">>"
            , lbrace
            , decls $
                  wrapFields
             ++ [ compileExpr . foldl1 App . map Var $
                      param:map (extra ++) params
                ]
            , rbrace
            ]
          where
            -- Names of all constructor fields.
            params = zipWith (const $ (field ++) . show) ts [0 :: Int ..]

            -- Fields need to be wrapped so that we may refer to their @type@ member.
            wrapFields = map wrapStruct params
              where
                wrapStruct name = struct (extra ++ name) $ using name

            dummy = "__dummy"
            field = "__field_param"
            extra = "__extra"


-- | Compile a value definition.
compileValDef :: ValueDef -> String
compileValDef (ValueDef name expr) = struct name $ compileExpr expr

-- | Compile an expression.
compileExpr :: Expr
            -> String
compileExpr = go 0
  where
    leftStruct  n = "__left"  ++ show n
    rightStruct n = "__right" ++ show n

    go :: Int     -- ^ Numeric suffix for @struct@s.
       -> Expr    -- ^ Expression to be compiled.
       -> String
    go _ (Var v) = using . innerType $ v

    go u (Lam x expr) = innerApply x $ go (u + 1) expr

    go u (App e1 e2) =
        decls $
            [ struct left  $ go (u + 1) e1
            , struct right $ go (u + 1) e2
            , using . innerType . concat $
                [ left
                , "::"
                , ty
                , "::template "
                , apply
                , "<"
                , right
                , ">"
                ]
            ]
      where
        left  = leftStruct  u
        right = rightStruct u

    go u (Let dec expr) = decls $
        map compileValDef dec ++ [go (u + 1) expr]

    go u (SetType expr _) = go u expr

    go _ (NumLit n) =
        using $ "Int<" ++ show n ++ ">"

    go _ (BoolLit b) =
        using $ "Bool<" ++ uncap (show b) ++ ">"

    -- Fixed point operator is transformed into language primitive
    -- "fix" and an abstraction.
    go u (Fix x expr) = go u (App (Var "fix") (Lam x expr))
