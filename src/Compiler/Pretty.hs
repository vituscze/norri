-- | Allows the abstract syntax tree to be pretty printed as a template
--   C++ code.
--
--   Future work: abstract away from the direct 'String' handling;
--   abstract representation of the structure of pretty printed C++
--   code would allow for more flexibility and type safety.
module Compiler.Pretty
    (
    -- * Module pretty printing
      prettyModule
    , prettyTopLevel

    -- * Top level entities pretty printing
    , prettyType
    , prettyDataDef
    , prettyValDef

    -- * Expression pretty printing
    , prettyExpr
    )
    where

import Data.Char
import Data.List

import Compiler.AST
import Utility

-- | Left opening brace surrounded by newlines.
lbrace :: String
lbrace = "\n{\n"

-- | Right closing brace surrounded by newlines.
rbrace :: String
rbrace = "\n};\n"

-- | Pretty print a @struct@ given its name and contents.
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

-- | Pretty print a @template@ given its name, contents and the (only)
--   template argument.
--
-- >>> putStrLn $ template "x" "s" "int y;"
-- template <typename x>
-- struct s
-- {
-- int y;
-- };
template :: String  -- ^ Name of the template argument.
         -> String  -- ^ Name of the @struct@.
         -> String  -- ^ Content of the @struct@.
         -> String
template arg name content
    = "template <typename " ++ arg ++ ">\n" ++ struct name content

-- | Pretty print a @template@ @struct@ forward declaration.
--
-- >>> putStrLn $ fwdTemplate "s"
-- template <typename>
-- struct s;
fwdTemplate :: String  -- ^ Name of the @struct@.
            -> String
fwdTemplate name = "template <typename>\nstruct " ++ name ++ ";\n"

-- | Pretty print a @typedef@ which identifies type expression @what@ with
--   @type@.
--
-- >>> putStrLn $ typedef "int"
-- typedef int type;
typedef :: String  -- ^ Type expression.
        -> String
typedef what = "typedef " ++ what ++ " " ++ ty ++ ";"

-- | Pretty print a type expression extracting inner @type@ from another
--   type.
--
-- >>> putStrLn $ innerType "vector"
-- typename vector::type
innerType :: String  -- ^ Type expression.
          -> String
innerType what = "typename " ++ what ++ "::" ++ ty

-- | Pretty print a nested hierarchy of @struct@ures used for
--   lambda abstraction.
--
-- >>> putStrLn $ innerApply "x" "s" "int x;"
-- struct s
-- {
-- struct type
-- {
-- template <typename x>
-- struct apply
-- {
-- int x;
-- };
-- };
-- };
innerApply :: String  -- ^ Name of the template argument.
           -> String  -- ^ Name of the @struct@.
           -> String  -- ^ Content of the @struct@.
           -> String
innerApply arg name = struct name . struct ty . template arg apply

-- | Pretty print a list of declarations.
--
--   This is just a name-flavored 'concat'.
decls :: [String]  -- ^ List of declarations.
      ->  String
decls = concat

-- | 'String' constant for inner @template@ used for lambda abstractions.
apply :: String
apply = "apply"

-- | 'String' constant for inner @typedef@s.
ty :: String
ty = "type"

-- | 'String' constant for the @dummy@ type.
dummy :: String
dummy = "dummy"

-- | Pretty print whole module.
--
--   Prints all top level entities contained in module and separates them
--   by two newlines.
prettyModule :: Module -> String
prettyModule (Module m) = intercalate sep $ map prettyTopLevel m
  where
    sep = "\n\n"

-- | Pretty pring a top level declaration.
prettyTopLevel :: TopLevel -> String
prettyTopLevel tl = case tl of
    Data  dd -> prettyDataDef dd
    Value vd -> prettyValDef vd
    Type  _  -> ""  -- Types are erased.

-- | Pretty print a type signature.
--
--   Since type signatures have no correspondence in the template C++ code,
--   an empty string is returned.
prettyType :: TypeSig -> String
prettyType _ = ""

-- | Pretty print a data definition.
--
--   Note that since this language doesn't allow pattern matching, this
--   function will automatically define an appropriate eliminator for the
--   data type.
prettyDataDef :: DataDef -> String
prettyDataDef (DataDef (TyCon tyConName _) variants) = decls
    [ intercalate sep $ zipWith defineCtor variants [0 ..]
    , defineElim variants
    ]
  where
    sep = "\n\n"

    localArg    n  = "__ctor_arg"   ++ show n
    localStruct n  = "__ctor_local" ++ show n

    ctorStruct     = "__ctor_top_local"
    elimStruct     = "__elim_top_local"

    primDataStruct = "__data"

    applyAlt       = "apply_alt"

    -- Pretty print a single data constructor.
    defineCtor :: Variant  -- ^ Data constructor.
               -> Int      -- ^ Numeric suffix for @struct@s.
               -> String
    defineCtor (DataCon cname ts) n = struct cname . decls $
        [ go 0 ctorStruct [] ts
        , typedef $ innerType ctorStruct
        ]
      where
        go :: Int -> String -> [String] -> [Type] -> String
        go _ name args [] = struct name . typedef . concat $
            [ primDataStruct
            , "<"
            , show n
            , ", "
            , dummy
            , concatMap ((", " ++) . innerType) (reverse args)
            , ">"
            ]

        go u name args (_:rest) = innerApply localA name . decls $
                [ go (u + 1) localS (localA:args) rest
                , typedef $ innerType localS
                ]
          where
            localA = localArg u
            localS = localStruct u

    -- Pretty print an eliminator for the whole data type.
    defineElim :: [Variant] -> String
    defineElim vs = struct (firstToLower tyConName) . decls $
        [ go 0 elimStruct [] vs
        , typedef $ innerType elimStruct
        ]
      where
        firstToLower []     = []
        firstToLower (c:cs) = toLower c:cs

        go :: Int        -- ^ Numeric suffix for @struct@s.
           -> String     -- ^ Outer @struct@ name.
           -> [String]   -- ^ Names of eliminator arguments.
           -> [Variant]  -- ^ Data constructors.
           -> String
        go _ name args [] =
            struct name . struct ty . decls . intersperse "\n" $
                [ fwdTemplate applyAlt
                ]
             ++   zipWith3 handleCase vs (reverse args) [0 ..]
             ++ [ template typeArg apply . typedef . innerType . concat $
                    [ applyAlt
                    , "<"
                    , innerType typeArg
                    , ">"
                    ]
                ]
          where
            typeArg  = "__type_arg"

        go u name args (_:rest) = innerApply localA name . decls $
            [ go (u + 1) localS (localA:args) rest
            , typedef $ innerType localS
            ]
          where
            localA = localArg u
            localS = localStruct u

        -- Pretty print a @template@ specialization which deconstructs
        -- @n@-th constructor and applies the corresponding elimination
        -- function to all its fields.
        handleCase :: Variant  -- ^ 'Variant' to be pretty printed.
                   -> String   -- ^ Argument name.
                   -> Int      -- ^ Argument position.
                   -> String
        handleCase (DataCon _ ts) arg n = concat
            [ "template <typename "
            , dummy
            , concatMap (", typename " ++) args
            , ">\nstruct "
            , applyAlt
            , "<"
            , primDataStruct
            , "<"
            , show n
            , ", "
            , dummy
            , concatMap (", " ++) args
            , "> >"
            , lbrace
            , decls $
                  wrapFields
             ++ [ prettyExpr localS . foldl1 App . map Var $
                      arg:map (extra ++) args
                , typedef $ innerType localS
                ]
            , rbrace
            ]
          where
            -- Names of all constructor fields.
            args = zipWith (const $ (fieldA ++) . show) ts [0 :: Int ..]

            -- Create a wrapper @struct@ures so the data type can
            -- contain the values directly rather than just the
            -- expression names.
            --
            -- This would otherwise lead to all kinds of problems with
            -- expressions not being interchangeable even though their
            -- values are.
            wrapFields = map wrapStruct args
              where
                wrapStruct name = struct (extra ++ name) $
                    typedef name

            fieldA = "__field_arg"
            localS = "__local_case"

            -- Prefix for the wrapped structures.
            extra  = "__extra"


-- | Pretty print a value definition.
prettyValDef :: ValueDef -> String
prettyValDef (ValueDef name expr) =
    struct name . decls $
        [ prettyExpr defStruct expr
        , typedef . innerType $ defStruct
        ]
  where
    defStruct = "__def"

-- | Pretty print and expression given a name of @struct@ it should be
--   declared in.
prettyExpr :: String  -- ^ Name of the @struct@.
           -> Expr
           -> String
prettyExpr = go (0 :: Int)
  where
    localStruct n = "__local" ++ show n
    leftStruct  n = "__left"  ++ show n
    rightStruct n = "__right" ++ show n

    go :: Int     -- ^ Numeric suffix for @struct@s.
       -> String  -- ^ Outer @struct@ name.
       -> Expr    -- ^ Expression to be pretty printed.
       -> String
    go _ name (Var v) =
        struct name . typedef . innerType $ v

    go u name (Lam x expr) = innerApply x name . decls $
            [ go (u + 1) local expr
            , typedef $ innerType local
            ]
      where
        local = localStruct u

    go u name (App e1 e2) =
        struct name . decls $
            [ go (u + 1) left  e1
            , go (u + 1) right e2
            , typedef . concat $
                [ "typename "
                , left
                , "::type::template apply<"
                , right
                , ">::type"
                ]
            ]
      where
        left  = leftStruct  u
        right = rightStruct u

    go u name (Let dec expr) =
        struct name . decls $
              map prettyValDef dec
         ++ [ go (u + 1) local expr
            , typedef $ innerType local
            ]
      where
        local = localStruct u

    go u name (SetType expr _) = go u name expr

    go _ name (NumLit n) =
        struct name . typedef $ "Int<" ++ show n ++ ">"

    go _ name (BoolLit b) =
        struct name . typedef $ "Bool<" ++ uncap (show b) ++ ">"

    -- Fixed point operator is transformed into language primitive
    -- "fix" and a lambda abstraction.
    go u name (Fix x expr) = go u name (App (Var "fix") (Lam x expr))
