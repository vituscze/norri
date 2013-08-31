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

-- | Left opening brace surrounded by newlines.
lbrace :: String
lbrace = "\n{\n"

-- | Right closing brace surrounded by newlines.
rbrace :: String
rbrace = "\n};\n"

-- | Pretty prints a @struct@ given its name and contents.
--
-- >>> putStrLn $ struct "s" "int x;"
-- struct s
-- {
-- int x;
-- };
struct :: String  -- ^ Name of the @struct@.
       -> String  -- ^ Content of the @struct@.
       -> String
struct name content
    = "struct " ++ name ++ lbrace ++ content ++ rbrace

-- | Pretty prints a @template@ given its name, contents and the (only)
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

-- | Pretty prints a @typedef@ which identifies type expression @what@ with
--   @type@.
--
-- >>> putStrLn $ typedef "int"
-- typedef int type;
typedef :: String  -- ^ Type expression.
        -> String
typedef what = "typedef " ++ what ++ " type;"

-- | Pretty prints a type expression extracting inner @type@ from another
--   type.
--
-- >>> putStrLn $ innerType "vector"
-- typename vector::type
innerType :: String  -- ^ Type expression.
          -> String
innerType what = "typename " ++ what ++ "::type"

innerApply :: String  -- ^ Name of the template argument.
           -> String  -- ^ Name of the @struct@.
           -> String  -- ^ Content of the @struct@.
           -> String
innerApply arg name = struct name . struct "type" . template arg "apply"

-- | Pretty prints a list of declarations.
--
--   This is just a name-flavored 'concat'.
decls :: [String]  -- ^ List of declarations.
      ->  String
decls = concat

-- | Pretty prints whole module.
--
--   Prints all top level entities contained in module and separates them
--   by two newlines.
prettyModule :: Module -> String
prettyModule (Module m) = intercalate sep $ map prettyTopLevel m
  where
    sep = "\n\n"

-- | Pretty prings a top level declaration.
prettyTopLevel :: TopLevel -> String
prettyTopLevel tl = case tl of
    Data  dd -> prettyDataDef dd
    Value vd -> prettyValDef vd
    Type  _  -> ""  -- Types are erased.

-- | Pretty prints a type signature.
--
--   Since type signatures have no correspondence in the template C++ code,
--   an empty string is returned.
prettyType :: TypeSig -> String
prettyType _ = ""

prettyDataDef :: DataDef -> String
prettyDataDef (DataDef (TyCon name _) variants) = decls $
    [ intercalate "\n" $ zipWith defineCtor variants [0 ..]
    , defineElim variants
    ]
  where
    localArg    n = "__ctor_arg"   ++ show n
    localStruct n = "__ctor_local" ++ show n

    ctorStruct    = "__ctor_top_local"
    elimStruct    = "__elim_top_local"

    defineCtor (DataCon cname ts) n =
        struct cname . decls $
            [ go 0 ctorStruct [] ts
            , typedef $ innerType ctorStruct
            ]
      where
        go _ name args [] = struct name . typedef . concat $
            [ "__data<"
            , show n
            , ", dummy"
            , concatMap (", " ++) (reverse args)
            , ">"
            ]

        go u name args (_:ts) = innerApply localA name . decls $
                [ go (u + 1) localS (localA:args) ts
                , typedef $ innerType localS
                ]
          where
            localA = localArg u
            localS = localStruct u

    defineElim vs = struct (firstToLower name) . decls $
        [ go 0 elimStruct [] vs
        , typedef $ innerType elimStruct
        ]
      where
        firstToLower []     = []
        firstToLower (c:cs) = toLower c:cs

        go _ name args [] =
            struct name . struct "type" . decls . intersperse "\n" $
                "template <typename>\nstruct apply_alt;\n"  -- Forward declaration.
              : zipWith3 handleCase vs (reverse args) [0 ..]
             ++ [ template "__t" "apply" $
                      typedef "typename apply_alt<typename __t::type>::type" -- TODO: make this nicer
                ]

        go u name args (_:vs) = innerApply localA name . decls $
            [ go (u + 1) localS (localA:args) vs
            , typedef $ innerType localS
            ]
          where
            localA = localArg u
            localS = localStruct u

        handleCase (DataCon _ ts) arg n = concat $
            [ "template <typename dummy"
            , concatMap (", typename " ++) args
            , ">\nstruct apply_alt<__data<"
            , show n
            , ", dummy"
            , concatMap (", " ++) args
            , "> >\n{\n"
            , decls $
                [ prettyExpr localS . foldl1 App . map Var $ arg:args
                , typedef $ innerType localS
                ]
            , "\n};\n"
            ]

          where
            args = zipWith (\_ n -> fieldA ++ show n) ts [0 ..]

            fieldA = "__field_arg"
            localS = "__local_case"



prettyValDef :: ValueDef -> String
prettyValDef (ValueDef name expr) =
    struct name . decls $
        [ prettyExpr defStruct expr
        , typedef . innerType $ defStruct
        ]
  where
    defStruct = "__def"

prettyExpr :: String  -- ^ Name of the @struct@.
           -> Expr
           -> String
prettyExpr = go (0 :: Int)
  where
    localStruct n = "__local" ++ show n
    leftStruct  n = "__left"  ++ show n
    rightStruct n = "__right" ++ show n

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
            map prettyValDef dec ++
            [ go (u + 1) local expr
            , typedef $ innerType local
            ]
      where
        local = localStruct u

    go u name (SetType expr _) =
        go u name expr

    go u name (NumLit n) =
        struct name . typedef $ "Int<" ++ show n ++ ">"
