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
prettyDataDef = error "prettyDataDef: TODO"

prettyValDef :: ValueDef -> String
prettyValDef (ValueDef name expr) =
    struct name $ prettyExpr defStruct expr ++
        typedef (innerType defStruct)
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

    go u name (Lam x expr) =
        struct name . struct "type" . template x "apply" . decls $
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
