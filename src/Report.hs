-- | Error reporting.
module Report
    (
    -- * Type checking and/or inference errors.
      reportTCError

    -- * Parse errors.
    , reportParseError
    )
    where

import Data.List
import System.Exit
import System.IO
import Text.Parsec.Error
import Text.Parsec.Pos

import Compiler.AST
import Compiler.Pretty
import Compiler.TypeChecking.Error

-- | Print a 'String' to standard error stream.
ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

-- | Report the location of an error.
--
--   Each step of the location is separated by an empty line.
--
-- >>> reportLocation [InType (TyVar "a"), InAssume (Sig "x" (...))]
-- In type:
--
--   a
--
-- In assumption:
--
--   assume x : a
--
reportLocation :: Location -> IO ()
reportLocation = mapM_ (ePutStrLn . (++ "\n") . go)
  where
    -- Report one step of the location.
    go (InExpr e) = goE e
    go (InDef k d) =
        "In " ++ k' ++ " definition:\n\n  " ++ runP (prettyVD d)
      where
        k' = case k of
            Local    -> "local"
            TopLevel -> "top level"
    go (InTyCon tc) =
        "In type constructor:\n\n  " ++ runP (prettyTyCon tc)
    go (InVariant v) =
        "In data constructor:\n\n  " ++ runP (prettyCon v)
    go InElim =
        "In eliminator for this data type."
    go (InDataDef dd) =
        "In data type definition:\n\n  " ++ runP (prettyDD dd)
    go (InTypeSig ts) =
        "In type signature:\n\n  " ++ runP (prettyTS ts)
    go (InType t) =
        "In type:\n\n  " ++ runP (prettyType t)
    go (InAssume ts) =
        "In assumption:\n\n  " ++ runP (prettyAssume ts)

    -- Report an expression.
    goE (Fix x _) = "When checking the type of recursive value: " ++ x
    goE (App e1 e2) = concat
        [ "When applying:\n  "
        , runP (prettyExpr e1)
        , "\nto:\n  "
        , runP (prettyExpr e2)
        ]
    goE e = "In expression:\n\n  " ++ runP (prettyExpr e)

-- | Report the whole error: both its content and its location. Then exit the
--   program with 'exitFailure'.
reportTCError :: TCError -> IO a
reportTCError (TCError err loc) = do
    ePutStrLn $ go err ++ "\n"
    reportLocation loc
    exitFailure
  where
    -- Scope errors.
    go (SError (UnboundVariable n)) =
        "Varible not in scope: " ++ n
    go (SError (UndefinedType n)) =
        "Type not in scope: " ++ n
    go (SError (ValueRedefined n)) =
        "Multiple definitions of value: " ++ n
    go (SError (TypeRedefined n)) =
        "Multiple definitions of type: " ++ n
    go (SError (TypeSigRedefined n)) =
        "Multiple type signatures for value: " ++ n
    go (SError (TypeSigTooLate n)) =
        "Type signature after definition of value: " ++ n
    go (SError VarsNotUnique) =
        "Type variables are not unique."

    -- Type errors.
    go (TError (TypeTooGeneral ts1 ts2)) = concat
        [ "Declared type is too general.\nActual type:\n\n  "
        , runP (prettyScheme ts2)
        , "\n\nExpected type:\n\n  "
        , runP (prettyScheme ts1)
        ]

    -- Kind errors.
    go (KError (KindMismatch t k1 k2)) = concat
        [ "Kind error in type: "
        , runP (prettyType t)
        , "\nActual kind:\n\n  "
        , kind k1
        , "\n\nExpected kind:\n\n  "
        , kind k2
        ]
      where
        kind n = intercalate " -> " $ replicate (n + 1) "*"

    -- Unification errors.
    go (UError (FullError ft fu ue)) = concat
        [ "Cannot match type\n  "
        , runP (prettyType ft)
        , "\nwith type\n  "
        , runP (prettyType fu)
        , "\n\n"
        , go' ue
        ]
      where
        go' (OccursCheck v t) = concat
            [ "Cannot construct infinite type: "
            , v
            , " = "
            , runP (prettyType t)
            ]
        go' (TyConMismatch t u) = concat
            [ "Specifically: "
            , runP (prettyType t)
            , " /= "
            , runP (prettyType u)
            ]

-- | Report a parse error, then exit the program with 'exitFailure'.
--
--   Based on 'Show' instance for 'ParseError'.
reportParseError :: ParseError -> IO a
reportParseError err = do
    let pos = errorPos err
        row = sourceLine pos
        col = sourceColumn pos

        msg = errorMessages err
    ePutStrLn . concat $
        [ "Line "
        , show row
        , ", column "
        , show col
        , ":"
        ]
    ePutStrLn $ showErrorMessages
        "or" "unknown parse error" "expecting" "unexpected" "end of input" msg
    exitFailure
