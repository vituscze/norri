-- | Error reporting.
module Report
    (
      reportTCError
    )
    where

import Data.List
import System.Exit
import System.IO

import Compiler.AST
import Compiler.Pretty
import Compiler.TypeChecking.Error

-- | Print a 'String' to standard error stream.
ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

-- | Report the location of an error.
reportLocation :: Location -> IO ()
reportLocation = mapM_ (ePutStrLn . (++ "\n") . go)
  where
    -- Report one step of the location.
    go (InExpr e) =
        "In an expression:\n\n  " ++ runP (prettyExpr e)
    go (InDef k d) =
        "In a " ++ k' ++ " definition:\n\n  " ++ runP (prettyVD d)
      where
        k' = case k of
            Local    -> "local"
            TopLevel -> "top level"
    go (InTyCon tc) =
        "In a type constructor:\n\n  " ++ runP (prettyTyCon tc)
    go (InVariant v) =
        "In a data constructor:\n\n  " ++ runP (prettyCon v)
    go InElim =
        "In an eliminator for this data type."
    go (InDataDef dd) =
        "In a data type definition:\n\n  " ++ runP (prettyDD dd)
    go (InTypeSig ts) =
        "In a type signature:\n\n  " ++ runP (prettyTS ts)
    go (InType t) =
        "In a type:\n\n  " ++ runP (prettyType t)
    go (InAssume ts) =
        "In an assumption:\n\n  " ++ runP (prettyAssume ts)

-- | Check and extract name of the recursive definition if an error happened
--   during type inference of 'Fix'.
checkRecErr :: TCError -> Maybe (Name, Location)
checkRecErr (TCError (UError _) (InExpr (Fix{}):r@(InDef _ (ValueDef n _):_))) =
    return (n, r)
checkRecErr _ =
    Nothing

-- | Report whole error: both its content and its location. Then exit the
--   program with 'exitFailure'.
reportTCError :: TCError -> IO ()
reportTCError e@(TCError err loc) = do
    ePutStrLn (go err ++ "\n")

    -- Let the user know that the error isn't due to some mysterious @fix@
    -- suddenly appearing in the code - it's because the recursive definition
    -- has wrong type.
    loc' <- case checkRecErr e of
        Just (n, loc') -> do
            ePutStrLn $ "When checking the type of value: " ++ n ++ "\n"
            return loc'
        Nothing ->
            return loc
    reportLocation loc'
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
        , runP (prettyScheme ts1)
        , "\n\nExpected type:\n\n  "
        , runP (prettyScheme ts2)
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
