-- | Error reporting.
module Report
    where

import Data.List
import System.Exit

import Compiler.Pretty
import Compiler.TypeChecking.Error

reportLocation :: Location -> IO ()
reportLocation = mapM_ (putStrLn . (++ "\n") . go)
  where
    go (InExpr e) =
        "In an expression:\n\n  "          ++ prettyExpr e    ""
    go (InDef d) =
        "In a top level definition:\n\n  " ++ prettyVD d      ""
    go (InLocalDef d) =
        "In a local definition:\n\n  "     ++ prettyVD d      ""
    go (InTyCon tc) =
        "In a type constructor:\n\n  "     ++ prettyTyCon tc  ""
    go (InVariant v) =
        "In a data constructor:\n\n  "     ++ prettyCon v     ""
    go InElim =
        "In an eliminator for this data type."
    go (InDataDef dd) =
        "In a data type definition:\n\n  " ++ prettyDD dd     ""
    go (InTypeSig ts) =
        "In a type signature:\n\n  "       ++ prettyTS ts     ""
    go (InType t) =
        "In a type:\n\n  "                 ++ prettyType t    ""
    go (InAssume ts) =
        "In an assumption:\n\n  "          ++ prettyAssume ts ""

reportTCError :: TCError -> IO ()
reportTCError (TCError err loc) = do
    putStrLn (go err ++ "\n")
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
    go (TError (TypeTooGeneral ts1 ts2)) =
        "Declared type is too general.\n" ++
        "Actual type:\n\n  " ++ prettyScheme ts1 "" ++ "\n\n" ++
        "Expected type:\n\n  " ++ prettyScheme ts2 ""

    -- Kind errors.
    go (KError (KindMismatch t k1 k2)) =
        "Kind error in type: " ++ prettyType t "" ++ "\n" ++
        "Actual kind:\n\n  "  ++ kind k1 ++ "\n\n" ++
        "Expected kind:\n\n  " ++ kind k2
      where
        kind n = concat . intersperse " -> " . replicate (n + 1) $ "*"

    -- Unification errors.
    go (UError (FullError ft fu ue)) =
        "Cannot match type\n  " ++ prettyType ft "" ++ "\n" ++
        "with type\n  " ++ prettyType fu "" ++ "\n\n" ++
        go' ue
      where
        go' (OccursCheck v t) =
            "Cannot construct infinite type: " ++ v ++ " = " ++ prettyType t ""
        go' (TyConMismatch t u) =
            "Specially: " ++ prettyType t "" ++ " /= " ++ prettyType u ""

