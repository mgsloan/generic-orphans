{-# LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies #-}
module GHC.Generics.Instances.TH where

import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Lift

-- | Build a set of Generic instances for opaque types
buildOpaque :: Name -> DecsQ
buildOpaque n = do
  let name = nameBase n
      dshadow = mkName $ "D_" ++ name
      cshadow = mkName $ "C_" ++ name

  mname <- case nameModule n of
    (Just m) -> return m
    Nothing -> report False "No Module in Name..." >> return ""

  instDecls <- [d|
    instance Datatype $(conT dshadow) where
      datatypeName _ = $(lift name)
      moduleName   _ = $(lift mname)

    instance Constructor $(conT cshadow) where
      conName _ = "" -- JPM: I'm not sure this is the right implementation...
   |]

  repType <- [t| D1 $(conT dshadow) (C1 $(conT cshadow) (S1 NoSelector (Rec0 $(conT n)))) |]

  [(FunD _ fromClauses), (FunD _ toClauses)] <- [d|
    from x = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = x
   |]

  return $
    [ DataD [] dshadow [] [] []
    , DataD [] cshadow [] [] []
  -- This really should be quote-able, but TH is kinda stupid about type families.
    , InstanceD [] (AppT (ConT ''Generic) (ConT n))
      [ TySynInstD (''Rep) [ConT n] repType
      , FunD 'from fromClauses
      , FunD 'to toClauses
      ]
    ] ++ instDecls