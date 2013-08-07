{-# LANGUAGE CPP, DeriveGeneric, EmptyDataDecls, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
module GHC.Generics.Instances where

import GHC.Generics ( Generic )
import GHC.Generics.Instances.TH

import GHC.Word

import Language.Haskell.TH.Syntax
#if !MIN_VERSION_template_haskell(2,8,0)
import Language.Haskell.TH.Syntax.Internals
#endif

--TODO: Exhaustive coverage of base?

$(fmap concat $ mapM buildOpaque
  [ ''Word
  , ''Word8
  , ''Word16
  , ''Word32
  , ''Word64
  ])

-- Instances for Language.Haskell.TH.Syntax.Internals (pre 2.8.0.0)
deriving instance Generic ModName
deriving instance Generic PkgName
deriving instance Generic OccName

-- Instances for Language.Haskell.TH.Syntax
--deriving instance Generic NameFlavour
deriving instance Generic NameSpace
deriving instance Generic NameIs

deriving instance Generic Loc
deriving instance Generic Info
deriving instance Generic Fixity
deriving instance Generic FixityDirection
deriving instance Generic Exp
deriving instance Generic Dec
deriving instance Generic Stmt
deriving instance Generic Type
deriving instance Generic Foreign
deriving instance Generic FunDep
deriving instance Generic Con
deriving instance Generic Body
deriving instance Generic Clause
deriving instance Generic Strict
deriving instance Generic Safety
deriving instance Generic Callconv
deriving instance Generic Guard
deriving instance Generic Range
deriving instance Generic Match
deriving instance Generic Pat
deriving instance Generic Lit

deriving instance Generic FamFlavour
deriving instance Generic Pragma
deriving instance Generic Pred
deriving instance Generic TyVarBndr

#if !MIN_VERSION_template_haskell(2,7,0)
deriving instance Generic ClassInstance
#endif

#if !MIN_VERSION_template_haskell(2,8,0)
deriving instance Generic InlineSpec
deriving instance Generic Kind
#endif

#if MIN_VERSION_template_haskell(2,8,0)
deriving instance Generic Inline
deriving instance Generic Phases
deriving instance Generic RuleBndr
deriving instance Generic RuleMatch
deriving instance Generic TyLit
#endif

#if MIN_VERSION_template_haskell(2,9,0)
deriving instance Generic Role
deriving instance Generic TySynEqn
#endif
