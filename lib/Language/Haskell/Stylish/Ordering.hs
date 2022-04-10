--------------------------------------------------------------------------------
-- | There are a number of steps that sort items: 'Imports' and 'ModuleHeader',
-- and maybe more in the future.  This module provides consistent sorting
-- utilities.
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Ordering
    ( compareLIE
    , compareSrcSpan
    , compareWrappedName
    , unwrapName
    ) where


--------------------------------------------------------------------------------
import           Data.Char                    (isUpper)
import           Data.Ord                     (comparing)
import           GHC                          (RdrName, SrcSpan (..))
import           GHC.Hs
import           GHC.Types.SrcLoc             (unLoc)


--------------------------------------------------------------------------------
import           GHC.Utils.Outputable         (Outputable)
import           Language.Haskell.Stylish.GHC (showOutputable)


--------------------------------------------------------------------------------
-- | NOTE: Can we get rid off this by adding a properly sorting newtype around
-- 'RdrName'?
compareLIE :: LIE GhcPs -> LIE GhcPs -> Ordering
compareLIE = comparing $ ieKey . unLoc
  where
    -- | The implementation is a bit hacky to get proper sorting for input specs:
    -- constructors first, followed by functions, and then operators.
    ieKey :: IE GhcPs -> (Int, String)
    ieKey = \case
        IEVar _ n             -> nameKey n
        IEThingAbs _ n        -> nameKey n
        IEThingAll _ n        -> nameKey n
        IEThingWith _ n _ _ _ -> nameKey n
        IEModuleContents _ n  -> nameKey n
        _                     -> (2, "")


--------------------------------------------------------------------------------
compareWrappedName :: IEWrappedName RdrName -> IEWrappedName RdrName -> Ordering
compareWrappedName = comparing nameKey


--------------------------------------------------------------------------------
unwrapName :: IEWrappedName n -> n
unwrapName (IEName n)    = unLoc n
unwrapName (IEPattern n) = unLoc n
unwrapName (IEType n)    = unLoc n


--------------------------------------------------------------------------------
nameKey :: Outputable name => name -> (Int, String)
nameKey n = case showOutputable n of
    o@('(' : _)             -> (2, o)
    o@(o0 : _) | isUpper o0 -> (0, o)
    o                       -> (1, o)

-- | Compare two `SrcSpan`, since ghc9.0 removed `Ord` instance for `SrcSpan`
compareSrcSpan :: SrcSpan -> SrcSpan -> Ordering
compareSrcSpan s1 s2 = case s1 of
  RealSrcSpan ss1 bf1 -> case s2 of
    RealSrcSpan ss2 bf2 -> case ss1 `compare` ss2 of
      LT -> LT
      EQ -> bf1 `compare` bf2
      GT -> GT
    UnhelpfulSpan _ -> GT
  UnhelpfulSpan _ -> case s2 of
    RealSrcSpan _ _ -> LT
    UnhelpfulSpan _ -> EQ
