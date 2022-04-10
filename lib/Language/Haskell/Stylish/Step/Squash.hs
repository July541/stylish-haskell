--------------------------------------------------------------------------------
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Haskell.Stylish.Step.Squash
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                      (mapMaybe)
import qualified GHC.Hs                          as Hs
import qualified GHC.Types.SrcLoc                as S


--------------------------------------------------------------------------------
import           GHC.Types.SrcLoc                (RealSrcSpan,
                                                  SrcSpan (RealSrcSpan), getLoc)
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
squash :: RealSrcSpan -> RealSrcSpan -> Maybe (Change String)
squash lAnn rAnn = do
  if S.srcSpanEndLine lAnn == S.srcSpanStartLine rAnn ||
      S.srcSpanEndLine lAnn + 1 == S.srcSpanStartLine rAnn
    then Just $
          changeLine (S.srcSpanEndLine lAnn) $ \str ->
          let (pre, post) = splitAt (S.srcSpanEndCol lAnn) str
          in [trimRight pre ++ " " ++ trimLeft post]
    else Nothing


--------------------------------------------------------------------------------
squashFieldDecl :: Hs.ConDeclField Hs.GhcPs -> Maybe (Change String)
squashFieldDecl (Hs.ConDeclField _ names type' _)
  | null names = Nothing
  | Just l <- srcSpanToRealSrcSpan $ getLoc (last names)
  , Just r <- srcSpanToRealSrcSpan $ getLoc type'
    = squash l r
squashFieldDecl _ = Nothing


srcSpanToRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
srcSpanToRealSrcSpan (RealSrcSpan ss _) = Just ss
srcSpanToRealSrcSpan _                  = Nothing


--------------------------------------------------------------------------------
squashMatch :: Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> Maybe (Change String)
squashMatch (Hs.Match _ (Hs.FunRhs name _ _) [] grhss) = do
    body <- unguardedRhsBody grhss
    l <- srcSpanToRealSrcSpan $ getLoc name
    r <- srcSpanToRealSrcSpan $ getLoc body
    squash l r
squashMatch (Hs.Match _ _ pats grhss) = do
    body <- unguardedRhsBody grhss
    l <- srcSpanToRealSrcSpan $ getLoc (last pats)
    r <- srcSpanToRealSrcSpan $ getLoc body
    squash l r
squashMatch (Hs.XMatch x) = Hs.noExtCon x


--------------------------------------------------------------------------------
step :: Step
step = makeStep "Squash" $ \ls module' ->
    let changes =
            mapMaybe squashFieldDecl (everything module') ++
            mapMaybe squashMatch (everything module') in
    applyChanges changes ls
