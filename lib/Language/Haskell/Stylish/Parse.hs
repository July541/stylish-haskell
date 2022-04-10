module Language.Haskell.Stylish.Parse
  ( parseModule
  ) where


--------------------------------------------------------------------------------
import           Data.Function                   ((&))
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           System.IO.Unsafe                (unsafePerformIO)

--------------------------------------------------------------------------------
import           GHC.Data.Bag                    (bagToList)
import           GHC.Data.FastString             (mkFastString)
import qualified GHC.Driver.Session              as GHC
import qualified GHC.Hs                          as GHC
import qualified GHC.LanguageExtensions          as GHC
import qualified GHC.Utils.Error                 as GHC
import           GHC.Data.StringBuffer           (stringToStringBuffer)
import qualified GHC.Data.StringBuffer           as GHC
import qualified GHC.Parser                      as GHC
import           GHC.Parser.Lexer                (ParseResult (..), mkPState,
                                                  unP)
import qualified GHC.Parser.Lexer                as GHC
import           GHC.Types.SrcLoc                (mkRealSrcLoc)
import qualified GHC.Types.SrcLoc                as GHC
import qualified GHC.Utils.Panic                 as GHC

--------------------------------------------------------------------------------
import qualified GHC.Driver.Types                as GHC
import qualified GHC.Parser.Header               as GHC
import           Language.Haskell.Stylish.GHC    (baseDynFlags)
import           Language.Haskell.Stylish.Module

type Extensions = [String]

--------------------------------------------------------------------------------
-- | Filter out lines which use CPP macros
unCpp :: String -> String
unCpp = unlines . go False . lines
  where
    go _           []       = []
    go isMultiline (x : xs) =
        let isCpp         = isMultiline || listToMaybe x == Just '#'
            nextMultiline = isCpp && not (null x) && last x == '\\'
        in (if isCpp then "" else x) : go nextMultiline xs

--------------------------------------------------------------------------------
-- | If the given string is prefixed with an UTF-8 Byte Order Mark, drop it
-- because haskell-src-exts can't handle it.
dropBom :: String -> String
dropBom ('\xfeff' : str) = str
dropBom str              = str


--------------------------------------------------------------------------------
-- | Abstraction over GHC lib's parsing
parseModule :: Extensions -> Maybe FilePath -> String -> Either String Module
parseModule exts fp string =
  parsePragmasIntoDynFlags baseDynFlags userExtensions filePath string >>= \dynFlags ->
    dropBom string
      & removeCpp dynFlags
      & runParser dynFlags
      & toModule dynFlags
  where
    toModule :: GHC.DynFlags -> GHC.ParseResult (GHC.Located GHC.HsModule) -> Either String Module
    toModule dynFlags res = case res of
      POk ps m ->
        Right (makeModule ps m)
      PFailed failureState ->
        let
          withFileName x = maybe "" (<> ": ") fp <> x
        in
        Left . withFileName . unlines . getParserStateErrors dynFlags $ failureState

    removeCpp dynFlags s =
      if GHC.xopt GHC.Cpp dynFlags then unCpp s
      else s

    userExtensions =
      fmap toLocatedExtensionFlag ("Haskell2010" : exts) -- FIXME: do we need `Haskell2010` here?

    toLocatedExtensionFlag flag
      = "-X" <> flag
      & GHC.L GHC.noSrcSpan

    getParserStateErrors dynFlags state
      = GHC.getErrorMessages state dynFlags
      & bagToList
      & fmap (\errMsg -> show (GHC.errMsgSpan errMsg) <> ": " <> show errMsg)

    filePath =
      fromMaybe "<interactive>" fp

    runParser :: GHC.DynFlags -> String -> GHC.ParseResult (GHC.Located GHC.HsModule)
    runParser flags str =
      let
        filename = mkFastString filePath
        parseState = mkPState flags (stringToStringBuffer str) (mkRealSrcLoc filename 1 1)
      in
        unP GHC.parseModule parseState

-- | Parse 'DynFlags' from the extra options
--
--   /Note:/ this function would be IO, but we're not using any of the internal
--   features that constitute side effectful computation. So I think it's fine
--   if we run this to avoid changing the interface too much.
parsePragmasIntoDynFlags ::
     GHC.DynFlags
  -> [GHC.Located String]
  -> FilePath
  -> String
  -> Either String GHC.DynFlags
{-# NOINLINE parsePragmasIntoDynFlags #-}
parsePragmasIntoDynFlags originalFlags extraOpts filepath str = unsafePerformIO $ catchErrors $ do
  let opts = GHC.getOptions originalFlags (GHC.stringToStringBuffer str) filepath
  (parsedFlags, _invalidFlags, _warnings) <- GHC.parseDynamicFilePragma originalFlags (opts <> extraOpts)
  -- FIXME: have a look at 'leftovers' since it should be empty
  return $ Right $ parsedFlags `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  where
    catchErrors act = GHC.handleGhcException reportErr (GHC.handleSourceError reportErr act)
    reportErr e = return $ Left (show e)
