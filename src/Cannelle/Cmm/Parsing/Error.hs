{-# LANGUAGE LambdaCase #-}

module Cannelle.Cmm.Parsing.Error
  ( prettyCmmErrorBundle
  , prettyCmmErrorBundleStr
  ) where

-- Error pretty-printer for Cannelle Cmm parser
-- --------------------------------------------

import Data.List (intercalate, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Ord as Do
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as ME
import Text.Megaparsec.Pos (SourcePos(..), unPos)

-- | Pretty-print a Megaparsec bundle with extra structure for Cannelle.
--   Emphasizes your `<?>` labels to hint which combinator/branch failed.
prettyCmmErrorBundle :: ME.ParseErrorBundle Text Void -> Text
prettyCmmErrorBundle bundle =
  T.intercalate "\n\n" (showError (ME.bundlePosState bundle) 1 (NE.toList (ME.bundleErrors bundle)))
  where
    showError :: MP.PosState Text -> Int -> [ME.ParseError Text Void] -> [Text]
    showError _   _ []     = []
    showError pst n (e:es) =
      let
        off = ME.errorOffset e
        (MP.PosState lineTxt _ srcPos _ _) = pst
        (pos, pst') = MP.reachOffset off pst
        fileName = T.pack (sourceName srcPos)
        lineNum = unPos (sourceLine srcPos)
        colNum = unPos (sourceColumn srcPos)
        caretLine = T.replicate (max 0 (colNum - 1)) " " <> "^"
        header = T.concat
          [ "• [", T.pack (show n), "] at "
          , if T.null fileName then "<input>" else fileName
          , ":", T.pack (show lineNum), ":", T.pack (show colNum)
          ]
        detail                 = renderParseError e
        snippet                = T.unlines
          [ "  " <> lineTxt
          , "  " <> caretLine
          ]
      in T.unlines [ header, snippet, detail ] : showError pst' (n+1) es

-- | String variant (e.g. for putStrLn).
prettyCmmErrorBundleStr :: ME.ParseErrorBundle Text Void -> String
prettyCmmErrorBundleStr = T.unpack . prettyCmmErrorBundle

-- Internal helpers ------------------------------------------------------------

renderParseError :: ME.ParseError Text Void -> Text
renderParseError pe = case pe of
  ME.TrivialError _ mUnexpected expected ->
    T.unlines $ filter (not . T.null)
      [ maybe T.empty (\u -> "  unexpected: " <> ppUnexpected u) mUnexpected
      , let (labels, tokens) = partitionExpected expected
        in T.intercalate "\n" $ filter (not . T.null)
             [ if null labels then T.empty
               else "  expected (labels): " <> commaSep labels
             , if null tokens then T.empty
               else "  expected (tokens): " <> commaSep tokens
             ]
      ]
  ME.FancyError _ fancySet ->
    let items = map ppFancy (S.toList fancySet)
    in if null items then "  (fancy error with no details)"
       else T.unlines (map ("  " <>) items)

ppUnexpected :: ME.ErrorItem Char -> Text
ppUnexpected = \case
  ME.Tokens ts -> "“" <> T.pack (NE.toList ts) <> "”"
  ME.Label  ts -> "«" <> T.pack (NE.toList ts) <> "»"
  ME.EndOfInput -> "end of input"

-- Split expected items into label-like vs token-like for clarity.
partitionExpected :: S.Set (ME.ErrorItem Char) -> ([Text], [Text])
partitionExpected s =
  let xs = S.toList s
      labels = [ "«" <> T.pack (NE.toList t) <> "»"
               | ME.Label t <- xs
               ]
      toks   = [ "“" <> T.pack (NE.toList t) <> "”"
               | ME.Tokens t <- xs
               ] ++ [ "end of input" | ME.EndOfInput <- xs ]
      -- Put longer labels first (often more specific)
      order  = sortOn (Do.Down . T.length)
  in (order labels, toks)

ppFancy :: ME.ErrorFancy Void -> Text
ppFancy = \case
  ME.ErrorFail msg -> "fail: " <> T.pack msg
  ME.ErrorIndentation ord ref actual ->
    let o = case ord of LT -> "less than"; EQ -> "equal to"; GT -> "greater than"
    in T.concat ["indentation error: got ", tPos actual
                , ", expected ", o, " ", tPos ref ]
  anotherError -> "another error: " <> T.pack (show anotherError)

tPos :: MP.Pos -> Text
tPos = T.pack . show . unPos

commaSep :: [Text] -> Text
commaSep = T.pack . intercalate ", " . map T.unpack
