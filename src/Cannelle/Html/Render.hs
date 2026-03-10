module Cannelle.Html.Render where

import Control.Monad (forM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Cannelle.Html.Types


--------------------------------------------------------------------------------
-- Compact text lookup
--------------------------------------------------------------------------------

lookupCompactText :: CompactTextIndex -> ByteString -> Int32 -> Either String Text
lookupCompactText textIndex compactBlock textId =
  case IM.lookup (fromIntegral textId) textIndex of
    Nothing ->
      Left
        ( "lookupCompactText: missing text ID "
            <> show textId
        )

    Just (offset32, len32) -> do
      offset <- int32ToNonNegative "text offset" offset32
      len    <- int32ToNonNegative "text length" len32

      let end = offset + len
          totalLen = BS.length compactBlock

      if end > totalLen
        then
          Left
            ( "lookupCompactText: slice out of bounds for text ID "
                <> show textId
                <> " (offset="
                <> show offset
                <> ", length="
                <> show len
                <> ", block length="
                <> show totalLen
                <> ")"
            )
        else
          let slice = BS.take len (BS.drop offset compactBlock)
          in first
               (\err ->
                   "lookupCompactText: invalid UTF-8 for text ID "
                     <> show textId
                     <> ": "
                     <> show err
               )
               (TE.decodeUtf8' slice)

--------------------------------------------------------------------------------
-- HTML reconstruction
--------------------------------------------------------------------------------

-- | Rebuild a canonical textual HTML document from:
--   * compact text index
--   * compact UTF-8 text block
--   * compact DOM representation
--
-- Notes:
--   * known tags are reconstructed from their positive tag codes
--   * unknown tags use the negative tag-code convention:
--         tagCode < 0  ==> abs tagCode is the pooled text ID of the tag label
--   * attribute values are emitted in double quotes
--   * text nodes are emitted as stored, without additional escaping
--     (this best matches the “reconstruct from pooled text” goal)
renderHtmlDocumentC :: CompactTextIndex -> ByteString -> HtmlDocumentC -> Either String Text
renderHtmlDocumentC textIndex compactBlock doc = do
  headerText <- lookupCompactText textIndex compactBlock doc.docHeaderC
  rootTexts  <- mapM (renderHtmlNodeC textIndex compactBlock) (V.toList doc.domC)
  let
    bodyText = T.concat rootTexts
  pure $ case headerText of
    "" -> bodyText
    _ -> "<!DOCTYPE " <> headerText <> ">" <> "\n" <> bodyText


renderHtmlNodeC
  :: CompactTextIndex
  -> ByteString
  -> HtmlNodeC
  -> Either String Text
renderHtmlNodeC textIndex compactBlock node =
  case node of
    TextEntryC textId ->
      lookupCompactText textIndex compactBlock textId

    CommentEntryC textId ->
      case lookupCompactText textIndex compactBlock textId of
        Left err -> Left err
        Right text -> pure ("<!--" <> text <> "-->")

    NodeHC {} -> do
      tagLabel <- tagCodeToLabel textIndex compactBlock node.tagIDC
      attrText <- renderAttributesC textIndex compactBlock node.attributesC

      if isVoidTagLabel tagLabel
        then
          pure ("<" <> tagLabel <> attrText <> ">")
        else do
          childTexts <- mapM (renderHtmlNodeC textIndex compactBlock) (V.toList node.childrenC)
          let inner = T.concat childTexts
          pure
            ( "<" <> tagLabel <> attrText <> ">"
                <> inner
                <> "</" <> tagLabel <> ">"
            )

renderAttributesC
  :: CompactTextIndex
  -> ByteString
  -> V.Vector AttributeC
  -> Either String Text
renderAttributesC textIndex compactBlock attrs = do
  rendered <- mapM renderOne (V.toList attrs)
  pure (T.concat rendered)
  where
    renderOne :: AttributeC -> Either String Text
    renderOne attr = do
      nameText  <- lookupCompactText textIndex compactBlock attr.nameC
      valueText <- lookupCompactText textIndex compactBlock attr.valueC
      pure
        ( " "
            <> nameText
            <> "=\""
            <> escapeAttributeValue valueText
            <> "\""
        )

--------------------------------------------------------------------------------
-- Tag decoding
--------------------------------------------------------------------------------

-- | Positive tag codes are known tags.
-- | Negative tag codes encode unknown tag labels via pooled text IDs.
tagCodeToLabel
  :: CompactTextIndex
  -> ByteString
  -> Int32
  -> Either String Text
tagCodeToLabel textIndex compactBlock tagCode
  | tagCode > 0 =
      case knownTagCodeToLabel tagCode of
        Just label ->
          Right label

        Nothing ->
          Left
            ( "tagCodeToLabel: unknown positive tag code "
                <> show tagCode
            )

  | tagCode < 0 =
      lookupCompactText textIndex compactBlock (abs tagCode)

  | otherwise =
      Left "tagCodeToLabel: tag code 0 is invalid"

knownTagCodeToLabel :: Int32 -> Maybe Text
knownTagCodeToLabel tagCode =
  case tagCode of
    1  -> Just "html"
    2  -> Just "head"
    3  -> Just "title"
    4  -> Just "body"
    5  -> Just "meta"
    6  -> Just "link"
    7  -> Just "script"
    8  -> Just "style"
    9  -> Just "div"
    10 -> Just "span"
    11 -> Just "p"
    12 -> Just "a"
    13 -> Just "img"
    14 -> Just "br"
    15 -> Just "hr"
    16 -> Just "input"
    17 -> Just "form"
    18 -> Just "button"
    19 -> Just "label"
    20 -> Just "select"
    21 -> Just "option"
    22 -> Just "textarea"
    23 -> Just "ul"
    24 -> Just "ol"
    25 -> Just "li"
    26 -> Just "table"
    27 -> Just "thead"
    28 -> Just "tbody"
    29 -> Just "tfoot"
    30 -> Just "tr"
    31 -> Just "td"
    32 -> Just "th"
    33 -> Just "section"
    34 -> Just "article"
    35 -> Just "header"
    36 -> Just "footer"
    37 -> Just "main"
    38 -> Just "nav"
    39 -> Just "aside"
    40 -> Just "h1"
    41 -> Just "h2"
    42 -> Just "h3"
    43 -> Just "h4"
    44 -> Just "h5"
    45 -> Just "h6"
    46 -> Just "strong"
    47 -> Just "em"
    48 -> Just "small"
    49 -> Just "pre"
    50 -> Just "code"
    _  -> Nothing

isVoidTagLabel :: Text -> Bool
isVoidTagLabel label =
  label `elem`
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]

--------------------------------------------------------------------------------
-- Escaping helpers
--------------------------------------------------------------------------------

-- | Canonical attribute escaping for reconstruction.
-- We leave text nodes untouched, but attribute values are quoted and escaped.
escapeAttributeValue :: Text -> Text
escapeAttributeValue =
  T.concatMap
    (\c ->
        case c of
          '&'  -> "&amp;"
          '"'  -> "&quot;"
          '<'  -> "&lt;"
          '>'  -> "&gt;"
          _    -> T.singleton c
    )

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

int32ToNonNegative :: String -> Int32 -> Either String Int
int32ToNonNegative label n
  | n < 0 =
      Left
        ( label
            <> " must be non-negative, got "
            <> show n
        )
  | otherwise =
      Right (fromIntegral n)