module Cannelle.Hugo.Defines where

import qualified Data.ByteString as Bs
import Data.List.NonEmpty (NonEmpty (..))
import Data.Int (Int32)
import qualified Data.Map as Mp

import Cannelle.VM.Context (MainText)
import Cannelle.Hugo.Types


impModules :: Mp.Map Int32 (MainText, Maybe Int32)
impModules = Mp.fromList [
  (0, ("hugo", Nothing))
  , (1, ("cast", Just 0))
      -- ToFloat, ToInt, ToString.
  , (2, ("collections", Just 0))
      -- After, Append, Apply, Complement, Delimit, Dictionnary, First, Group, In, Index, Intersect, IsSet, KeyVals, Last, Merge, NewScratch, Querify, Reverse, Seq, Shuffle, Slice, Sort, SymDiff, Union, Uniq, Where.
  , (3, ("compare", Just 0))
      -- Conditional, Default, Eq, Ge, Gt, Le, Lt, Ne.
  , (4, ("crypto", Just 0))
      -- FNV32a, HMAC, MD5, SHA1, SHA256.
  , (5, ("css", Just 0))
      -- PostCSS, Sass, TailwindCSS.
  , (6, ("data", Just 0))
      -- GetCSV, GetJSON.
  , (7, ("debug", Just 0))
      -- Dump, Timer.
  , (8, ("diagrams", Just 0))
      -- Goat.
  , (9, ("encoding", Just 0))
      -- Base64Decode, Base64Encode, Jsonify.
  , (10, ("fmt", Just 0))
      -- Errorf, Erroridf, Print, Printf, Println, Warnf, Warnidf.
  , (11, ("global", Just 0))
      -- page, site.
  , (12, ("go template", Just 0))
      -- and, len, not, or, urlquery.
  , (13, ("hash", Just 0))
      -- FNV32a, XxHash.
  , (14, ("hugo", Just 0))
      -- BuildDate, CommitHash, Deps, Environment, Generator, GoVersion, IsDevelopment, isExtended, isMultiHost, isMultilingual, isProduction, isServer, Version, WorkingDir.
  , (15, ("images", Just 0))
      -- AutoOrient, Brightness, ColorBalance, Colorize, Config, Contrast, Dither, Filter, Gamma, GaussianBlur, GrayScale, Hue, Invert, Opacity, Overlay, Padding, Pixelate, Process, Saturation, Sepia, Sigmoid, Text, UnsharpMask.
  , (16, ("inflect", Just 0))
      -- Humanize, Pluralize, Singularize.
  , (17, ("js", Just 0))
      -- Babel, Build.
  , (18, ("lang", Just 0))
      -- FormatAccounting, FormatCurrency, FormatNumber, FormatNumberCustom, FormatPercent, Merge, Translate.
  , (19, ("math", Just 0))
      -- Abs, Acos, Add, Asin, Atan, Atan2, Ceil, Cos, Counter, Div, Floor, Log, Max, Min, Mod, ModBool, Mul, Pi, Pow, Rand, Round, Sin, Sqrt, Sub, Sum, Tan, ToDegrees, ToRadians.
  , (20, ("openapi3", Just 0))
      -- Unmarshal.
  , (21, ("os", Just 0))
      -- FileExists, GetEnv, ReadDir, ReadFile, Stat.
  , (22, ("partials", Just 0))
      -- Include, IncludeCached.
  , (23, ("path", Just 0))
      -- Base, BaseName, Clean, Dir, Ext, Join, Split.
  , (24, ("reflect", Just 0))
      -- IsMap, IsSlice.
  , (25, ("resources", Just 0))
      -- Babel, ByType, Concat, Copy, ExecuteAsTemplate, Fingerprint, FromString, Get, GetMatch, GetRemote, Match, Minify, PostCSS, PostProcess, ToCSS.
  , (26, ("safe", Just 0))
      -- CSS, HTML, HTMLAttr, JS, JSStr, URL.
  , (27, ("strings", Just 0))
      -- Chomp, Contains, ContainsAny, ContainsNonSpace, Count, CountRunes, CountWords, Diff, FindRE, FindRESubmatch, FirstUpper, HasPrefix, HasSuffix, Repeat, Replace, ReplaceRE, RuneCount, SliceString, Split, Substr, Title, ToLower, ToUpper, Trim, TrimLeft, TrimPrefix, TrimRight, TrimSuffix, Truncate.
  , (28, ("templates", Just 0))
      -- Defer, Exists.
  , (29, ("time", Just 0))
      -- AsTime, Duration, Format, Now, ParseDuration.
  , (30, ("transform", Just 0))
      -- CanHighlight, Emojify, Highlight, HighlightCodeBlock, HtmlEscape, HtmlUnescape, Markdownify, Plainify, Remarshal, ToMath, Unmarshal, XMLEscape.
  , (31, ("urls", Just 0))
      -- AbsLangURL, AbsURL, Anchorize, JoinPath, Parse, Ref, RelLangURL, RelRef, RelURL, URLize.
  ]

impRevModules :: Mp.Map MainText [(Int32, Maybe Int32)]
impRevModules = Mp.foldlWithKey (\acc modId (modName, mbParentId) -> Mp.insertWith (<>) modName [(modId, mbParentId)] acc) Mp.empty impModules


-- Warning: Hugo's functions are polymorphic, the right function is selected at runtime!
impFunctions :: Mp.Map MainText [(FctDefComp, Int32)]
impFunctions =
  let
    hugoFcts = [
      -- cast:
      -- ToFloat, ToInt, ToString.
        FctDefComp 1 "ToFloat" (MonoDef (SgnS (Just (("input", DynamicVT) :| [])) [] Nothing (MonadicVT (DynamicVT :| []))))
      , FctDefComp 1 "ToInt" (MonoDef (SgnS (Just (("input", DynamicVT) :| [])) [] Nothing (SimpleVT IntST)))
      , FctDefComp 1 "ToString" (MonoDef (SgnS (Just (("input", DynamicVT) :| [])) [] Nothing (SimpleVT StringST)))
      -- collections:
      -- After, Append, Apply, Complement, Delimit, Dictionnary, First, Group, In, Index, Intersect, IsSet, KeyVals, Last, Merge, NewScratch, Querify, Reverse, Seq, Shuffle, Slice, Sort, SymDiff, Union, Uniq, Where.
      , FctDefComp 2 "After" (MonoDef (SgnS (Just (("index", SimpleVT IntST) :| [("collection", MonadicVT (DynamicVT :| []))])) [] Nothing (MonadicVT (DynamicVT :| []))))
      , FctDefComp 2 "Append" (PolyDef [
            SgnS (Just (("element", DynamicVT) :| [("arrayElement", MonadicVT (DynamicVT :| []))])) [] Nothing (MonadicVT (DynamicVT :| []))
          , SgnS (Just (("collection1", MonadicVT (DynamicVT :| [])) :| [("collection2", MonadicVT (DynamicVT :| []))])) [] Nothing (MonadicVT (DynamicVT :| []))
        ])
      , FctDefComp 2 "Apply" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [("function", SimpleVT StringST)])) [] (Just ("args", DynamicVT)) (MonadicVT (DynamicVT :| []))))
      , FctDefComp 2 "Complement" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [])) [] (Just ("others", MonadicVT (DynamicVT :| []))) (MonadicVT (DynamicVT :| []))))
      , FctDefComp 2 "Delimit" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [("delimiter", SimpleVT StringST)])) [("last", SimpleVT StringST)] Nothing (SimpleVT StringST)))
      , FctDefComp 2 "Dictionnary" (MonoDef (SgnS (Just (("value", MonadicVT (DynamicVT :| [])) :| [])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns mapany; alias dict.
      , FctDefComp 2 "First" (MonoDef (SgnS (Just (("n", SimpleVT IntST) :| [("collection", MonadicVT (DynamicVT :| []))])) [] Nothing DynamicVT))
      , FctDefComp 2 "Group" (MonoDef (SgnS (Just (("key", SimpleVT StringST) :| [("collection", MonadicVT (DynamicVT :| []))])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- collection: PAGES; returns any (in collection?); alias group.
      , FctDefComp 2 "In" (MonoDef (SgnS (Just (("set", DynamicVT) :| [("value", DynamicVT)])) [] Nothing (SimpleVT BoolST))) -- alias in.
      , FctDefComp 2 "Index" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [("key", DynamicVT)])) [] Nothing DynamicVT))  -- returns any; alias index.
      , FctDefComp 2 "Intersect" (MonoDef (SgnS (Just (("set1", MonadicVT (DynamicVT :| [])) :| [("set2", MonadicVT (DynamicVT :| []))])) [] Nothing DynamicVT)) -- returns any (collection?); alias intersect.
      , FctDefComp 2 "IsSet" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [("key", DynamicVT)])) [] Nothing (SimpleVT BoolST)))  -- alias isSet.
      , FctDefComp 2 "KeyVals" (MonoDef (SgnS (Just (("key", DynamicVT) :| [])) [] (Just ("value", DynamicVT)) (MonadicVT (DynamicVT :| []))))  -- returns types.KeyValues; alias keyVals.
      , FctDefComp 2 "Last" (MonoDef (SgnS (Just (("key", SimpleVT IntST) :| [("collection", MonadicVT (DynamicVT :| []))])) [] Nothing (DynamicVT))) -- returns any; alias last.
      , FctDefComp 2 "Merge" (MonoDef (SgnS (Just (("map1", MonadicVT (DynamicVT :| [])) :| [])) [] (Just ("map2", MonadicVT (DynamicVT :| []))) (MonadicVT (DynamicVT :| [])))) -- returns any; alias merge.
      , FctDefComp 2 "NewScratch" (MonoDef (SgnS Nothing [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns maps.Scratch; alias newScratch.
      , FctDefComp 2 "Querify" (MonoDef (SgnS Nothing [] (Just ("value", DynamicVT)) (SimpleVT StringST))) -- alias querify.
      , FctDefComp 2 "Reverse" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns any.
      , FctDefComp 2 "Seq" (PolyDef [
            SgnS (Just (("last", SimpleVT IntST) :| [])) [] Nothing (MonadicVT (SimpleVT IntST :| []))
            , SgnS (Just (("first", SimpleVT IntST) :| [("last", SimpleVT IntST)])) [] Nothing (MonadicVT (SimpleVT IntST :| []))
            , SgnS (Just (("first", SimpleVT IntST) :| [("increment", SimpleVT IntST), ("last", SimpleVT IntST)])) [] Nothing (MonadicVT (SimpleVT IntST :| []))
          ])  -- returns []int; alias seq.
      -- TODO: Continue the proper typing (currently fake signature defined by AI auto-completer).
      , FctDefComp 2 "Shuffle" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns any;alias shuffle.
      , FctDefComp 2 "Slice" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [("start", SimpleVT IntST), ("end", SimpleVT IntST)])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- -- returns any; alias slice.
      , FctDefComp 2 "Sort" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [("key", SimpleVT StringST), ("order", SimpleVT StringST)])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns any; alias sort.
      , FctDefComp 2 "SymDiff" (MonoDef (SgnS (Just (("collection1", MonadicVT (DynamicVT :| [])) :| [("collection2", MonadicVT (DynamicVT :| []))])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns any; alias symdiff.
      , FctDefComp 2 "Union" (MonoDef (SgnS (Just (("set1", MonadicVT (DynamicVT :| [])) :| [("set2", MonadicVT (DynamicVT :| []))])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns any; alias union.
      , FctDefComp 2 "Uniq" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- returns any; alias uniq.
      , FctDefComp 2 "Where" (MonoDef (SgnS (Just (("collection", MonadicVT (DynamicVT :| [])) :| [("key", DynamicVT), ("value", DynamicVT)])) [("operator", SimpleVT StringST)] Nothing (MonadicVT (DynamicVT :| [])))) -- opt operator is in between key / value; returns any; alias where.
      -- compare:
      -- Conditional, Default, Eq, Ge, Gt, Le, Lt, Ne.
      , FctDefComp 3 "Conditional" (MonoDef (SgnS (Just (("condition", SimpleVT BoolST) :| [("value1", DynamicVT), ("value2", DynamicVT)])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- alias cond.
      , FctDefComp 3 "Default" (MonoDef (SgnS (Just (("value", DynamicVT) :| [("default", DynamicVT)])) [] Nothing DynamicVT)) -- alias default.
      , FctDefComp 3 "Eq" (MonoDef (SgnS (Just (("value1", DynamicVT) :| [("value2", DynamicVT)])) [] Nothing (SimpleVT BoolST))) -- alias eq.
      , FctDefComp 3 "Ge" (MonoDef (SgnS (Just (("value1", DynamicVT) :| [("value2", DynamicVT)])) [] Nothing (SimpleVT BoolST))) -- alias ge.
      , FctDefComp 3 "Gt" (MonoDef (SgnS (Just (("value1", DynamicVT) :| [("value2", DynamicVT)])) [] Nothing (SimpleVT BoolST))) -- alias gt.
      , FctDefComp 3 "Le" (MonoDef (SgnS (Just (("value1", DynamicVT) :| [("value2", DynamicVT)])) [] Nothing (SimpleVT BoolST))) -- alias le.
      , FctDefComp 3 "Lt" (MonoDef (SgnS (Just (("value1", DynamicVT) :| [("value2", DynamicVT)])) [] Nothing (SimpleVT BoolST))) -- alias lt.
      , FctDefComp 3 "Ne" (MonoDef (SgnS (Just (("value1", DynamicVT) :| [("value2", DynamicVT)])) [] Nothing (SimpleVT BoolST))) -- alias ne.
      -- crypto:
      -- FNV32a, HMAC, MD5, SHA1, SHA256.
      , FctDefComp 4 "FNV32a" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT IntST))) -- alias fnv32a.
      , FctDefComp 4 "HMAC" (MonoDef (SgnS (Just ( ("input", SimpleVT StringST) :| [("key", SimpleVT StringST)] )) [] Nothing (SimpleVT StringST))) -- alias hmac.
      , FctDefComp 4 "MD5" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias md5.
      , FctDefComp 4 "SHA1" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias sha1.
      , FctDefComp 4 "SHA256" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias sha256.
      -- css:
      -- PostCSS, Sass, TailwindCSS.
      , FctDefComp 5 "PostCSS" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("options", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias postcss.
      , FctDefComp 5 "Sass" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("options", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias sass.
      , FctDefComp 5 "TailwindCSS" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("options", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias tailwindcss.
      -- data:
      -- GetCSV, GetJSON.
      , FctDefComp 6 "GetCSV" (MonoDef (SgnS (Just (("path", SimpleVT StringST) :| [])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- alias getcsv.
      , FctDefComp 6 "GetJSON" (MonoDef (SgnS (Just (("path", SimpleVT StringST) :| [])) [] Nothing (MonadicVT (DynamicVT :| [])))) -- alias getjson.
      -- debug:
      -- Dump, Timer.
      , FctDefComp 7 "Dump" (MonoDef (SgnS (Just (("value", DynamicVT) :| [])) [] Nothing DynamicVT)) -- alias dump.
      , FctDefComp 7 "Timer" (MonoDef (SgnS Nothing [] Nothing (SimpleVT IntST))) -- alias timer.
      -- diagrams:
      -- Goat.
      , FctDefComp 8 "Goat" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias goat.
      -- encoding:
      -- Base64Decode, Base64Encode, Jsonify.
      , FctDefComp 9 "Base64Decode" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias base64decode.
      , FctDefComp 9 "Base64Encode" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias base64encode.
      , FctDefComp 9 "Jsonify" (MonoDef (SgnS (Just (("input", DynamicVT) :| [])) [] Nothing (SimpleVT StringST))) -- alias jsonify.
      -- fmt:
      -- Errorf, Erroridf, Print, Printf, Println, Warnf, Warnidf.
      , FctDefComp 10 "Errorf" (MonoDef (SgnS (Just (("format", SimpleVT StringST) :| [("a", DynamicVT)])) [] Nothing (SimpleVT StringST))) -- alias errorf.
      , FctDefComp 10 "Erroridf" (MonoDef (SgnS (Just (("format", SimpleVT StringST) :| [("a", DynamicVT)])) [] Nothing (SimpleVT StringST))) -- alias erroridf.
      , FctDefComp 10 "Print" (MonoDef (SgnS (Just (("values", MonadicVT (DynamicVT :| [])) :| [])) [] Nothing DynamicVT)) -- alias print.
      , FctDefComp 10 "Printf" (MonoDef (SgnS (Just (("format", SimpleVT StringST) :| [("values", MonadicVT (DynamicVT :| []))])) [] Nothing (SimpleVT StringST))) -- alias printf.
      , FctDefComp 10 "Println" (MonoDef (SgnS (Just (("values", MonadicVT (DynamicVT :| [])) :| [])) [] Nothing (SimpleVT StringST))) -- alias println.
      , FctDefComp 10 "Warnf" (MonoDef (SgnS (Just (("format", SimpleVT StringST) :| [("a", DynamicVT)])) [] Nothing (SimpleVT StringST))) -- alias warnf.
      , FctDefComp 10 "Warnidf" (MonoDef (SgnS (Just (("format", SimpleVT StringST) :| [("a", DynamicVT)])) [] Nothing (SimpleVT StringST))) -- alias warnidf.
      -- global:
      -- page, site.
      , FctDefComp 11 "page" (MonoDef (SgnS Nothing [] Nothing (MonadicVT (DynamicVT :| [])))) -- alias page.
      , FctDefComp 11 "site" (MonoDef (SgnS Nothing [] Nothing (MonadicVT (DynamicVT :| [])))) -- alias site.
      -- go template:
      -- and, len, not, or, urlquery.
      , FctDefComp 12 "and" (MonoDef (SgnS (Just (("value1", SimpleVT BoolST) :| [("value2", SimpleVT BoolST)])) [] Nothing (SimpleVT BoolST))) -- alias and.
      , FctDefComp 12 "len" (MonoDef (SgnS (Just (("value", DynamicVT) :| [])) [] Nothing (SimpleVT IntST))) -- alias len.
      , FctDefComp 12 "not" (MonoDef (SgnS (Just (("value", SimpleVT BoolST) :| [])) [] Nothing (SimpleVT BoolST))) -- alias not.
      , FctDefComp 12 "or" (MonoDef (SgnS (Just (("value1", SimpleVT BoolST) :| [("value2", SimpleVT BoolST)])) [] Nothing (SimpleVT BoolST))) -- alias or.
      , FctDefComp 12 "urlquery" (MonoDef (SgnS (Just (("values", MonadicVT (DynamicVT :| [])) :| [])) [] Nothing (SimpleVT StringST))) -- alias urlquery.
      -- hash:
      -- FNV32a, XxHash.
      , FctDefComp 13 "FNV32a" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT IntST))) -- alias fnv32a.
      , FctDefComp 13 "XxHash" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT IntST))) -- alias xxhash.
      -- hugo:
      -- BuildDate, CommitHash, Deps, Environment, Generator, GoVersion, IsDevelopment, isExtended, isMultiHost, isMultilingual, isProduction, isServer, Version, WorkingDir.
      , FctDefComp 14 "BuildDate" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias builddate.
      , FctDefComp 14 "CommitHash" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias commithash.
      , FctDefComp 14 "Durations" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias durations.
      , FctDefComp 14 "Environment" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias environment.
      , FctDefComp 14 "Generator" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias generator.
      , FctDefComp 14 "GoVersion" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias goversion.
      , FctDefComp 14 "IsDevelopment" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias isdevelopment.
      , FctDefComp 14 "IsExtended" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias isextended.
      , FctDefComp 14 "IsMultiHost" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias ismultihost.
      , FctDefComp 14 "IsMultilingual" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias ismultilingual.
      , FctDefComp 14 "IsProduction" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias isproduction.
      , FctDefComp 14 "IsServer" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias isserver.
      , FctDefComp 14 "Version" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias version.
      , FctDefComp 14 "WorkingDir" (MonoDef (SgnS Nothing [] Nothing (SimpleVT StringST))) -- alias workingdir.
      -- images:
      -- AutoOrient, Brightness, ColorBalance, Colorize, Config, Contrast, Dither, Filter, Gamma, GaussianBlur, GrayScale, Hue, Invert, Opacity, Overlay, Padding, Pixelate, Process, Saturation, Sepia, Sigmoid, Text, UnsharpMask.
      , FctDefComp 15 "AutoOrient" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias autoorient.
      , FctDefComp 15 "Brightness" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("value", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias brightness.
      , FctDefComp 15 "ColorBalance" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("red", SimpleVT IntST), ("green", SimpleVT IntST), ("blue", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias colorbalance.
      , FctDefComp 15 "Colorize" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("color", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias colorize.
      , FctDefComp 15 "Config" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias config.
      , FctDefComp 15 "Contrast" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("value", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias contrast.
      , FctDefComp 15 "Dither" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias dither.
      , FctDefComp 15 "Filter" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("filter", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias filter.
      , FctDefComp 15 "Gamma" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("value", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias gamma.
      , FctDefComp 15 "GaussianBlur" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("sigma", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias gaussianblur.
      , FctDefComp 15 "GrayScale" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias grayscale.
      , FctDefComp 15 "Hue" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("value", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias hue.
      , FctDefComp 15 "Invert" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias invert.
      , FctDefComp 15 "Opacity" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("value", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias opacity.
      , FctDefComp 15 "Overlay" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("overlay", SimpleVT StringST), ("position", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias overlay.
      , FctDefComp 15 "Padding" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("top", SimpleVT IntST), ("right", SimpleVT IntST), ("bottom", SimpleVT IntST), ("left", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias padding.
      , FctDefComp 15 "Pixelate" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("blocksize", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias pixelate.
      , FctDefComp 15 "Process" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("commands", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias process.
      , FctDefComp 15 "Saturation" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("value", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias saturation.
      , FctDefComp 15 "Sepia" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias sepia.
      , FctDefComp 15 "Sigmoid" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("value", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias sigmoid.
      , FctDefComp 15 "Text" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("text", SimpleVT StringST), ("font", SimpleVT StringST), ("size", SimpleVT IntST), ("color", SimpleVT StringST), ("position", SimpleVT StringST), ("angle", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias text.
      , FctDefComp 15 "UnsharpMask" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("sigma", SimpleVT IntST), ("radius", SimpleVT IntST), ("amount", SimpleVT IntST), ("threshold", SimpleVT IntST)])) [] Nothing (SimpleVT StringST))) -- alias unsharpMask.
      -- inflect:
      -- Humanize, Pluralize, Singularize.
      , FctDefComp 16 "Humanize" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias humanize.
      , FctDefComp 16 "Pluralize" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias pluralize.
      , FctDefComp 16 "Singularize" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias singularize.
      -- js:
      -- Babel, Build.
      , FctDefComp 17 "Babel" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias babel.
      , FctDefComp 17 "Build" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias build.
      -- lang:
      -- FormatAccounting, FormatCurrency, FormatNumber, FormatNumberCustom, FormatPercent, Merge, Translate.
      , FctDefComp 18 "FormatAccounting" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias formataccounting.
      , FctDefComp 18 "FormatCurrency" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias formatcurrency.
      , FctDefComp 18 "FormatNumber" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias formatnumber.
      , FctDefComp 18 "FormatNumberCustom" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [("format", SimpleVT StringST)])) [] Nothing (SimpleVT StringST))) -- alias formatnumbercustom.
      , FctDefComp 18 "FormatPercent" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias formatpercent.
      , FctDefComp 18 "Merge" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias merge.
      , FctDefComp 18 "Translate" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias translate.
      -- math:
      -- Abs, Acos, Add, Asin, Atan, Atan2, Ceil, Cos, Counter, Div, Floor, Log, Max, Min, Mod, ModBool, Mul, Pi, Pow, Rand, Round, Sin, Sqrt, Sub, Sum, Tan, ToDegrees, ToRadians.
      , FctDefComp 19 "Abs" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias abs.
      , FctDefComp 19 "Acos" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias acos.
      , FctDefComp 19 "Add" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias add.
      , FctDefComp 19 "Asin" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias asin.
      , FctDefComp 19 "Atan" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias atan.
      , FctDefComp 19 "Atan2" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias atan2.
      , FctDefComp 19 "Ceil" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias ceil.
      , FctDefComp 19 "Cos" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias cos.
      , FctDefComp 19 "Counter" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias counter.
      , FctDefComp 19 "Div" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias div.
      , FctDefComp 19 "Floor" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias floor.
      , FctDefComp 19 "Log" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias log.
      , FctDefComp 19 "Max" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias max.
      , FctDefComp 19 "Min" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias min.
      , FctDefComp 19 "Mod" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias mod.
      , FctDefComp 19 "ModBool" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias modbool.
      , FctDefComp 19 "Mul" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias mul.
      , FctDefComp 19 "Pi" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias pi.
      , FctDefComp 19 "Pow" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias pow.
      , FctDefComp 19 "Rand" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias rand.
      , FctDefComp 19 "Round" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias round.
      , FctDefComp 19 "Sin" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias sin.
      , FctDefComp 19 "Sqrt" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias sqrt.
      , FctDefComp 19 "Sub" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias sub.
      , FctDefComp 19 "Sum" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias sum.
      , FctDefComp 19 "Tan" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias tan.
      , FctDefComp 19 "ToDegrees" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias todegrees.
      , FctDefComp 19 "ToRadians" (MonoDef (SgnS (Just (("input", SimpleVT FloatST) :| [])) [] Nothing (SimpleVT FloatST))) -- alias toradians.
      -- openapi3:
      -- Unmarshal.
      , FctDefComp 20 "Unmarshal" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias unmarshal.
      -- os:
      -- FileExists, GetEnv, ReadDir, ReadFile, Stat.
      , FctDefComp 21 "FileExists" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT BoolST))) -- alias fileexists.
      , FctDefComp 21 "GetEnv" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias getenv.
      , FctDefComp 21 "ReadDir" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias readdir.
      , FctDefComp 21 "ReadFile" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias readfile.
      , FctDefComp 21 "Stat" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias stat.
      -- partials:
      -- Include, IncludeCached.
      , FctDefComp 22 "Include" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias include.
      , FctDefComp 22 "IncludeCached" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias includecached.
      -- path:
      -- Base, BaseName, Clean, Dir, Ext, Join, Split.
      , FctDefComp 23 "Base" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias base.
      , FctDefComp 23 "BaseName" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias basename.
      , FctDefComp 23 "Clean" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias clean.
      , FctDefComp 23 "Dir" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias dir.
      , FctDefComp 23 "Ext" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias ext.
      , FctDefComp 23 "Join" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias join.
      , FctDefComp 23 "Split" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias split.
      -- reflect:
      -- IsMap, IsSlice.
      , FctDefComp 24 "IsMap" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias ismap.
      , FctDefComp 24 "IsSlice" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias isslice.
      -- resources:
      -- Babel, ByType, Concat, Copy, ExecuteAsTemplate, Fingerprint, FromString, Get, GetMatch, GetRemote, Match, Minify, PostCSS, PostProcess, ToCSS.
      , FctDefComp 25 "Babel" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias babel.
      , FctDefComp 25 "ByType" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias bytype.
      , FctDefComp 25 "Concat" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias concat.
      , FctDefComp 25 "Copy" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias copy.
      , FctDefComp 25 "ExecuteAsTemplate" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias executeastemplate.
      , FctDefComp 25 "Fingerprint" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias fingerprint.
      , FctDefComp 25 "FromString" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias fromstring.
      , FctDefComp 25 "Get" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias get.
      , FctDefComp 25 "GetMatch" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias getmatch.
      , FctDefComp 25 "GetRemote" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias getremote.
      , FctDefComp 25 "Match" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias match.
      , FctDefComp 25 "Minify" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias minify.
      , FctDefComp 25 "PostCSS" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias postcss.
      , FctDefComp 25 "PostProcess" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias postprocess.
      , FctDefComp 25 "ToCSS" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias tocss.
      -- safe:
      -- CSS, HTML, HTMLAttr, JS, JSStr, URL.
      , FctDefComp 26 "CSS" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias css.
      , FctDefComp 26 "HTML" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias html.
      , FctDefComp 26 "HTMLAttr" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias htmlattr.
      , FctDefComp 26 "JS" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias js.
      , FctDefComp 26 "JSStr" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias jssstr.
      , FctDefComp 26 "URL" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias url.
      -- strings:
      -- Chomp, Contains, ContainsAny, ContainsNonSpace, Count, CountRunes, CountWords, Diff, FindRE, FindRESubmatch, FirstUpper, HasPrefix, HasSuffix, Repeat, Replace, ReplaceRE, RuneCount, SliceString, Split, Substr, Title, ToLower, ToUpper, Trim, TrimLeft, TrimPrefix, TrimRight, TrimSuffix, Truncate.
      , FctDefComp 27 "Chomp" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias chomp.
      , FctDefComp 27 "Contains" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias contains.
      , FctDefComp 27 "ContainsAny" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias containsany.
      , FctDefComp 27 "ContainsNonSpace" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias containsnospace.
      , FctDefComp 27 "Count" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias count.
      , FctDefComp 27 "CountRunes" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias countrunes.
      , FctDefComp 27 "CountWords" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias countwords.
      , FctDefComp 27 "Diff" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias diff.
      , FctDefComp 27 "FindRE" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias findre.
      , FctDefComp 27 "FindRESubmatch" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias findresubmatch.
      , FctDefComp 27 "FirstUpper" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias firstupper.
      , FctDefComp 27 "HasPrefix" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias hasprefix.
      , FctDefComp 27 "HasSuffix" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias hassuffix.
      , FctDefComp 27 "Repeat" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias repeat.
      , FctDefComp 27 "Replace" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias replace.
      , FctDefComp 27 "ReplaceRE" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias replacere.
      , FctDefComp 27 "RuneCount" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias runecount.
      , FctDefComp 27 "SliceString" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias slicestring.
      , FctDefComp 27 "Split" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias split.
      , FctDefComp 27 "Substr" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias substr.
      , FctDefComp 27 "Title" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias title.
      , FctDefComp 27 "ToLower" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias tolower.
      , FctDefComp 27 "ToUpper" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias toupper.
      , FctDefComp 27 "Trim" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias trim.
      , FctDefComp 27 "TrimLeft" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias trimleft.
      , FctDefComp 27 "TrimPrefix" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias trimprefix.
      , FctDefComp 27 "TrimRight" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias trimright.
      , FctDefComp 27 "TrimSuffix" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias trimsuffix.
      , FctDefComp 27 "Truncate" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias truncate.
      -- templates:
      -- Defer, Exists.
      , FctDefComp 28 "Defer" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias defer.
      , FctDefComp 28 "Exists" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias exists.
      -- time:
      -- AsTime, Duration, Format, Now, ParseDuration.
      , FctDefComp 29 "AsTime" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias astime.
      , FctDefComp 29 "Duration" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias duration.
      , FctDefComp 29 "Format" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias format.
      , FctDefComp 29 "Now" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias now.
      , FctDefComp 29 "ParseDuration" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias parseduration.
      -- transform:
      -- CanHighlight, Emojify, Highlight, HighlightCodeBlock, HtmlEscape, HtmlUnescape, Markdownify, Plainify, Remarshal, ToMath, Unmarshal, XMLEscape.
      , FctDefComp 30 "CanHighlight" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias canhighlight.
      , FctDefComp 30 "Emojify" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias emojify.
      , FctDefComp 30 "Highlight" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias highlight.
      , FctDefComp 30 "HighlightCodeBlock" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias highlightcodeblock.
      , FctDefComp 30 "HtmlEscape" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias htmlescape.
      , FctDefComp 30 "HtmlUnescape" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias htmlunescape.
      , FctDefComp 30 "Markdownify" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias markdownify.
      , FctDefComp 30 "Plainify" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias plainify.
      , FctDefComp 30 "Remarshal" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias remarshal.
      -- urls:
      -- AbsLangURL, AbsURL, Anchorize, JoinPath, Parse, Ref, RelLangURL, RelRef, RelURL, URLize.
      , FctDefComp 31 "AbsLangURL" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias abslangurl.
      , FctDefComp 31 "AbsURL" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias absurl.
      , FctDefComp 31 "Anchorize" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias anchorize.
      , FctDefComp 31 "JoinPath" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias joinpath.
      , FctDefComp 31 "Parse" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias parse.
      , FctDefComp 31 "Ref" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias ref.
      , FctDefComp 31 "RelLangURL" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias rellangurl.
      , FctDefComp 31 "RelRef" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias relref.
      , FctDefComp 31 "RelURL" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias relurl.
      , FctDefComp 31 "URLize" (MonoDef (SgnS (Just (("input", SimpleVT StringST) :| [])) [] Nothing (SimpleVT StringST))) -- alias urlize.
      ]
  in
  fst $ foldl (\(accum, idx) fd -> (Mp.insertWith (<>) fd.fname [(fd, fromIntegral idx)] accum, succ idx)) (Mp.empty, 0) hugoFcts


{-
globalStruct :: CompType
globalStruct = StructVT "GlobalContext" [
   NamedSF "hugo" hugoStruct
   , NamedSF "DURATION" durationStruct
   , NamedSF "menu"
 ]

hugoStruct :: CompType
hugoStruct = StructVT "Hugo" [
  NamedSF "name" (SimpleVT StringST)
  ]
-}
