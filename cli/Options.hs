module Options
where

import Data.Semigroup ( (<>) ) -- needed for GHC 8.0 and 8.2
import Options.Applicative


data TemplateSource =
    TemplateFromFile FilePath
  | TemplateFromStdin


data DataSource = 
    DataFromFile FilePath
  | DataLiteral String
  | DataFromStdin


newtype OutputSpec = OutputSpec FilePath


data Options = RunOptions TemplateSource DataSource TechMode (Maybe OutputSpec)


data TechMode =
  Jinja
  | Hugo
  | PHP
  | Fuddle


parseOptions :: [String] -> IO Options
parseOptions args =
  execParser $ info (options <**> helper) (
       fullDesc
    <> header "canelle - A command-line interface for the Jinja and Hugo template languages."
  )


options :: Parser Options
options = runOptions


runOptions :: Parser Options
runOptions =
  RunOptions <$> templateSource <*> dataSource <*> techMode <*> optional outputSpec

outputSpec :: Parser OutputSpec
outputSpec =
  OutputSpec <$> option str (
      long "output"
      <> short 'o'
      <> metavar "OUTPUT"
      <> help "Output of compiled template"
    )

templateSource :: Parser TemplateSource
templateSource =
  convert <$> option str (
       long "template"
    <> short 't'
    <> metavar "TEMPLATE"
    <> help "Load ginger template from this file"
    <> value "-"
  )
  where
    convert "-" = TemplateFromStdin
    convert f = TemplateFromFile f


dataSource :: Parser DataSource
dataSource =
  dataFromFile <|> dataLiteral


dataFromFile :: Parser DataSource
dataFromFile =
  convert <$> option str
        ( long "data-file"
        <> metavar "DATAFILE"
        <> help "Load JSON or YAML data from this file (`-' to read from stdin)"
        )
  where
    convert "-" = DataFromStdin
    convert f = DataFromFile f

dataLiteral :: Parser DataSource
dataLiteral =
  DataLiteral <$> option str
        ( long "data"
        <> short 'd'
        <> metavar "DATA"
        <> help "Use specified (JSON or YAML) DATA"
        <> value "{}"
        )


techMode :: Parser TechMode
techMode =
  flag' Jinja (
      long "jinja"
    <> short 'j'
    <> help "Use the Jinja template engine"
  )
  <|> flag' Hugo (
       long "hugo"
    <> short 'g'
    <> help "Use the Hugo template engine"
  )
  <|> flag' PHP (
       long "php"
    <> short 'p'
    <> help "Use the PHP template engine"
  )
  <|> flag' Fuddle (
       long "fuddle"
    <> short 'f'
    <> help "Use the Fuddle template engine"
  )
