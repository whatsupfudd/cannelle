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


data Options = RunOptions Int DataSource TechMode (Maybe OutputSpec) TemplateSource


data TechMode =
  Jinja
  | Hugo
  | PHP
  | Fuddle
  | Tsx
  | Templog


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
  RunOptions <$> debugFlags <*> dataSource <*> actionSpec <*> optional outputSpec <*> templateSource


debugFlags :: Parser Int
debugFlags =
  flag 0 1 (
      long "debug"
    <> short 'd'
    <> help "Enable debug mode"
  )

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
  convert <$> strArgument (
    help "template file location (or '-' for stdin)"
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

actionSpec :: Parser TechMode
actionSpec =
  subparser $ command "parse" (info (techMode <**> helper) (
      progDesc "Parse a template engine file"
    ))

techMode :: Parser TechMode
techMode =
  subparser $ command "jinja" (info (pure Jinja) (progDesc "Use the Jinja template engine"))
    <> command "hugo" (info (pure Hugo) (progDesc "Use the Hugo template engine"))
    <> command "php" (info (pure PHP) (progDesc "Use the PHP template engine"))
    <> command "templog" (info (pure Templog) (progDesc "Use the Templog template engine"))
    <> command "tsx" (info (pure Tsx) (progDesc "Use the TypeScript-TSX template engine"))
    <> command "fuddle" (info (pure Fuddle) (progDesc "Use the Fuddle template engine"))
