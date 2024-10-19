{-|
A Haskell implementation of the <http://jinja.pocoo.org/ Jinja2> template
language.

Ginger aims to be as close to the original Jinja language as possible, but
avoiding blatant pythonisms and features that make little sense outside of
an impure dynamic host language context, especially when this would require
sacrificing runtime performance.

GoHugo aims to be 100% compatible with the Hugo template system.

-}

module Text.Ginger
(
-- * Template Syntax
-- ** Minimal example template
-- | > <!DOCTYPE html>
--   > <html>
--   >     <head>
--   >         <title>{{ title }}</title>
--   >     </head>
--   >     {# This is a comment. Comments are removed from the output. #}
--   >     <body>
--   >         <menu id="nav-main">
--   >         {% for item in navigation %}
--   >             <li><a href="{{ item.url }}">{{ item.label }}</a></li>
--   >         {% endfor %}
--   >         </menu>
--   >         <div class="layout-content-main">
--   >             <h1>{{ title }}</h1>
--   >             {{ body }}
--   >         </div>
--   >     </body>
--   > </html>

-- | There are two kinds of delimiters: @{% ... %}@ and @{{ ... }}@. The first
-- one is used to execute statements such as for-loops or assign values, the
-- latter prints the result of an expression to the template.
--
-- Both kinds of delimiters support adding a dash (@-@) on either side or both,
-- putting them in whitespace-eating mode on the respective side.
-- Whitespace-eating means that any whitespace that precedes cq. follows the
-- delimited construct is removed, including newlines. For example,
-- @A {{ "b" }}@ renders as @\"A b\"@, but @A {{- "b" }}@ renders as @\"Ab\"@.
-- Note that whitespace /inside/ delimiters is never printed; the dash only
-- ever removes whitespace on the outside.

-- These delimiters can be changed on the Haskell side. In principle, any
-- string is accepted for any delimiter; you may, however, get surprising
-- results if you pick delimiters that clash with other Jinja syntax, or with
-- one another (e.g., using the same string to start interpolations and flow
-- control constructs will not work). See the 'ParserOptions' and 'Delimiters'
-- data structures for more details.

-- ** Variables
-- | You can mess around with the variables in templates provided they are
-- passed in by the application. Variables may have attributes or elements
-- on them you can access too. What attributes a variable has depends
-- heavily on the application providing that variable.

-- | You can use a dot (@.@) to access attributes of a variable, but
-- alternatively the so-called “subscript” syntax (@[]@) can be used. The
-- following lines do the same thing:

-- | @
-- {{ foo.bar }}
-- {{ foo[\'bar\'] }}
-- @

-- | It’s important to know that the curly braces are /not/ part of the
-- variable, but the print statement. If you access variables inside tags
-- don't put the braces around them.

-- | If a variable or attribute does not exist you will get back an undefined
-- value. What you can do with that kind of value depends on the
-- application configuration: the default behavior is that it evaluates to
-- an empty string if printed and that you can iterate over it, but every
-- other operation fails.

-- ** Expressions
-- | Variables aren't the only thing that can go inside @{{ ... }}@; any valid
-- Jinja expression can be used, and expressions can be constructed in many
-- different ways. Note that all expressions are case sensitive: @null@ and
-- @Null@ are not the same thing.
-- Currently, the following constructs are available:

-- *** Simple (\"Atomic\") Expressions
-- **** Variable reference
-- | Look up a variable in the current scope and return its value.

-- | > {{ username }}

-- **** String literals
-- | A constant string. Strings can be single- or double-quoted.

-- | > {{ "Hello, world!" }}

-- **** Numeric literals
-- | Numeric literals can be given in integer or decimal format:

-- | @
-- {{ 21 }}
-- {{ 44.5 }}
-- @

-- **** Boolean literals
-- | There are two boolean values, @true@ and @false@. You will not normally
-- need to use them, as they are produced by boolean expressions such as
-- comparisons, but they can be useful occasionally.

-- | @
-- {{ true }}
-- {{ false }}
-- @

-- **** The Null literal
-- | Represents the special /null/ value, which is used to signal the absence
-- of a result or value. Looking up non-existent scope variables, for example,
-- will produce Null.

-- | > {{ null }}

-- *** Data Structures
-- **** List Literals
-- | A list literal consists of a comma-separated list of expressions between
-- square brackets:

-- | > {{ [ foo, "bar", 1234 ] }}

-- | Lists can contain any kind of expression, including other lists, so you can
-- nest them:

-- | > {{ [ foo, [ 1, 2, 3 ], "baz" ] }}

-- **** Object Literals
-- | An object is an unsorted key/value container, declared like this:

-- | > {{ { "foo": "bar", "baz": "quux" } }}

-- | Objects, like lists, can contain any kind of value. The keys, however, are
-- restricted to strings; you can define them using any expression you like,
-- but they are always converted to strings when inserting items into the
-- container. This means that the following is valid:

-- | > {{ { ["foo", 1]: "bar" } }}

-- | It will, however, produce the same object as the following:

-- | > {{ { "foo1": "bar" } }}

-- | That's because converting @[\"foo\", 1]@ to a string produces @\"foo1\"@, and
-- that is used as the key.

-- *** Parentheses
-- | Parentheses can be used to group expression constructs to override default
-- precedence. Excess parentheses are simply ignored.

-- *** Unary operators
-- **** Function calls
-- | When an expression evaluates to a function, you can call it using a list
-- of arguments between parentheses. Most of the time, the function itself just
-- comes from a scope variable, so a function call typically looks like this:

-- | > {{ print(x, "hello!") }}

-- | However, anything that returns a function can be called as a function:

-- | > {{ system.console['log']("hello!") }}

-- **** Filter expressions
-- | Any function that takes at least one argument can be called through the
-- alternative filter syntax instead:

-- | > {{ x|print }}

-- | This is equivalent to:

-- | > {{ print(x) }}

-- | Filters can take arguments:

-- | > {{ x|append("foobar") }}

-- | This is equivalent to:

-- | > {{ append(x, "foobar") }}

-- | A list of available filters can be found in the 'Cannelle.Jinja.Run' module.

-- | /Deviation from Jinja2:/ there is no distinguishing between filters and
-- functions at the semantics level; any function can be called as a filter,
-- and vv., and the filter syntax is merely a syntactic variant of a function
-- call. And there isn't real differences between functions and values,
-- either: a function is a value that happens to be callable; other than that,
-- functions live in the same namespace as any other value, and if you bind
-- a function to a context value on the host side, you can use it just like
-- any other function / filter.

-- ** Control Constructs
-- *** Conditionals: `{% if %}`
-- | Simplest form:

-- | > {% if condition %}Hello!{% endif %}

-- | Slightly more elaborate:

-- | > {% if condition %}Hello!{% else %}Goodbye!{% endif %}

-- | Full glory:

-- | > {% if condition %}Hello!{% elseif otherCondition %}Sayonara!{% else %}Goodbye!{% endif %}

-- *** set
-- | TODO
-- *** for
-- | TODO
-- *** include
-- | TODO
-- *** macro
-- | TODO
-- *** block
-- | TODO
-- *** call
-- | TODO
-- *** scope
-- | TODO

-- * Haskell API
-- ** General
-- | On the Haskell side of things, executing a template is a two-step process.
-- First, template source code is parsed into a 'Template' data structure,
-- which is then fed to 'runGinger' or 'runGingerT'.

-- **  Parsing
-- | Because Jinj templates can include other templates, the parser needs a way of
-- resolving template names. Instead of hard-wiring the parser into 'IO' though,
-- The Ginger logic will work on any Monad type, but requires the caller to provide a
-- suitable template resolver function. For 'IO', the resolver would typically
-- load a file from a template directory, but other monads might have access to
-- some sort of cache, or expose template compiled into a program, or simply
-- return 'Nothing' unconditionally to disable any and all imports. A suitable
-- example implementation for 'IO' would look like this:

-- | > loadFile fn = openFile fn ReadMode >>= hGetContents
-- >
-- > loadFileMay fn =
-- >     tryIOError (loadFile fn) >>= \e ->
-- >          case e of
-- >             Right contents ->
-- >                 return (Just contents)
-- >             Left err -> do
-- >                 print err -- remove this line if you want to fail silently
-- >                 return Nothing

-- | (Taken from @cli/CannelleCLI.hs@). This interprets the template name as a
-- filename relative to the CWD, and returns the file contents on success or
-- 'Nothing' if there is any error.

-- | If you don't need a monadic context for resolving includes (e.g. because you
-- have pre-loaded all template sources), you can use the pure 'parseGinger'
-- flavor, which does not rely on a host monad.
module Text.Ginger

-- ** Running
-- | The core function for running a template is 'runGinger' (or its monadic
-- flavor 'runGingerT'); in order to pass an initial context to the template
-- engine, pass a suitable 'GingerContext', which you can create using the
-- 'makeContext' / 'makeContextM' functions.

-- | An example call (for running a template in 'IO') would look something like
-- this:

-- | > runGingerT (makeContextM scopeLookup (putStr . Text.unpack . htmlSource)) tpl

, module Cannelle.Jinja.Run

-- ** Other concerns
-- *** GVal: Ginger's unitype value
, module Cannelle.Jinja.GVal

-- *** AST
-- | The data structures used to represent templates, statements and
-- expressions internally.
, module Cannelle.Jinja.AST

-- *** Optimizer
-- | An optimizing AST rewriter
, module Cannelle.Jinja.Optimizer
)
where
import Cannelle.Jinja.Parse
import Cannelle.Jinja.Optimizer
import Cannelle.Jinja.AST
import Cannelle.Jinja.Run
import Cannelle.Jinja.GVal
