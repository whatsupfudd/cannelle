# Ginger


# Cannelle

A template processing toolkit that supports the Jinja, Hugo, Fuddle and PHP syntaxes for efficient generation of textual content.

## Introduction

*A template - A pattern or gauge used as a guide in making something accurately.*

[The Template Engine](https://en.wikipedia.org/wiki/Template_processor) is an approach to document generation from the rule-based insertion of data into patterns that has been around for a long time.  Anyone remembers the report and mail-merge templates of the old Lotus 1-2-3/dBase softwares?

The Cannelle package provides a powerful set of tools to assemble of small patterns of text and data based on a driving logic.  The patterns can be any textual content, for example Haskell code. But Cannelle is usually applied to web
documents and there are some additional facilities to integrate with web technologies such as [Daniell](https://whatsupfudd.com/projects/daniell/) and [EasyWordy](https://whatsupfudd.com/projects/easy-wordy/). The logic is expressed with:

  - [Hugo's extension of Golang's text/template](https://gohugo.io/templates/)
  - [Jinja](https://jinja.palletsprojects.com/)
  - [Fuddle](https://whatsupfudd.com/projects/fuddle/)
  - [PHP](https://www.php.net/)

Cannelle provides a universal virtual machine that runs the template logic and assembles data into the patterns to produce the final output at high speed.

Cannelle also creates routing rules to provide the templates within a navigation/hypertext context.

The Cannelle package recycles the [Ginger template engine](https://github.com/tdammers/ginger) and adds the additional syntaxes, the VM and a serialization of the parsed templates.

## Usage

The API of Cannelle is simple: 

 - call a **process** function of one of Cannelle's syntax parser with the path of the template and the path of the resulting processed format, and it transforms the source file into a verified and optimized representation.
 - call the **render** function with the path of the processed template, the data context and the path of the output file to generate the output.
 - use the variations of these two functions to read and write to different type of data streams (files, strings, etc.)


## Components
The Cannelle package is composed of the following components:

- Fuddle: for templates using the extended Fuddle syntax,
- Hugo: for templates using Hugo's Golang text/template syntax,
- Jinja: for templates using the Jinja syntax (recycled from Ginger),
- PHP: for templates using the PHP syntax,
- VM: a virtual machine specialized in the generation of textual content from assembly of templates and data,
- InOut: a serialization of the parsed templates that is efficient for transport and execution.

