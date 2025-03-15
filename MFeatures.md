## M Language Summary

### Overview

'M' is a lower level systems language. It is roughly at the level of C,
but has a quite different syntax, and many modern conveniences.

It has been around a long time, and has been implemented on one platform at time, from 8-bit microprocessors through 3 generations of x86, evolving along the way. It was an in-house tool and now is a personal language. 

The current implementation is for x64 running Windows. M programs used to run on Linux via a transpiler tool, but that has fallen out of use.

### The 'Q' Language

This is a companion, dynamic and interpreted scripting language. This not documented, however it shared pretty much the same syntax as 'M'. Most of what's described below applies to both. (Q has optional variable declarations inside functions, and has some higher level, more flexible data types.)

### Highlights

* Whole-program compiler. This affects the language design
* 64-bit-based (64-bit default `int` and float types and promotions)
* N-based, defaults to 1-based mostly
* Case-insensitive
* Non-brace syntax
* Expression-based
* Module scheme; no external build system needed
* Fast, single file, self-containing compiler, includes run-from-source as native code or via interpretation

### Syntax

This was heavily inspired by Algol68, but with a number of differences, like no keyword stropping or embedded whitespace in identifiers. It is now evolved into its own style.

Keywords like `fi od esac` are still used, but conventional `end` keywords can be used too. There are no `begin...end` blocks.

Braces are not used at all . `{...}` is reserved for anonymous functions; not used in M, but they appear in Q.)

See various examples around this site. M source files have extension ".m", or ".ma" for amalgamations of source files.

### Hello, World
````
proc main =
    println "Hello, World!"
end
````
Here's a meatier example:
````
proc main =
    for i to 20 do
        println i, sqrt i
    od
end
````
Demonstrates:
* No standard libraries are needed for language features
* `proc` is used for functions with no return value, otherwise it is `func`
* One form of `for` loop, which here starts from 1 if not specified
* `For`-loop indices can be automatically declared (if so, they are also read-only)
* `sqrt` is a built-in, and is an operator, hence parentheses are optional
* Automatic conversion from float to integer
* `println` is built-in, and needs no format strings or codes
* It will add spaces between print items (`,,` will suppress them)
* The `od` shows the Algol68 influence, but `end`, `end do` or `end for` will all work too

### Case-insensitive


### Unicode Support and character encoding

### Line-orientation and Semicolons

### Comments and Doc-strings

### Header files and Standard Library

Module Scheme

Declarations and Definitions

Out-of-Order

Scope Control

Include

N-Based

Expression Based

Lvalues

Numeric Types

Pointers

Array Types

Record Types


Self-referential types

Bitfields in Records

Type Aliases

Static

Namespaces

Min/Max Values

Sizeof and Bounds

Named Constants

Enumerations and Table Data

Encapsulation

Mixed Arithmetic and Promotions

Type Conversions and Type-Punning

Extra Operators (min max abs sqr maths divrem)

'In' operator

'$' Syntax

= and :=

Swap

Chained Comparisons

Augmented Assignment

Bit and Bit-field indexing and slicing; A.msb etc

Numeric Literals

String Literals and Raw Strings

Character Literals

Data Strings and Embedding

Data Initialisation

Operator Precedences

Two-way, N-way and other Selections

Multiple Assignments

Pointer Derefencing and Address-of

Piping

Function Syntax

Pass-by-reference

Keyword arguments and Default parameter values

Multiple function return values

Slices

Type Reflection

Function Tables

Entry Point

'Start' Functions

Termination

Expression List ('Comma Operator')

Array Indexing

If Statements

Loops

Loop Control

Switch and Case

Looping Switch and Case

Composite if/switch/case

Control Flow statements

Label Pointers and Computed Goto

Macros

Compiler Variables (including Pi)

Numeric suffixes (million)

ImportDLL

FFI

Equivalence

Read and Print

Inline Assembly

LIBFFI

Whole-program Compiler and Build System

Source Amalgamations

Run-From-Source

C Features that don't exist

VLAs

Compound Literals

Designated Initialisers

Octal Literals

A bunch of C23 stuff

Variadic functions

