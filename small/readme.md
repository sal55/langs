## Small Language for Parsing Demo

This toy language consists of a series of two statement types:

 let name = expr
 print expr

There are no loops, conditional statements or jumps; it's a basically a
calculator. It's to make it as simple as possible, yet still have something useful.

### Informal Grammar

I will implement it using my normal approach, which does not need a formal
grammar. But I will give an informal one (in an informal, half made-up version
of BNF) to give a better idea of what the syntax is.

#### Grammar
````
program    = {statement Newline}* Eof

statement  = letstmt
           = printstmt

letstmt    = Let name Equals expr

printstmt  = Print expr

expr       = factor||{Plus/Minus}

factor     = term||{Times/Divide}

term       = Number
           = Name
           = Minus term
           = Lbrack expr Rbrack
````

#### Lexical Tokens (capitalised terms)
````
Let        'let'
Print      'print'
Equals     '='
Plus       '+'
Minus      '-'
Times      '*'
Divide     '/'
Lbrack     '('
Rbrack     ')'
Number     Any integer or floating point literal
Name       Any identifier that isn't a reserved word
NewLine
Eof
````
-----------------------------------------------------

**Notes**

* Multiple "=" lines give alternatives

* A/B/C are inline alternatives

* A|B mean zero or more instances of A separated by B
* A||B mean one or more instances of A separated by B

* A* means zero or more instances of A (A can be {...})

* {...} group terms

### Example Program
````
let b = 2
let c = 3
let d = 4

let a = b+c*d

print a
````


### The Lexer or Tokeniser

Here I will assume the existence of a tokeniser that delivers the tokens listed
above (I've no idea how it works with parser generators).

Some token types (Number and Name here) will have a value attached.
