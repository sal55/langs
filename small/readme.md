## Small Language for Parsing Demo

This toy language consists of a series of two statement types:
````
 let name = expr
 print expr
````
There are no loops, conditional statements jumps or means to do input; it's to make it as simple as possible, yet still have something that can perform a task, and which has a syntax.

### Update

The syntax used in the source codes has been updated, and also reflected below. This may be one disadvantage of not having an official grammar description that drives the source code, since they will usually be out of step.

I've allowed blank lines; comments; power ops (\*\*), made Print optional (this itself is optional if it's too fiddly); and turned Number into Const, which can be strings too.

### Informal Grammar

I will implement it using my normal approach, which does not need a formal
grammar. But I will give an informal one (in an informal, half made-up version
of BNF) to give a better idea of what the syntax is.

**Grammar**
````
program    = Newline* {statement Newline+}* Eof

statement  = letstmt
           = printstmt

letstmt    = Let name Equals expr

printstmt  = [Print] expr||,

expr       = addterm

addterm    = multerm||{Plus/Minus}

multerm    = powerterm||{Times/Divide}

powerterm  = term {Power powerterm}*       # (precedence is right to left)

term       = Constant
           = Name
           = Minus powerterm               # (parse -a**b as -(a**b) rather than (-a)**b
           = Lbrack expr Rbrack
````

**Lexical Tokens (capitalised terms)**
````
Let        'let'
Print      'print'
Equals     '='
Plus       '+'
Minus      '-'
Times      '*'
Divide     '/'
Power      '**'              # (or '^' if you prefer)
Lbrack     '('
Rbrack     ')'
Constant   Integer, float or string literal (for dynamic host language; can simplified for static language)
Name       Any identifier that isn't a reserved word
NewLine
Eof

Comment    (Not visible to parser) '#' marks start of a line-comment

````
-----------------------------------------------------

**Notes**

* Multiple "=" lines give alternatives

* A/B/C are inline alternatives

* A|B mean zero or more instances of A separated by B
* A||B mean one or more instances of A separated by B

* A* means zero or more instances of A (A can be {...})

* A+ means one or more instances of A

* {...} group terms
* \[A\] means A is optional (0 or 1 instance of A)

### Example Program
````
let b = 2
let c = 3
let d = 4

let a = b+c*d

print a
````
### My Implementation

Dynamic scripting language:
````
parser.q      Parser and driver function (see start())
lex.q         Tokeniser
run.q         Interpreter (makes use of features of the host language)
````
Static systems language port:
````
parser.m      Parser and driver function (see start())
lex.m         Tokeniser
run.m         Interpreter
````
The scripting version works with Bignums, Bigfloats and String values. It's set up to use a REPL (switching to read from a file is trivial).

The static version uses only 64-bit integers as values. I think it's set up to read from a hard-coded filename.

The main part of this project is comparing the parser code. The driver function and lex module is just to provide suitable input (a stream of tokens).
The run module is to verify it works.

