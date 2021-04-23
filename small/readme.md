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

powerterm  = term||powerterm               # (precedence is right to left)

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
* \[A\] means A is optional

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

Some token types (Constant and Name here) will have a value attached.
