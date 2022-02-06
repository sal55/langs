## New 'B' Language

This is an experimental project to design a new language which combines my static M language and scripting Q language.

It has the working title 'B'. (If it manages to replace 'M', then it will be renamed M).

### Update

This is an update after 2 weeks. Quite a lot about the project has changed:

* I started with a copy of the compiler for M language
* That was changed to compile new language 'B'
* A big overhaul was done on that compiler to:
  * Reintegrate the 'PCL' intermediate language into the main compiler, so they no longer have their own STs etc
  * Add in type designations needed for the variant types of Q
  * Use new syntax for functions, to allow informal Q-style definitions (more below)
  * Did a few more syntax tweaks
  * Changed how main/start/$init functions work
  * Remove 128-bit integer support (128-bit types likes slices remain)
* There will be no interpreter option. There just wouldn't have been enough difference between run-from-source options of bytecode and native code
* Overhauled the routines for generating ML/MX files and running in-memory; now more code is shared
* I've given version numbers to my M languages. M4 was the current one, and M3 before that. This new one, codenamed 'B', will be M5

At all points, I get it working on my main applications, however I've had to make copies of the M4 versions, and do the tweaks necessary for M5.

I'm now using the new compiler (still called BB, but the language is now M, except it's version M5) for production work.

At this point, it doesn't do much more than before; it still compiles a lower level statically typed language. Provision has been made for dynamic and higher level types, and for writing more dynamic code, but it's not yet working:

* Code with variant types won't work beyond pass AST2 (AST3 does the type handling)
* Code derived from Q won't work beyond AST1 or AST2. But it can be parsed with the correct syntax adjustments

These will be worked on gradually. The project is ambitious! I also need to figure out what to do with libraries, which will expand from 1 to 3:

* **mlib** was the small library that came with M
* **vlib** is the substantial library need to implement the dynamic and higher level types
* **glib** is the library that currently goes with Q, and a provides console, graphics and GUI capabilies

### M5 Differences from M4

* Static functions had been defined with **function proc** in both languages. Now typed functions will use **function func procedure proc**, while dynamic ones, where types are optional, will use **fun sub**
* Inside **fun sub** functions, local variable declarations are not needed
* I had used a special **start** function on the lead module as the entry point. Special **$init** functions were automatically called in each module. **main** remained a possible entry point, but rarely used. New rules are as follows:
  * The primary entry point is the **main** function, in the lead module, mandatory unless module-level code is present.
  * All modules can have an optional **start** function containing implicit and explicit start-up code, and executing any module-level code, automatically called and replacing **$init**. (A missing start() function will be added if needed.)
* Code outside a function is now allowed (but in a limited way). If present, it will be added to the **start** function of the module and called automatically
* The prefixes **var mut let** had already existing, but rarely used. **var mut** are the same thing. **var** will feature more heavily:
* A declaration such as **var a, b, c**, with a missing type, now defaults to **variant** type, not **auto** as before (which was not implemented anyway). This is the syntax used in Q to declare variants
* Variants can be declared explicitly using any of:
```
    var variant a, b, c
    variant a, b, c
    string a, b, c         # at present strings etc are implemented as variant types
```
* Variant types can hinted when they are expected to contain a certain type:
```
    var (point) a, b
    var point a, b             # when 'point' is a variant record
    point a, b
    var (string) a, b
    string a, b
    var (int) a, b             # Variant containing an int
```
The (T) syntax is only really needed when the type also exists in static forn. If T is a variant type anyway (like a user-record or a string), it ca be used like a regular type
* **minclude** can be used in header sections (use of **include** is troublesome)
* **include** hadn't worked anyway. It now does, but cannot be at the boundary between header and module code, which also means at the top of a module. A dummy declaration may be needed just before
* An experimental piping operator has been added, in a crude version that only allows one argument:
```
    f(g(x+1))          # can now also be written as:
    x+1 -> g -> f      # how the lowest precedence so no need to write (x+1)
```
* **{ }** braces, used for one-line functions, are no longer used. (They are up grabs at the minute.) One line functions can be written like this (here using dynamic style):
```
    fun triple(x) = x*3
```
* Set constructors **\[...\]** are now allowed, but these clash with array declarations with also start with **\[**. So an array declaration can't start with an open bracket:
```
    [10]int A            # Not allowed unless in one of the following forms:
    global [10]int A     # Also export, static 
    var [10]int A        # Also mut, let
    ([10]int A)          # (Parameter list)
    array [10]int A      # Or use this if stuck
``` 
* MM already had run-from-source, and BB has the same:
```
    BB -run prog        # compile into memory and run from there
```
But BB will have a feature where if the executable is called, for example, BX instead of BB, then it will use -run as the default option:
```
    BX prog
```
I'm still finalising this, and haven't decided to EXE names that will be trigger a specific option. It may be rolled out to other options too.


