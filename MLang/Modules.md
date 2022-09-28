## Module Import Scheme

(Discusses how this is done in reference to my `M`  and `Q` languages using `mm.exe` and `qq.exe` compilers.)

Modules are typically dealt with by explicitly importing them like this:

    import A
    import B
    import C

This is done at the top of each module, with each being a different set of `imports` depending on which other module's exports are needed.

This was how my previous scheme worked as well. But it looked dreadful: each module started with a ragtag 'shopping list' of modules, that was different from every other, and that needed constant maintenance as code changed: new symbols were imported, some were deleted.

Also, it didn't really work when a group of related modules wanted to share entities, but which they wanted to keep private from other modules.

### My 2022 Module Scheme

Here, the entire module structure is specified in one place, at the top of the first module, called the Header Module.

For simplicity, it can be assumed for now that that module only contains such information, no code. So it might look like this in a header module `P.m`:

    module A
    module B
    module C
    module D

for a program which uses modules `A.m`, `B.m`, `C.m` and `D.m`, plus the header `P.m`. No `module` nor `import` statements appear anywhere else. Such a program is compiled as:

    mm P

which produces `P.exe`. Each module effectively imports every other. Anything marked as `global` (or `export` - see later) is visible from every other module. You don't even need to qualify them, unless there are ambiguities.

There are no straggly lists of imports that need constant maintenance. Program structure can be discovered, by both compiler and human reader, by
glancing at these few lines in the lead module, rather than have to look at and follow a web of import links across dozens of modules.

If a module name changes, you change one line in the header module. (Because of not needing qualifiers, usually no code needs changing either.)

By itself, this is a big improvement on the previous scheme, or how it seems to be done in other languages. However there is a bit more to it as explained below.

Still, most of my applications are basically just lists of modules.

### Subprograms

A further refinement is that modules can be grouped into Subprograms. Effectively, a program consists of a collection of Subprograms, and a Subprogram is a collection of modules.

But for simple programs such as my example above, that first collection of modules is assumed to be a Subprogram called `P`.

Modules within each subprogram can see other's globals, but they cannot see globals in other subprograms' modules. To export from a subprogram requires `export` rather than `global`.

Further, while module names themselves are not exported from a subprogram; they are not visible from another subprogram.

If module B.m of my above program defined `export func Foo` for example, it would need to be called as `P.Foo()` not `B.Foo()` if accessed from another subprogram. (In practice, qualifiers are not needed here either, unless there are clashes. M is very lax and informal here.)

So, subprograms can be used to directly incorporate libraries consisting of a collection of modules, without needing to expose everything that is shared across that library.

If a library comprised modules Q.m, X.m. Y.m and I wanted to use it from `P`, I can add this line:

    import Q

or I can enumerate the modules individually (use of `import` requires that Q.m only contains directives):

    subprog Q
    module X
    module Y

The library itself can be compiled into a separate library:

    mm -dll Q             # produces Q.dll (plus an export file, containing its API)

Within a top-level Subprogram like Q here, the `export` attribute will export functions and variables from the library, so that
they are accessible to other programs. (I'm working on exporting also types, enums, macro and so on, but that will be done via the export file.)

### The Standard Library

This is a collection of 5 or so modules, which are usually compiled together with user-written modules. But the compiler will automatically add the necessary header directives.

If it didn't do that, then my example above would have needed to add either this to the header module:

    syssubprog mlibx
      sysmodule msys
      sysmodule mlib
      sysmodule mclib
      sysmodule mwindows
      sysmodule mwindll

Or this:

    sysimport mlibx          # mlibx.m is a header module containing those same `sysmodule` lines

The `sys` prefix to these directives just changes where it looks for those modules (either inside the compiler, or in the development folder).

This is also an example of a subprogram: user programs only see function names, variables etc from this standard library that are explicitly exported usng `export`.

So, the above example program is now seen to consist of two Subprograms, 'P' and 'mlibx', which is typical for most of my apps.

### Header Modules

I've said that header modules containing only module directives, not code. This was my original intention, and how my main applications are structured.

Simple programs of one module contain only code. But after feedback from Reddit users when I first posted about this scheme towards the end of last year, this was relaxed.

The Lead Module, the one submitted to this whole-program compiler, can contain header directives followed by normal code.

Although this is a little problematical; that module name, say `P`, is used for both the primary Subprogram name `P`, and the name of the first module 'P'. But Subprogram and Module names cannot clash, because they both create separate namespaces.

(The solution used is to internally rename the main module as `$P`, which is usually not seen as module names are rarely used. But it can be seen in error messages.)

### Header Modules as Stub Modules

Header-only modules are used in my main applictions, because it is easy to build different configurations of a program, with slightly different collections of modules as the lead 

An example I use is `mm.m`, for the regular M compiler, and `mc.m` for the C-targetting M compilerw, which contain only module directives. The compiler will build either `mm.exe` or `mc.exe`.

There are conditional directives as listed below, but I haven't really used these yet. I will start to use them more as I once again deal with programs that might target C or work on Linux.


### Header Directives

**Principle directives** Here `A B C` etc are examples of different names, which must be valid identifiers:

    module  A            Add a new module A to the list of modules belonging to the subprogram
                         Create a module namespace A
                         The corresponding source file will be A.m in the current module search path

    subprog B            Start a new subprogram B. Subsequent modules will belong that.
                         Create a subprogram namespace B. There is no source file associated with this

    import  C            Start a new subprogram C
                         Creat a subprogram namespace C
                         Read further directives from source file C.m in current subprogram search path

    linkdll D            Add D.dll to the list of imported DLLs
                         Can also specify as "D", or a header-variable

    linklib E            Add E.ml to the list of imported ML library files (ML is my version of DLL)

    modulepath expr      Use this path as search path for subsequent modules
    headerpath expr      Use this path as search path for subsequent imports.
                         These two can also be done using `setvar`


(M applications already link to these DLLs on Windows: `msvcrt gdi32 user32 kernel32` automatically.)

**Special directives:**

    sysmodule  H         These work like the above but use a special search path
    syssubprog I         (The standard library is imported automatically so these are little used)
    sysimport  J

    setvar  $A = expr    Set header variable (eg $A) to a name, string or other header variable
                         There are three user variables $A $B $C that can be assigned to
    showvar $mmpath      Display a header variable while compiling

**Options** that can follow some directives:

    module name as alias       Provide an alternative name to the module
    sysmodule name as alias

    module name when expr

That `expr` can be a string constant or one of these predefined header variables:
````
     $devpath            (Path used for M compiler development: internal)
     $mmpath             Path where the invoked M compiler resides
     $headerpath         Both of these are set to path of the lead module submitted to compiler/interpeter
     $modulepath         They can be separately overwridden using `headerpath` and `modulepath` directives

     $ctarget            All set to "" or "1", depending on compiler and options
     $windows
     $linux
     $optim
     $a, $b, $c          All set to "", can be changed with setvar
````

### Conditional Directives

The `module` directive can be written conditionally:

    module name when expr

Here, `expr` will be a header variable, one that is either `""` (false) or not `""` (true). I haven't tried this yet, it would be used like this:

    module winmod as osmod when $windows
    module linmod as osmod when $linux

So, include either `winmod.m` or `linmod.m` depending on whether it's for Windows or Linux. And alias either choice as `osmod`, to give a consistent way to qualify names imported from that module, if ever needed.

But at the moment, I tend to use commenting for conditional code:
```
    module mm_diags              # With diagnostics
#   module mm_diags_dummy        # Without
```
Comment or uncomment one line or the other. This is another advantage of specifying a particular module in only one place.

### Modules and the File System

I like to keep these separate. Ideally there is no mention of files or paths at all in my header information.

There is an abstract module name, say `A`. The source file associated with that will be `A.m`. But the location of that file depends on the input given to the compiler. The same location will be used for other discovered modules, unless overridden.

So, sometimes explicit paths *are* specified, via the `modulepath` and `headerpath` directives. But there will always be exactly one place where it will look for a module.

Older schemes had a stack of possible places, so you could never be sure what it was loading. Maybe several search locations had a matching file; which one will it pick up?

The rules for system files (standard library, anything using `sysmodule` etc) are a little different, partly such files are often *inside* the compiler.
