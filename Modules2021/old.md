## Module System IV (2021 version)

My current module system cannot easily cope with groups of modules that can form parts of different projects. It becomes very messy.

This scheme IV will be subprogram based and project-driven. It is again intended for whole-program compilers.

### Terms

**Program** A set of one or more Subprograms, which when built forms a single executable file like a .EXE.

**Subprogram** A set of one or more Modules. One module is designated the Header Module.

**Module** A single source file forms one module. 

**Header** This is an information block at the start of a Header Module. It lists the modules in this Subprogram, any Subprograms it imports, and any names that are exported from the Subprogram.

**Header Module** The main or only Module of a Subprogram, the one expected to contain any Header.

**Stub** A Header Module that only contains a Header, so no functions, variables or other code, is also called a Stub.

**Library** This is a Program that is built into a shared library, such as a .DLL file. For this purpose, the main Subprogram needs to export function names to allow access from other programs.

**Program Module** The lead/main/header module of the main Subprogram. This is the key module that contains the top level header info, that is used to locate all other modules,
and that is submitted to a compiler, or read by an external tool.

### A Header Block

This goes at the beginning of a Header Module, or in a Stub. (I haven't decided whether header blocks inside another module should be an error, or just ignored.)

It contains Module, Import and Export declarations, and can optionally be wrapped in a Header ... End block. (Optional because that would be too heavy for small programs.)

**Module**
````
    module A
    module B as Z
````
This lists the modules comprising the Subprogram. The header module is always included, but can be listed anyway.

**Import** Import specifies any Subprograms that are used by this one:
````
    import mlib
````    

**Export** Export lists names exported from this Subprogram, to other Subprograms (or other Programs, if forming a DLL):
````
    export pcl_start
    export pcl_end
````
Export additional can export:

* A whole module; every global name in that module is exported
* A subprogram: if this subprogram is S, which imports T, when *export T* is equivalant importing both S and T when any subprogram imports S.
* A DLL block: imports from DLLs are defined inside an **importdll** block, which are automatically global - visible from other modules. This will export then names from that block. (Possibly, this can be handled by being able to export the whole module. It means I can't have multiple importdll blocks in the same module, and selectively exporting some.)

**Importlib** This specifies the names of DLLs:
````
    importlib opengl           # .dll is optional
    importlib opengl.dll
    importlib "5lib"           # when the DLL name is not a legal identifier
````
All DLL from all subprograms (duplicates are merged) are collected together program-wide. The compile ensures the DLLs are imported into the final executable.

**Note:** **Export** and **Importlib** will clash with uses outside of a header, will need alternatives. Insisting on putting them inside a Header..End block is unsatisfactory.

### Namespaces, Attributes and Scope

* All modules in a Subprogram automatically import each other; it just needs the modules listed in the header
* Each module name creates a namespace used for access between modules
* To share a name of a function, variable, named constant, enum, type or macro, a **global** attribute, eg. *global function ...*.
* Shared names, eg. F in module A, can be used without qualifiers, as *F*, or can be qualified with *A.F*, in case of a clash (two modules use the same Global name); or to override a more locally scoped name.
* The name of the header module also forms a separate namespace, when imported into another Subprogram. That allows that Subprogram access to this one's exports. Again, qualifying the name is not necessary unless there's a clash.

Example: this Subprogram comprises the modules S, T, U, with S as the header module. T has a global function F which is also an export. F can be accessed anywhere within the subprogram as F, or T.F to be explicit.

But from outside, it will be F, or S.F to be explicit. Modules T and U are unknown outside.

I may need to allow, as well as **global**, an **export** attribute. Some subprograms may have a lot of names to export, and it is extra effort, and error prone, to list them as exports in the header. Then marking them as 'export' will simplify that. (Downsides: I won't have a definite list of exports in one place, Also, when the same module is used with different subprograms, I can't choose to export in some cases and not others.)


### Entry Point

Any start() or main() function anyway in the Subprogram is automatically exported, and forms the program entry point is this is the main Subprogram. It does not to be in the header module, which could anyway be a Stub.

Any start() or main() in a nested Subprogram will not clash, as that will not automatically be exported from this one.

### Module Mapping

This refers to changing to logical name of a module to an actual one. Some special names are done automatically:
````
    oslib is changed to either mwindows or mlinux depending on target OS
    osdll is changed to either mwindll or mlindll
````
The last module scheme had some facilities for user-defined and conditional, to build different configurations, but that hasn't been worked out yet.

### Identity

A Program can be known by the name of its main (or only) Subprogram. And a Subprogram can be known by the name of its header module, which can also be the only one.

It is that module that is submitted to a compiler or tool.

### Parsing by External Tools

I want header information to have as simple a syntax as possible, so that it doesn't need a compiler-class lexer and parser to process. So in most cases, it will look like this:
````
    module A
    import B as D
    export C
````
Only a keyword, and a name (the 'as D' is usually not relevant outside the program). Module mapping, and one or two other things, may impact on the simplicity, but not too much.

After scanning the header of the program module, it may be necessary to recursively scan any imported subprogram header modules too, to discover all the subprograms and modules of the project.


### Circular Imports

This is automatic between the modules of a subprogram: A and B can refer to names in each other, provides those names are Global.

### Module Import Order

The out-of-order definitions and circular imports of the previous scheme meant that the ordering of modules in internal tables was indertermine. This causes problems with automatically called init-routines, and similarly when used in my dynamic language which was more behind-the-scenes init code going on.

Here, the module order within a Subprogram is fixed within the Header. (The header itself comes first if not explicitly listed.)

I haven't yet decided to allow circular imports between Subprograms. But I think the consequences will be fewer.

### Duplicate Modules Between Subprograms

I think that sometimes, the same module can be used in different Subprograms (and has not been made into an independent Subprogram of its own).

Then I will probably need to create of a duplicate, and compile it as a different module, since references to modules will be to different versions. But I'll have to come across it for real.

### Import Paths and Mapping Directives

These might be extra directives inside the header. I use them in the previous scheme, for example to help locate modules residing in separate directives.

I haven't yet worked this out. I get the feeling that explicit names don't belong in source code, or should be limited to a specific program stub module.

### Importing a DLL Library
This is done with an importdll block:
````
    importdll libname =
        clang proc puts(ichar)int
    end
````
Names in the block are automatically global, so visible outside this module. They can be qualified, but that uses the name of the module, not of the library. (So effectively repackaged into one library, if several DLLs are imported.)

The DLL name is added to the list of library imports put in the header, so need not be added that, but a duplicate is harmless.

The real DLL name need not be used, for example if the names actually exist in several libraries, or the actual DLL name is variable (for example varies by version number) so sorted out either in the header, or elsewhere. In this case a dummy name, anything starting with $, can be used instead.

For DLLs written in other languages (usually C), the importdll block is either written by hand, or all or most of the work is done with tools that process C headers for example.

For DLLs written in my language M, it is expected that, when the DLL is compiled, it automatically produces:
* An exports file with .exp extension (.m would risk overwriting the M source)
* Possibly, and optionally, a C header file, allowing it to be used from either C, or a language that can work with C header files
* Both can have doc-strings including (when doc-strings have been provided in the source)

To use the .exp from from M, at the moment it is necessary to list the module as:
````
    module bignum.exp
````
Alternatively just rename the .exp file to a separate .m file.

### Scheme I

That used a more primitive language and compiler. Modules were independently compiled, one at a time.

* No IMPORT statements in main modules

* Program structure described in project files (part of a simple IDE)

* Project file listed MODULE and HEADER files (all files with .m extension)

* To compile a module, HEADER files were combined by the IDE into a single
  composite header (with INCLUDE directives), and the names of the module together
  with that of the generated header were submitted to the compiler

* Shared functions, variables, types and named constants needed to be
  manually defined or declared in the header file (not related to C headers)

* Header files can also define actual functions and variables, which means it
  itself needs compiling, and linked in to the program

* Names in the composite header formed a separate scope

### Diagram
Outline of a simple with one main program module; plus a one-module library; plus a 5-module library with language support.
````
This demo program consists of 3 subprograms and 7 modules in all:

pidemo.m                Subprogram #1, one module

bignum.m                Subprogram #2, one module

mlib.m                  Subprogram #3, five modules


pidemo.m:               Main Subprogram/Program 'pidemo'; one module
 _____________________
|                     |      Header follows...
|    module pidemo    |      Optional to declare the first module
|    import mlib      |      Import these two subprograms
|    import bignum    |
|-------------------- |      ... End of header
|    proc start =     |      Entry point
|    ....             |
|    end              |
|_____________________|


bignum.m:               Subprogram 'bignum'; one module
 _____________________
|                     |      Header follows...
|    module bignum    |      (Optional)
|    import mlib      |      Import M library
|---------------------|      ... End of header
|    ....             |
|                     |
|_____________________|


mlib.m:                 Subprogram 'mlib'; five modules
 _____________________
|                     |      Header follows...
|    module mlib      |      (Optional)
|    module msys      |
|    module clib      |
|    module oslib     |      (Mapped to mwindows)
|    module osdll     |      (Mapped to mwindll)
|-------------------- |      ... End of header
|    ....             |
|                     |
|_____________________|

Other modules don't need to declare or import anything; they are just code:

msys.m:
 _____________________
|                     |
|    ....             |
|_____________________|

clib.m:
_____________________
|                     |
|    ....             |
|_____________________|

mwindows.m:
 _____________________
|                     |
|    ....             |
|_____________________|

mwindll.m:
 _____________________
|                     |
|    ....             |
|_____________________|
````

### Emulating Module System III

This would be handy for initally adapting existing programs.

* The existing program needs to multiple import statements per module. (Some projects, I got fed up with this, and dumped the same list of imports of all modules into one module, and all the othes used import* on that; they can be changed back.
* Make each module a Subprogram; that will be the case when a module is imported anywhere else
* Global names must be exported: add the line *export M* in each module, where M is the name of the module. (This makes it incompatible with the old compiler, unless it is tweaked to ignore such statements)
* It requires that circular imports of subprograms work

### Using Modules IV
Basic program of a single module, using only standard functions:
````
proc start =
    println "Hello World!"
    os_messagebox(message:"Hello")
end
````
Every program (or subprogram) will do 'import mlib', so that is never needed. (To inhibit that, compiler with -nosys; then that allows alternatives too.)

This program, say the module is called demo1.m, is build as 'mm demo1', to produce demo1.exe. A program of one main module, plus one library (demo2.m):
````
import bignum

proc start =
    ....
end
````
This is built as 'mm demo2'. A program of one module demo3.m, and two others, moda.m and modb.m, and using that same library, is:
````
import bignum
module moda
module modb

proc start =
    ....
end
````
Again, build as 'mm demo3'. Here, it doesn't care what modules bignum comprises, or how many; it only needs to know about 'bignum', which is also used to qualify ambiguous names.

moda.m and modb.m contain only normal code; nothing about modules. But if it turns out moda.m needs a library liba.m, then 'import liba' must go in the main module demo3.m.
