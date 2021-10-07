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
