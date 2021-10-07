## Module System IV (2021 version)

My current module system cannot easily cope with groups of modules that can form parts of different projects. It becomes very messy.

So I've been looking at version I module scheme I was using in 1990s (I forget the even earlier ones), and I was impressed by the simplicity.

Schemes II and III have similarites to other languages that use 'import module'. This scheme IV will be subprogram based and project-driven.

This new one like scheme III are intended for whole-program compilers.

### Terms

**Program** A set of one or more Subprograms, which when built forms an single executable file like a .EXE.

**Subprogram** A set of one or more Modules. One module is designated the Header Module.

**Module** A single source file forms one module. 

**Header** This is an information block at the start of a Header Module. It lists the modules in this Subprogram, and any Subprograms in imports. It also lists the names that are exported from Subprogram.

**Header Module** The main or only Module of a Subprogram, the one expected to contain any Header.

**Stub** A Header Module that only contains a Header, so not functions, variables or other code, is also called a Stub.

**Library** This is a Program that is built into a shared library, such as a .DLL file. For this purpose, the main Subprogram needs to export function names to allow access from other programs.

### A Header Block

This goes at the beginning of a Header Module, or in a Stub. (I haven't whether header blocks inside another module should be an error, or just ignored.)

It contains Module, Import and Export declarations, and can optionally be wrapped in Header ... End block. (Optional because that would be too heavy for small programs.)

**Module**
````
    module A
    module B as Z
````
'Module' is used to specify the modules comprising the Subprogram. This header module is always included, but can be listed anyway.

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

The **Global** attribute can be used with functions, variables, named constants, types, enums and macros, in order to share them with other modules.

All modules in a Subprogram automatically import each other; no explicit **import** is needed. Every Global name is visible. But Globals are not automaticaly exported to other subprograms; that needs explicit Export: either use **Export** instead of **Global**, and/or add the name to the exports listed in the header.

Within a Subprogram, each module name create a namespace, with can be used to qualify names imported from other modules. Usually such names don't need qualifying, unless there is a clash (two modules use the same Global name); or to override a more locally scoped name.

The name of the Header Module also forms a separate namespace, when imported into another Subprogram. That allows that Subprogram access to this one's exports. Again, qualifying the name is not necessary unless there's a clash.

Example: this Subprogram comprises the modules S, T, U, with S as the header module. T has a global function F which is also an export. F can be accessed anywhere within the subprogram as F, or T.F to be explicit.

But from outside, it will be F, or S.F to be explicit. Modules T and U are unknown outside.

### Entry Point

### Module Mapping

### Identity

### Parsing by External Tools

### Circular Imports

### Module Import Order

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
