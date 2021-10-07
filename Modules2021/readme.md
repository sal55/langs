## Module System IV (2021 version)

My current module system cannot easily cope with groups of modules that can form parts of different projects. It becomes very messy.

So I've been looking at version I module scheme I was using in 1990s (I forget the even earlier ones), and I was impressed by the simplicity.

Schemes II and III have similarites to other languages that use 'import module'. This scheme IV will be subprogram based and project-driven.

This new one like scheme III are intended for whole-program compilers.

### Terms

**Program**


**Subprogram**

**Module**

**Header**

**Header Module**

**Stub**

**Library**

### A Header Block

**Module**

**Import**

**Export**

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
