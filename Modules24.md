## Modules 2024

I created a new Modules scheme a couple of years ago. Based on my experience since, this has been greatly simplified. This new version is in use in two languages, one static and one dynamic, both ahead-of-time compiled.

### What is a Module

A 'module' in this scheme is always one source file. One module cannot be implemented across multiple source files. One source files cannot define multiple modules. Modules cannot contain other modules.

The name of a module must be both a valid identifier in the language, and a valid filename.

### Example Project

I will use an example app P consisting of 4 modules P (the lead module), A, B and C. All information about the layout of project is given in the lead module. There are two ways do this:

**(1)** P contains only the list of modules and no other code. So it looks like this:
````
module A
module B
module C
````
This is the pattern I use for most projects. This allows the lead module to be easily swapped with another, with a different set of modules to provide a differently configured application.

**(2)** Here, P can also contains code, although here you'd probably dispense with P completely, and put the module info at the start of A:
````
module B
module C
.... the rest of module A ....
````
(Note that the application will now be called A, but of course you can name the modules P, B, C.)

There is no project info, no `module` or `import` statements, in any other module. Other module schemes then to have rag-bag collections of `import` statements at the top of every module, which in my view is unnecessary micro-managing.

### The SubProgram

Modules in my scheme are grouped into SubPrograms. Within that group, any entity exported by any module (using `global` attribute), is visible to all modules in the group. No specific `import` is needed, so long as all modules are listed in the lead module. My example program contains one subprogram.

No name-qualifier is needed either: to call a global function `F` defined in `B` from `A`, I can just write `F()`. I only need to write `B.F()` if, for example, `C` also exported a function `F`.

So all modules in the subprogram group are on familiar terms with each other. There is no hierarchy. There can be cycles: A imports B that imports A.

### Multiple SubPrograms

Most of my current projects have one subprogram - one group of chummy modules (plus the standard library; see below). Programs can have several subprograms, but each shoule be group of modules that could be compiled by themselves, either into an EXE file, or into  DLL file (when there is no `main` entry point and the subprogram is a library).

Suppose there is a 3-module library Q with modules Q, X, Y. Q might contain:
````
module X
module Y
````
To incorporate this into P, so that is Q is statically compiled into the same EXE, P is defined like this:
````
module A
module B
module C
import Q           # read further modules from Q
````
The resulting program compromises modules P, A, B, C, Q, X, Y, although P and Q contain only module info here.

### Visibility between SubPrograms

Even global entities between the modules of Q, for example, are not visible from P. Unless they are specifically exported from Q. This involved using the `export` attribute instead of `global`. (It is not possible to export without also making a name global in that subprogram.)

So if X exports a file `G`, it can be called from module A using `G()`, you don't need to qualify the name unless there is again a clash. But if you do it will be written as `Q.G()` not `X.G()` or `Q.X.G()`; P knows nothing of the internal modules of Q. (It is not possible to export two different `G` functions from Q.)

Here there is a hierarchy of dependencies; cycles are not allowed. The first subprogram (P in my example) is at the top of the hierarchy.

Q can't call exports of P, as it needs to work standalone. (Possibly I can have a project Q that imports P; I'll have to try it to see what happens!)

### The Standard Library

For my static language, this is a collection of 5 modules, listed in a 6th module called `msyslib`. This would normally require this line in each application:
````
import msyslib
````
but this module is included automatically, unless specifically excluded.

### NameSpaces

Each module creates a namespace within that subprogram. And each subprogram also creates namespace visible across the program. Maybe this are used for disambiguation when global or exported names clashes. In that case, aliases can be created:
````
 module longmodulename as lmn
````
In the dynamic language, module names can be stored in variables and used for name resolving at runtime.

### Creating a Library

Any program can be compiled to a DLL file (Windows dynamic shared library) rather than EXE. Any names with `export` attribute in the top or only subprogram, are also exported from DLL library. (In this case, the names are unadorned. If `B.F` is exported, it will have the name `F`, so some care needs to be taken to avoid clashes.)

(Comments about EXE and DLL obviously refer to the systems language.)

In principle, anything in its own subprogram can be made into a DLL, and the same functions via the usual FFI methods. If the DLL is created with my compiler, it will automatically produce an exports file to allow its use from my language. So if Q is compiled, it will create a module called Q_LIB. Then P can be revised to be this:
````
module A
module B
module C
module Q_LIB           # contains FFI module to access exported entities of Q
````

### Library Imports

One other thing the scheme specifies is any external libraries that are needed to build the application. For example:
````
linkdll opengl
````
This is only needed (in my language) if there isn't an import module somewhere with an FFI block that explicitly names the library.
(Often there is no clean link: an FFI block might named 100 function which exist in the separate DLLs, or the exact DLL name depends on version which I want in one place. Sometimes multiple FFIs declare names in the same DLL.)

### The File System

The module scheme tries to be independent of the file system. But it can't always manage that.

There is currently a weak spot: unless all input modules are in the same directory of the lead module, it needs to be told where to look. But as it's done now, that info is hardcoded here, which is undesirable. See the real example below.

So this needs a better solution. Otherwise a module like this:
````





### Module Evaluation Order



### Real Example

This is from a C compiler, an old one which incorporates an x64 assembler as a separate subprogram. There are two programs plus the standard library.

