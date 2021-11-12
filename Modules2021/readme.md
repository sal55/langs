## Modules IV 2021

Module schemes used mainly in my lower level systems language. Similar ones were used in scripting language.

### Modules I (1990s)

(This is the earliest scheme I can remember the details of.)

* The compiler worked on a module at a time (independent compilation; traditional)
* There were no imports, nothing in source code that controlled program structure
* There was a crude IDE working with project files
* The IDE project file listed all source files as Modules, or Headers
* Headers usually contained declarations shared across all modules of the project, which included functions and variables exported from the modules
* Headers could also contain their own function and variable definitions
* Names in headers were in separate, more global scope than in modules (but this aspect as poorly developed)
* Then compiler input was a single module name, plus exactly one header
* When compiling a module, the IDE would collect all header files into one (by creating a temporary header file with include directives), and submit those two file names
* Because headers had their own data, they needed to compiled too: that temporary header was compiled by itself (the compiler that and used different rules)
* For creating an executable, the IDE invoked whatever linking method was in use

Cons:

* All shared functions and variables needed a separate definition, and a declaration in a header (unless defined in the header, but I kept this to a minimum)

### Modules II (2010s?)

* The compiler still compiled a module at a time to some intermediate format (likely asm)
* I first introduced the idea of every entity only being defined in one place
* To export a name outside a module, it needed a **global** attribute
* I introduced **import** *modulename*. This made visible names exported from that module
* When compiling a module, it generated a special .exp exports file containing declarations
* These declaration were in an  **importmodule...end** block in the language syntax
* When importing module **A**, it looked for **A.exp** and effectively just included that source text

Cons:

* Modules could not import each other. If A imported B, it needed B.exp. B had to be compiled first to generate B.exp. But when B imported A, then A had to be compiled first...
* The same with A importing B importing C importing A
* For such reasons, the module layout was strictly hierarchical, which was very limiting
* I still had to retain ad hoc, mamnually written declarations (but unchecked by the compiler) to get around the problems
* See also Modules III

### Modules III (mid 2010s?)

* I now had a whole-program compiler: all source modules were compiled at the same time to provide initially ASM, then direct EXE
* I still have **import**
* I introduced **importpath** to give some info about where to look for modules
* I could now have circular and mutual imports: modules could import each other with no restrictions. Exports files were no longer needed (but see below)
* I introduced **mapmodule**, which could be conditional, to help in building different configurations. So module **import A** might be import X or Y, depending on mapping.

Cons:

* If a group of related modules X, Y, Z were needed elsewhere, I need to import all three
* I introduced **import\* A** so that whatever modules A imported, were also visible here. (So A could import X, Y, Z), but this was imperfect (to resolve X.F, I need to type X.F, not A.F, but X was unknown to the importing module)
* Each module tended to have arbitrary lists of imports of various modules, very untidy and haphazard
* To mitigate this, I tried creating or designating a special module that would import all others. Then any module needed only **import\***.
* This was better, but when I tried to create an app that incorporated modules from another program, it got chaotic.
* The controls for locating a module were insufficient. There was also a scheme which maintained a list of directories to search through, but I considered this poor; you could never be certain where it would end up getting a module from. Accidentally delete a source file, and it might find an old one elsewhere)

This was when I decided I needed another overhaul.


### Modules IV (2021)





* **import** statements are removed from individual module files
* A special kind of module, called a Header, is introduced that contains only project directives.
* The Header module defines the project structure, listing the subprograms and modules, and anything else needed for building
* All modules in the same subprogram can share each others global names
* Names must be exported to be used across subprograms
* Each module name creates a namespace used for disambiguating global names that clash within a subprogram
* Each subprogram name also creates a namespace, used to disambiguate names across subprograms (however, exported names must be unique)
* To compile a project, submit the header module to the compiler
* For simple one-module projects, just submit that; the compiler will add the standard library only
* Subprograms can be defined with their own dedicated Header module
* The main Header can use **import** to include the header of a subprogram, which describes its modules
* The same set of modules can be in more than one header module, allowing different configurations of a program via different headers
* Most lines in a header are conditional, another waty to configure a program
* A mixed header + code module is not allowed, because the namespaces of header (which forms the primary subprogram) and module will clash

### Project Files

My simple IDE uses project files, separate from any source code, which lists the modules and files used by a project. Plus a bench of extra stuff like how to run the program.

I had hoped that the new Header modules would replace project files, but that is not yet practical:

* Header syntax and other matters mean the file is not so simple to read in without proper parsing
* Project files include extra info, like listing miscellaneous support files, run info, run options, command line parameters, that don't really belong in a Header

What I will have, instead, is a way for the IDE to get the compiler to turn a header file into project file format. Typically this file will be included within the main project file, which has all that extra info. The the module data, which is needed for browsing through, is kept up-to-date.

