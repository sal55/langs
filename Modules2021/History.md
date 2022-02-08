## Modules 2021

These are the Module schemes used mainly in my lower level systems language. Similar ones were used in my scripting language.

I've just devised a new scheme called Modules IV, described below. But it makes a bit more sense after reading about the previous ones.

### Modules I (1990s)

(This is the earliest scheme I can remember the details of.)

* The compiler worked on a module at a time (independent compilation; traditional)
* There were no import statements, nothing in source code that controlled program structure
* There was a crude IDE that used project files
* The IDE project file listed all source files as Modules, or Headers
* Headers usually contained declarations shared across all modules of the project, which included functions and variables exported from the modules
* Headers could also contain their own function and variable definitions
* Names in headers were in a separate, more global scope than in modules (but this aspect as poorly developed)
* Then, compiler input was a single module name, plus exactly one header
* When compiling a module, the IDE would collect all header files into one (by creating a temporary header file with include directives for those files), and submit those two file names
* Because headers had their own data, they needed to be compiled too: that temporary header was compiled by itself (the compiler knew that and used different rules)
* For creating an executable, the IDE invoked whatever linking method was in use

Cons:

* All shared functions and variables needed a separate definition, and a declaration in a header (unless defined in the header, but I kept this to a minimum)

### Modules II (2010s?)

* The compiler still compiled a module at a time to some intermediate format, likely ASM
* I first introduced the idea of every entity (function, variable, named constant, type, record, enum) only being defined in one place
* To export a name outside a module, it needed a **global** attribute
* I introduced **import** *modulename*. This made visible names exported (marked 'global') from that module
* When compiling a module, it generated a special .exp exports file containing declarations for names marked 'global'
* These declarations were written in an  **importmodule...end** block in the language syntax
* When importing module **A**, it looked for **A.exp** and effectively just included that source text

Cons:

* No circular imports. If A imported B and B imported A, then to compile A, it needed B.exp. So B had to be compiled first to generate B.exp. But then that needed A.expr, so A had to be compiled first...
* The same with A importing B importing C importing A
* For such reasons, the module layout was strictly hierarchical, which was very limiting
* I still had to retain ad hoc, manually written declarations (but unchecked by the compiler) to get around the problems.
* See also Modules III

### Modules III (mid 2010s?)

* I now had a whole-program compiler: all source modules were compiled at the same time to initially ASM, then directly EXE or DLL
* A program was compiled by submitting the lead module. It would follow its imports to locate all other modules
* It still had **import**
* I introduced **importpath** to give some info about where to look for modules (this just added an extra search path)
* I could now have circular and mutual imports: modules could import each other with no restrictions. Exports files were no longer needed (but see below)
* I introduced **mapmodule**, which could be conditional, to help in building different configurations. So module **import A** might really import X or Y, depending on mapping.

Cons:

* If a group of related modules X, Y, Z were needed elsewhere, I need to import all three
* I introduced **import\* A** so that whatever modules A imported, were also visible here. (So A could import X, Y, Z), but this was imperfect (to resolve X.F, I need to type X.F, not A.F, but X was unknown to the importing module)
* Each module tended to have arbitrary lists of imports of various modules, very untidy and haphazard, and which I had to keep updating
* To mitigate this, I tried creating or designating a special module, say **M**, that would import all others. Then any module needed only **import\* M**.
* This was better, but when I tried to create an app that incorporated modules from another program, it got chaotic.
* The path controls for locating a module were insufficient. There was also a scheme which maintained a list of directories to search through, but I considered this poor; you could never be certain where it would end up getting a module from. Accidentally delete a source file, and it might find an old one elsewhere)
* If I decided to change a module name, I might have to update a bunch of other modules
* Because of unrestricted circular imports, module import order is indeterminate. Since modules can have inititialisation routines called automatically, a program can't know for sure which order they are called. (This is especially a program in the dynamic language with complex static data initialisation.)

This was when I decided I needed another overhaul.



### Sample Project File (Scheme I)

Some lines such as run-info omitted:

````
    module mplc.q
    module gltest.q
    module dxscan.q
    module readdxf.q
    module mpci.q
    module mpcilib.q
    module gs.q
    module start.q

    module memory.q

    module gx.q
    module maths.q
    module messages.q

    module dwg.q
    module text.q
    module cadlib.q
    module hide.q
    module shade.q
    module mbitmaps.q
    module dsgdi.q
    module bitmaps.q

    header \m\winhdr.q
    header \m\qlibhdr.q

    header screcords.q
    header scprocs.q
    header scconsts.q
    header scvars.q

    header glhdr.q
    header dxhdr.q

````

This is an example of an IDE-generator header file, from info in the project file, used as the header file needed by the compiler:
````
!Auto-header file generated by ...
    include "\\m\\winhdr.q"
    include "\\m\\qlibhdr.q"
    include "screcords.q"
    include "scprocs.q"
    include "scconsts.q"
    include "scvars.q"
    include "glhdr.q"
    include "dxhdr.q"
````

