## Modules IV 2021

Module schemes used mainly in my lower level systems language. Similar ones were used in my scripting language.

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

* I now had a whole-program compiler: all source modules were compiled at the same time to provide initially ASM, then direct EXE
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

This was when I decided I needed another overhaul.


### Modules IV (2021)

* All **import** and **importpath** statements are removed from individual source files
* Every significant project starts with a Header module, which contains only directoives such **module subprog import**; no code
* The header module lists all modules in the program, using **module**, with more precise location info as needed
* When compiling, the header module is submitted. It will assemble the info about all modules, including their locations, before loading all sources together
* Simple one-file programs with no header are still supported (it will create a one-module project, which imports **mlib**; see below)
* I introduced extra structuring with modules grouped into subprograms, using **subprog**
* All the modules in the same or only subprogram will automatically see all **global** enties in the other modules
* Names in a subprogram are only visible to others when exported using an **export** attribute
* For subprograms that can also form their own programs, they can have their own header containing only those modules. That header can then be imported into this one using **import**, which now applies to such subprograms, not individual modules
* Most directives can be conditional, allowing different configurations
* Alternatively, a different header can be created with slightly different info, without touching the modules that contain code
* The standard library, which comprised several modules, now forms one subprogram **mlib**, which usually is automatically imported
* Each module creates a namespace used for qualifying imported names (although rarely needed unless ambiguous)
* Each subprogram creates a separate namespace used for qualifying names imported from that subprogram (so code doesn't need to know the individual module names, just the subprogram name). (Again, at the moment I allow unqualified imports from subprograms.)
* A **link** directive gives the name of any external DLL used by the program, if not already known. (An **importdll** block in code can either give the actual name of the DLL, or a dummy name if it's more complicated; then it will need **link**)


Cons:

* I've just started trying it out, give me a few months...


### Project Files

I had hoped that the header files of scheme IV would replace the project files of my (still crude) IDE. But it's not possible yet. Project files will contain a bunch of stuff not relevant to a program header. However:

* The IDE could extract the list of modules from the program header
* Or, as the syntax is more involved than expected (conditionals, alternate paths, imports), invoke the compiler on the program to return a flat list of modules and their locations

This part is on-going. The IDE needs the file list so I can browse through for editing; it no longer needs it to feed to the compiler (that's ancient history)

### Importing and Exporting across FFIs

This is another subject, not covered here. But:

* All schemes have required imports via FFIs (external libraries not in my language) to have declarations for those written manually, as **importdll...end** blocks in schemes II onwards
* I've experimented with tools to convert C headers for example into suitable import blocks; but these are not 100%
* When I generate DLL files with my compiler (which can then be used from other languages), I use the **export** attribute above, to determine which main program names are exported.
* The process also generates a new kind of .exp exports file, for my own language to be able to use. I may extend this to generating a .h file too.

### Sample Header File

This is the header for my 'M' compiler, an early example of such a file:
````
    module mmcli
    module mm_blockpcl
    module mm_decls
    module mm_diags
!   module mm_diags_dummy  (will be conditional)
    module mm_export
    module mm_genpcl
    module mm_lex
    module mm_lib
    module mm_libsources
    module mm_name
    module mm_parse
    module mm_start
    module mm_support
    module mm_tables
    module mm_type

    subprog pc
        module pc_decls
        module pc_disasm
        module pc_genmcl
        module pc_genss
        module pc_lex
        module pc_libmcl
        module pc_libpcl
        module pc_objdecls
        module pc_optim
        module pc_parse
        module pc_stackmcl
        module pc_tables
        module pc_writeexe
        module pc_writess

        module pci_mcl as md
        module pc_win64
````
At some point I will extract that subprogram part into its own header, then I will just use **import pc** here.

For modules that can form a separate program, like 'pc' here, usually there will be an extra module that contains a command line interface, and its own entry point. Bu that module is not needed when importing the rest into another program.

So either there can be two header files, or that extra module will be conditional, eg:
````
     module pc_cli when $mainmodule
````

At the moment I haven't taken it far except revising my current projects just enough to work with this new scheme.

(Also, the old-style **import**s are still littered through source modules, to allow building with the old compile still. They are ignored by the new one.)
