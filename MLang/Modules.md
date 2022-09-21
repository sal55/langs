### Modules IV (2021)

* All **import** and **importpath** statements are removed from individual source files
* Every significant project starts with a Header module, which contains only directives such as **module subprog import**; no code
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
* The module import order will always be fixed in the order encountered in the header module.

Cons:

* I've just started trying it out, give me a few months...

Restrictions:

* Currently module names must be unique; they can't be shared between subprograms
* A subprogram name can't have the same name as a module name; this is because each creates a namespace, at the same scope level, and they would clash

Thanks to feedback from Reddit uses, I've made some tweaks:
* I've relaxed the need for modules to be either all-header info or all-code
* A header information block can appear at the top of the lead module (the one submitted to the compiler)
* It can also appear at the start of the lead module of a subprogram, which is **import**ed elsewhere
* Where a program with header H and first module A; is turned into composite header/module H, then the main program name is still H. But the first module becomes $H. However for name qualifying, H can be used in place of $H.



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

### Sample Header Modules (Scheme IV)

This is the header module for my 'M' compiler, an early example of such a file; the module is called mm.m:
````
    module mmcli             # (mm modules are the compiler front-end)
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

    subprog pc             # (pc modules are the backend, translating 'pcl' code to, here, native win64 code.
        module pc_decls    # In this, this forms a static part of the compiler. These will later be better structured)
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

(Also, the old-style **import**s are still littered through source modules, to allow building with the old compiler still. They are ignored by the new one.)

This is another header module, for a C compiler project. This incorporates an assembler/linker:
````
    module cccli
    module cc_parse
    module cc_lex
    module cc_genmcl
    module cc_blockmcl
    module cc_lib
    module cc_genasm
    module cc_libmcl
    module cc_export

    module cc_headers
    module cc_support
    module cc_decls
    module cc_tables

    importpath "/ax/"
    subprog aa
        module cc_assembler
        module aa_writeexe
        module aa_disasm
        module aa_genss
        module aa_lex
        module aa_lib
        module aa_parse
        module aa_objdecls
        module aa_writeobj
        module aa_decls
        module aa_tables
 ````
Here, the header module itself is the file cc.m, which also gives its name to the executable: cc.exe (which is later renamed to bcc.exe for the production program; since 'cc' clashes with components of other C compilers).

The assembler project is built as a standalone file aa.exe, using this header module aa.m:
````
    module aacli
    module aa_decls
    module aa_tables
    module aa_objdecls
    module aa_lex
    module aa_parse
    module aa_writeobj
    module aa_writeexe
    module aa_writess
    module aa_disasm
    module aa_genss
    module aa_lib
````

Building any of these projects is done by submitting the header module to the mm compiler as follows:
````
    mm aa          # assembler
    mm cc          # C compiler
    \m\mm mm       # M compiler (the two mm.exe's must be in different locations; Windows doesn't allow overwriting a running EXE file)
````

### Header Directives, Scheme IV

Directive  | Parameter | Optional | Description
--- | --- | --- | ---
**module** | name | **as**, **when** | Module name used by project
**subprog** | name | **as** | Start new subprogram section
**import** | name | **as**, **when** | Import subprogram info from a separate header
-- | | |
**sysmodule** | name | **as**, **when** | As above but different rules on locating modules
**syssubprog** | name | **as** |
**sysimport** | name | **as**, **when** |
-- | | |
**altpath** | expr | **when** | Alternate module path
**headerpath** | expr | **when** | Alternate subprogram/import path
-- | | |
**linkdll** | expr | **when** | Define any DLLs not already specified by `importdll` in code
**linklib** | expr | **when** | Define any LIBs not already specified by `importlib` in code
-- | | |
**setvar** | var **=** expr| **when** |

**name** is an identifier (module base-filenames must be valid language identifiers)

**var** is one of a set of predefined variables giving some environment info, current path etc. There are also some user-definable names

**expr** is one of 'var', 'name' or 'string'

**as** is followed by an alias for a module or subprogram name

**when** is followed by an 'expr' (this bit, like a lot of it, not fully developed; it depends on what comes up)

**LIBs** are the shared library alternative to DLLs, that can be generated by the M compiler.
