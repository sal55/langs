## Modules IV, 2nd Attempt

The last idea got too elaborate I think, and require extra levels in the compiler's ST

The scheme below is somewhat simpler, but is also a significant departure from how it works now.

### Project Files

* Source files themselves no longer have directives defining the structure of a program. So no **import** statements (They can still be used for a while, for compatibility with older compilers)
* They do still have global attributes (to share between the modules of this group), and export (to share between groups or outside the program)
* All program structure is defined in a project file, which will have extension .mm; regular modules have extension .m
* This project file will replace the .pro files currently used by my IDE
* A project file is basically a list of modules in the program. Followed by modules in sub-programs, either headed by **subprog**, located in a separate project file and incorporated with **import**
* They also contain directives normally used by the IDE, such as info on running the program (and in buiding it - something I've neglected up to now). These are ignored when part of an import file

So, since I'm already using project files anyway, and maintaining them separately, I will be using them for gleaning the build info requried.

### Invoking the Compiler

* This curently allows .m (default) and .ma files as input; just one file of either; plus @files
* It will be changed to allow .mm files (new default)
* .m input will still be allowed for simple programs (using some default libs) that need no project file
* .ma files will incorporate the necesary .mm file (a flattened version; not the original file as is, to void extra dependencies)
* I could have changed the compiler to allow multiple .m files, and IDE submits all the info needed, perhaps via an @ file
* Alternatively, @ files could be used for this, but then *they* would become the project files.

Effectively, the project file becomes an extension of the compiler CLI, rather than of the language.

### Simple Projects

* Here the project file is just a list of the modules, what I already have in my IDE project files, more or less
* Each module name forms namespace, visible to all modules
* All global names are accessible from all other modules
* Where there are ambiguities, the module name is used to disambiguate
* When 'export' is used, the name is additional exported from the program (or subprogram; see below). Exported names must be unique across these modules

Here, then, current projects can be trivially converted. *Except* that, because all modules are shared, there will be clashes (eg. lex names in MM, and lex names in PC).

That is one use for introducing Subprograms and extra segregation

### Projects with Subprograms

* A project file (which should not share the same name as any module) creates a special namespace with the same name as the project file
* This could be used as an alternate to access globals that have the export attribute
* The list of modules can be partitioned using a Subprog directive.
* This introduces a new namespace for the following modules
* Multiple Subprog directives can follow
* Modules in the main program and each subprogram form groups which share globals, but they cannot see modules or globals from other subprograms...
* ... unless names are exported
* All exported names are visible across all subprograms
* If there are ambiguities (allowed between 2 or more subprogs, but not within the same one), the subprog namespace can be used to distinguish them. (This is way they have to be unique in each namespace)
* Exported names from the main program (not following subprograms) are exported also from the program, when used as a DLL.
* (DLL names must be unique; so duplicate names are not allowed. DLL-exported names are not decorated or mangled.)
* The set of modules and other info for a Subprog can be put into a dedicated project file. Then **import** is used on that project file
* This allows those projects to be shared more easily. And for those projects to form their own programs (normally requiring a main module providing the CLI)

### The Symbol Table

* Curently, all module names exist at the same level in the ST
* Subprog namespaces will be at the same level.
* Import maps and such will control visibility as before.

### Export

**Export** can be used in the project file as source code. Here it is used to say that all global names of a whole module should be exported. Mainly useful for names that are DLL imports, which don't allow attributes.

