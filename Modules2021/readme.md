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

### Overview

The current compiler system uses these 3 lots of inputs:

* Source code (the compiler only knows one module, and derives the module structure from that)
* The compiler command-line parameter: mainly, the lead module (.m or .ma), any libraries, options
* The project file. This lists the various modules separately. The compiler knows nothing about this file, it is used by my IDE.

Above, info about the module structure is in 2 places: within the various source modules; and some is repeated inside the project file. (I have experimented with the IDE extracting module info from the source code, but some complications were introduced, and I dropped that)

The new scheme also uses those 3 kinds of info:

* Source code, but there us no info about modules or structure, other than indivdual names have global/export attributes
* The command line, which now accepts the project file as the primary info, but still reads .m/.ma files, libraries and options
* The project file, which is now read by the compiler as well as the IDE

The compiler input is one of:

* .mm file, with extra options that can override the ones in .mm
* Multiple .m files, plus libs and options; this info is used to construct suitable mm data
* .ma file, which must contain a .mm file.

### How It Works Now
```
    mm test
```
This processes test.m, which may contain **import** statements to invoke other modules. The compiler builds the module structure from following imports.

This is quite tidy, and can be improved further:
```
    mm test.ma
```
test.ma contains all relevant modules and support files. It can be generated with the -ma option of mm when compiling test.m. It makes it easier to distribute apps as source code for someone else to build; they need only mm.exe and test.ma.

There are some problems however:

* If there are N modules, every module might contain an arbitrary number of imports to some arbitrary subset of the other N-1 modules
* It makes it harder to use a related set of M modules, which also form part of another project.
* In the latter case, all globals shared by the M modules, but not intended for use outside, become visble to the modules of this project when any of those modules is imported
* There are poor arrangements for defining locations
* **import\*** was introduced, to make it possible to import a module X, which itself imports A, B and C; and make it as though A, B, C were imported directly
* But, if qualification is needed for ambiguous function F, I can't do X.F, it might be B.X for example, yet A, B, C are not directly known; they don't appear where X is imported
* This feature was also used to just collect together all modules as one list of imports, put them all into the main module P, say, then **import P** is used in all the other modules. Which then raises the question, was it that needed at all?
* In addition, if every module has **import P**, how can I use the same module in project Q?

### How The New Scheme Works

This uses a separate project file, with extension .mm, then lists all the modules. For my test.m example, let's say it's called proj.mm. Building proj.exe (not test.exe) then becomes:
```
    mm proj
```
Still fairly tidy, and can incorporate options and libraries that would have to be specified separately before.

The -ma option still works, and now includes proj.mm (whether verbatim, or synthesised, is not clear):
```
    mm proj.ma
```
So distributed source code can work the same way. But there are some advantages:

* Source modules no longer have **import** statements, or specify the layout of the program. (**global** and **export** attributes are still there.) This makes it easier to share them with different projects
* The module order is determined by the project file. Previously, circular imports meant the module order was not determinable. (Causing problems with the order of calling $init functions, or start() and autmatic setup functions in the dynamic language.)
* The project file can group the modules into subprograms. Modules in each subprogram can all share each other's globals, but they are not visible to other subprograms.
* To share names across subprograms, they need the **export** attribute
* A subprogram also has a name (cannot be a module name) which forms a new namespace, at the same level of module namespaces.
* This subprogram namespace can be used to disambiguate clashing names. All exports from a subprogram belong in its namespace.
* Exports and globals still belong to the namespace of their owner module, but this is only used for disambiguating clashes within a subprogram.
* Project files can include lots of other directives:
 * Path information
 * Export directives (eg. export all globals in a module)
 * Build options
 * Arbitrary files for browsing
* An **import** statement can be used to include a project file of a separate program. It is treated as though it was headed with **subprog** with the name of that project. Build info will be ignored.
* **module** statements can specifiy actual files; they can have **as** to define an alias; and **when** to make them conditional.
* So, this can replace module mapping, both implicit and explicit.
