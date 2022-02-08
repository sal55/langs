## Modules 2021

A summary of my current scheme:

* All modules used in a program are listed in a special header section in the lead module (and it uses `module` not `import`)
* This is a few dozen lines out of tens of thousands, so is hardly onerous to maintain
* It always tells me *exactly* what modules are being used
* It also defines an ordering to the modules, important when automatically called init routines are present
* The info can also be picked up by my toy IDE so that I can browse through or search the relevant project files
* My whole-program compilers like to load all modules used, before starting any parsing, and that info is provided by that list, all in one place
* (This approach also means that you don't have arbitrary lists of imports of top of every module, creating a chaotic import graph, which also hard work to maintain.)
* Any entity in a module marked `global`, is exported, and can be used from any other module without needing to fully qualify the name (unless there is clash, which is discouraged)
* I can change module names easily without changing the code (any qualified names generally use a short alias, that doesn't change)
* I can compile a version of the program with a slightly different set of modules (which still export the same names), for different functionality (eg. with or without a diagnostic module)
* I can specify the exact module, if ones with the same name exist in different locations
* Conditions can be attached to the modules, for special purposes (eg. this module for Windows, that module for Linux; my language otherwise has no conditional code)

I allow also a further level of program structure:

* A program (ie. forming one EXE or library file) can contain one or more subprograms
* A subprogram is a collection of modules (as described above)
* I use 4 levels of scope outside of functions:
   * Local within one module
   * Shared between modules of a subprogram using `global`; not visible outside
   * Shared between subprograms using \`export\`
   * Shared between programs (eg. shared libraries) by using `export` on the top-level or main subprogram
* No qualifiers are needed even for accessing entities in a subprogram unless, again, there is a clash. Then the qualifying name is the name of the subprogram (a 'black box'), not the name of the specific module
* All this structure is again listed in the header section. That uses `subprog` to start a new subprogram section, or `import` when info on the subprogram is in its own file, since it can used in multiple places

