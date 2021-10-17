## Modules IV, 2nd Attempt

The last idea got too elaborate I think, and require extra levels in the compiler's ST

The scheme below is somewhat simpler, but is also a significant departure from how it works now.

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
