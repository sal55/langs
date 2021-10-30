## Modules IV, 3rd Attempt

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

