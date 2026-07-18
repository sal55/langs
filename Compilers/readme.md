
Compilers

Plans:

* Archive MM7 compiler
* MM7's IL backend was also used by my 'MCC' C compiler project. That will also be archived in that form. Probably only the front half will be retained to use as a tool to convert C APIs into M bindings.
* MM7's MCL backend (the part that comes after the IL) was also used by my 'AA7' assembler. That needs to be revised to use the backend of MM8
* The AA6 assembler will also be archived. That was needed to allow MCC to compile multi-module apps (only single-module apps can be compiled directly to EXE). AA7 only deals with single-file inputs
* MM7 has an interpret option, but this was little used
* MM7 was also the basis for the 'MC' transpiler, which can turn M programs via ILv7, into linear C. To retain this, I can revise MC to be based around MM8 instead.

Summary: get rid of MM7; reduce what MCC does; overhaul AA7 and MC so they work with MM8. That will tidy things up.

In practice, I'll probably just keep using binaries of AA7, MCC and MC until I get round to any changes.
