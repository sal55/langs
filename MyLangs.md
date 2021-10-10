## Summary of Language Projects

I will shortly be wrapping up further development of my personal languages, so this is a summary of what they are up to.

These are not modern languages with all the lastest ideas in advanced features. Just simple, down-to-earth tools to do a job of work.

Development is done on Windows, and implementations are mainly for Windows

### The Languages

Language  | Program | Project name | Written In | Description
--- | --- | --- | --- | ---
M | mm.exe | MX | M | My systems programmming language
Q  | qq.exe | QX  | M | My dynamic, interpreted scripting language
PCL | pc.exe | MX | M | Portable intermediate language, used as target by mm.exe
ASM | aa.exe | AX | M | Assembly language for x64 using my syntax
(C  | bcc.exe | CX | M | C subset)

The first two languages are designed for writing programs in. The next two, while they could be used for writing whole applications in, are intended as code-generation targets. (And mainly for testing, as usually textual intermediate forms are by-passed.)

I've included the line about C, which is clearly not my language, since it rounds off the set of language tools. bcc.exe has a number of uses, but an important one is for testing: by building bcc.exe with a new version of mm.exe, I can test it on a wide range of C source files.

### Availabilty

I class these are private languages, although anyone can try out the .exe files whenever I upload them, because:
* I can't do the support that would be needed for general use
* There are no proper docs
* They haven't been tested enough with lots of people applying them to diverse applications, to iron out bugs, highlight shortcomings, and fill in missing features
* The languages have also been volatile as I'm always tweaking

For my limited codecase, I can deal with bugs, or unimplemented combinations of types and operators, as they are encountered. And for a breaking update, there's not much of codebase to modify.
