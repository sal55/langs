### B Language

This had been an actual new language compiler, a new attempt to create a hybrid of my Q and M languages.

This has now been abandoned.

Instead there will be a new implementation of Q, which has type annotations, and the ability to replace bytecode sequences with custom native code.

That also went in a couple of directions, and also got abandoned. The latest version was also getting too complex, and the gains didn't look as though they were going to be worthwhile.

I trying yet anther idea, which is to translate everything to native code. And, for a first attempt, it might generate
textual M code.

That also won't be much faster (type dispatch still needed), but it has some advantages:

* Much easier to gradually improve the code, and to make use of type annotations
* It allows me to create a standalone executable from my Q programs.

But it will have to be a completely separate product from the Q interpreter. It would be too complex otherwise. I don't know yet if this 'B' language will diverge from Q. Hopefully not since then I can have interpreted Q code which can also be compiled to faster B code as a normal EXE.
