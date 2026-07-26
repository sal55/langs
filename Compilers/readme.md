
## Compilers

I had had two M compilers, MM7 and MM8. MM8 was a smaller, streamlined version, with a more compact IL.

However, MM7 has some useful properties:

* The AA7 assembler depends on MM7's backend
* The BCC compiler depends on MM7's IL and backend
* The MC M to C transpiler is based around MM7's frontend and translates MM7's IL
* MM7 retains multiple function return values
* MM7 retains slices (both these are were dropped by MM8 as being rarely used, but they are cool)
* MM7 still generates somewhat smaller code than MM8
* MM7 retains the interpreter for its IL
* MM7 retains inline assembly that was dropped from MM8 (little used, but when used it is essential)
* MM7 can generate IL dumps that can be self-contained textual representation and which can be processed by a separate tool. MM8 only produces an IL dump for debugging.
* Having to maintain both MM7 and MM8 and language tweaks in sync is also a bit of strain.
  
When I put it like this, it's a no-brainer. So I'm dropping MM8 and reverting to MM7.

MM8 has the smaller and tider IL (some 80 ops instead of 130), so where those provide some advantage, I'll try and and do the same within MM7.

MM8's IL was also the basis for MZ, an M compiler targeting Z80, but that is self-contained product.

