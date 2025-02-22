Measuring Compilation Speed

(Blog post)

This is mainly about measuring the build speed of my systems language compiler: how fast it can turn source code into (on Windows) an EXE or DLL file.

That's particularly important here as it's a whole-program compiler; it doesn't have independent or incremental compilation.

It is in fact quite fast, this not about boasting about its performance. My first compiler was probably 1000 times slower: most of the speed-up is due to advances in hardware over 40+ years. A lot is because my compilers remain unsophisticated: there's little for it to spend much time doing! And the language remains primitive.

**Quantifying Compile Speed**

I use lines-per-second as the main metric, but below I also give figures for MB/sec of generated code.

I know LPS isn't popular, because it depends on language, and coding style (some languages cram a lot in horizontally). Some also need to compile large amounts of library code, or preludes, sometimes repeatedly, so that a line-count becomes less meaningful.

But this is abput *my* language, where each line of source is processed once. I can tell you that my source code averages under 20 characters per line, using hard tabs.

**Measuring Elapsed Time**

Three ways have been considered all based on calling C's `clock()` function (other timing methods have been tried):

* **1** Invoking the compiler via C's `system()` function, as though typed via shell commands, which includes CLI overheads.
* **2** Invoking it via direct OS calls, which still include process overheads
* **3** Measuring directly inside the compiler, by calling 'clock' on entry and again
just before terminated.

The problem with **1** is that compiling a 3-line 'hello-world' can take 0.04 seconds, so is not accurate for determining LPS (lines-per-second); most of my programs build in under 0.1 seconds.

Actually, I mostly run the compiler from my IDE, which uses **2**. **3** tells me the time my compiler actually spends compiling, which is all I have control over. I believe this is the true LPS, which is also the elapsed time when the compiler is a resident library, or is embedded (but I don't do that right now).

However, for timings below 0.1 seconds, the the **3** timing can be 40% faster, compared to 10% for the longer tests. This is a little suspect so only **2** timings are shown below.

**Source File Caching**

All timings assume source files are already cached, somewhere in the OS's memory. Because that will nearly always be the case, as you will have just edited a source file, or last built the app half a minute ago, or it was just downloaded etc. It's not clear if writing of any output file extends past the measured runtime, but
this wouldn't affect the of perceived edit-run cycles.

**Single Core**

The discussion here is about programs running on a single core and in one thread, since it is to do with raw compilation speed.

In any case, whole-program compilers don't parallelise easily. At best you can build two programs at the same time on separate cores, but that is not a common use-case for me. But an extra CPU core is still useful because then all the background servces that Windows likes to run can interfere less.

**Implementation Language**

My compiler is self-hosted, but since it doesn't do optimisations, that puts it at a disadvantage when comparing LPS figures with other products that use fully optimised languages.

I can apply optimisations if I take the trouble to transpile to C and then use an optimisation compiler. At present that only works for an older version, where it speeds up build times by about 30%. This is mentioned below

**Test Cases**
````
Project    LOC  Modules   EXE Size

mm         37K   50        400 KB   (Main compiler; all these include 5 stdlib modules)
qq         35K   36        250 KB   (Interpeter project)
pcl        19K   29        180 KB   (IR backend as library)
mm200     200K   96       1800 KB   (mm padded to 200Kloc)
fann4     743K    6       5400 KB   (740Kloc is in one module)
````
mm/qq/pcl are real projects. 'mm200' is `mm` with multiple copies of some modules to emulate a 200KB project. 'fann4' is a synthesised test input. For these tests, any embedded data has been omitted.

**Test Results** 

On my machine PC2 (see below):
````
                mm     qq    pcl  mm200  fann4

Build time      80     70     50    340    970   msec

LPS            460    500    380    590    760   Kloc/sec

Output rate    5.0    3.5    3.6    3.0    5.5   MB/sec
````

** Other Hardware**
````
I tested also on machines PC1 (an old laptop) and PC3 (a borrowed laptop):

Machine  Rating  Cores    Relative to B

PC1         555    1      ~3   x slower
PC2        1760    2       1.0
PC3        3500    6       1.7 x faster
````
The rating is from a CPU benchmarks site; faster is better. Across these machines, LPS figures would range from 150Klps to 1300Klps.

** Possible Speedups **

Ideally this would be done by writing a cleverer compiler, for example having it, plus the data
structures used for the last compilation, resident in memory, then looking at doing incremental
compilation internally.

Otherwise there are easier methods:

* Use a faster computer, for example PC3 would be 70% faster than my current machine

* Get it transpiled to C then use an optimising compiler; this could add 30%. Combined, these
could yield 120% improvements in LPS.

* If the edit-run cycle is an issue, then I can use the compiler's interpreter feature where I compile as far as IL only, then run that. Compilation to IL is then 50% faster (so just manages 1Mlps on PC2).

However, my own projects clearly aren't really in need of the extra speed. This is more for sport.
