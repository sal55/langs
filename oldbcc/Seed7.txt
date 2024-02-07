### Compiling Seed7 with MCC

This is experimental work and is going to be hard going. And a successful result doesn't look very likely at the minute.

Some missing header features have been filled in, but not all. With the version [7.5b](mcc64.c) uploaded today (July 5), the following has been achieved:

* Out of 153 C modules of Seed7 (a version from a few weeks ago), 120 compile without errors using mcc. 33 fail.

* Of the 33 that fail, 20 also fail with gcc (they will be for Linux or whatever)

* So there are 13 modules that don't yet compile. Out of those, 8 are failing on windows.h declarations that are not yet in place (those tend to go on and on (and on), but will get there eventually).

* Of the other 5, a couple at least need winsock2.h (and many dependent sub-headers); one needs shellapi.h, and there are a couple of mysterious undefined names that I'm trying to track down (eg. ftello64 and gettimeofday), but not having much luck.

### Further Work

Mcc has now been updated with a 'better' code generator, which is more reliable, but much slower.

Work on completing missing headers hasn't been done yet, that will be a while still. The experimental work to
automatic production of a streamlined windows.h didn't get anywhere.

