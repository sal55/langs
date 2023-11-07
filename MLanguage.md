## 'M' Systems Programming Language

This is something I first devised at the beginning of the 80s for programming a microprocessor. I'm still using a version of it over 40 years later.

Although it has evolved, it is still at roughly the level of C, and a million miles from the advanced, sophisticated languages and smart compilers that everyone seems to be using now.

If it wasn't for C still being tremendously popular and relevant, I would probably have let my language die. It would be too embarassing to admit all the features, types and abilities it doesn't have.

As it is, it is still going, mainly used to code a small circle of related language applications. I'm cutting some of those back, but keeping my M language for the time being.

### It's Different

I'm just starting to realise that because this is a personal language, different rules apply:

* It's designed to run on one platform at a time, whichever is current. 40 years ago it was 8-bit machines running CP/M. Now the range is wider, but it happens to run on 64-bit Windows. (Still the most practical-to-buy desktop machine in any computer store. I don't do smartphones or tablets.)
* I'm the only user, I can use whatever syntax I like, and blow everyone else
* I don't need to have features I don't understand, don't like, don't know to use, or would never use anyway. I'm sure there are features that many languages implement only because others do so, or because it is expected
* I can have, and can use, features that are frowned upon, like `goto`, or mutable variables, or inline assembly.
* I don't have advanced types, eg. no tagged unions or ADTs(?)
* I don't need full coverage of all combinations of features
* I can work around known bugs
* Error reporting only needs to be sufficient
* I don't need to have language servers or worry about syntax highlighting for every editor in the world (only mine)
* I don't have an optimising compiler. (Most M programs might take 30-50% more runtime compared to gcc/clang -O3; it's not a big deal.)

Mine isn't a typical toy language either: in the 80s and 90s it was used for real applications that people paid money for. It can still be used to write real, useful programs, but with the current state of technology, its insularity is a problem.

For example, my 1981 version, running on an 8-bit computer, was able to frame-grab from a TV camera or off-air, because I took care of all the hardware. I wouldn't have a clue how to do it know. With a mainstream language, somebody will already have devised a solution, so just download a suitable library and API for the language. Maybe the purveyors of the camera are using the same language.

For me it would take too much effort as, with a private language, you have to create your own bindings to any libraries, and that's if they are C-compatible. If they are C++, Go, Rust or Python, then forget it.

However, I'm not planning anything ambitious, it's just for recreation. And it gives me a kick to be in the unusual situation of having used a personal language for 99% of my coding for the last 40 years, and to have made a living with it for a chunk of that. I think I owe to the language to keep it alive a little longer.






