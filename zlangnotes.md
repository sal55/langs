## Zlang Notes

(https://obiwanjacobi.github.io/Zlang/)

#### Functions

There is little difference between a function call, and a funcion definition, except the latter is followed by an indented block. I don't know if that would cause ambiguity (except maybe defining a function with an empty body), but consider:

    fn1()
        stmt
        
    if fn2()
        stmt

These look similar, but the first defines fn1, the second calls fn2. I found a similar issue with defining structs, which start with just an identifier on a line by itself (presumably when it is capitalised, it would be a type name, and a type name cannot otherwise exist by itself?).

Elsewhere, ":" is used to define a name of a certain type; perhaps this can be mandatory, even when there is no type (so Python-like although Python uses a 'def' keyword) too:

     fn(1):
         stmt

#### Case Sensitivity and Examples

I assume (since quite a few features seem inspired by C) that it is case-sensitive. Perhaps that should be stated, if not already.

Given that, this example is not quite as confusing as it seemed at first:

    ptr: Ptr<U8>

However the issue I found (with lots of languages that someone is not familiar with actually) is with using example identifiers that could plausibly be keywords the language.

#### Strings

Their layout is not that clear. Zero-terminated strings have not been mentioned (or I couldn't see it). Such strings are well-proven and would work very well even on processors such as the Z80. So you would pass strings around around as pointers (Ptr<U8>) although there would need to be some other means to denote that such a parameter is a zero-terminated string rather than a sequence of bytes. Or maybe that would be implicit.

(How would strings be passed to functions now? How would the function know their length?)

#### Null pointer value

Apparently there isn't one. So a pointer always has to point to something? This is just about workable, as you can use a special value to do its job (point to a specific global variable), but I think there is a need, otherwise lots of things you do with low-level pointers can't work (what do you put into the last node of linked list for example?).

#### Range

I couldn't understand this section. Is Range a type? Is is just syntax? Where would it be used?





