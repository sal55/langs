Some types expressed in English:
````
pointer to function
pointer to function returning pointer to function
array 10 of pointer function
array 10 of pointer to int
pointer to array 10 of int
```` 
In C, as bare types (as used in unnamed parameter or cast):
````
void (*)(void);
void (*(*)(void))(void);
void (*[10])(void);
int *[10];
int (*)[10];
````
In C with an associated name:
````
void (*A)(void);
void (*(*B)(void))(void);
void (*C[10])(void);
int *D[10];
int (*E)[10];
````
In a typical left-to-right syntax (this is mine, inspired by Algol68),
first with no names:
````
ref proc
ref function=>ref proc
[10]ref proc
[10]ref int
ref[10]int
````
Now with a name (here following the type, but can also precede the
type, but will never be INSIDE the type!):
````
ref proc A
ref function=>ref proc B
[10]ref proc C
[10]ref int D
ref[10]int E
````
Finally, defining two variables of the same type as E, first in C:
````
int (*E1)[10], (*E2)[10];
````
Now in left-to-right form:
````
ref[10]int E1, E2
````
