Declaring i64 in C:
````
long long
long long int
signed long long
signed long long int
long int long
int long long
long signed long
long long signed
long signed long int
long long signed int
long long int signed
signed long int long
long signed int long
long int signed long
long int long signed
signed int long long
int long signed long
int long signed long
int long long signed
int64_t               (Needs inttype.h or stdint.h)
long                  (When long is 64 bits, eg. Linux 64)
long int
signed long
signed long int
int long
long signed
signed int long
signed long int
int signed long
int long signed
````
Note that int64_t might be implemented on top of 'long long' (etc...), or on top of 'long'. So there is the additional problem that an int64_t* type may not be compatible with long long int*; you can't print int64_t using "%lld", and you can't create int64_t constants using 0LL.

When it comes to const i64, then there are an unlimited number of ways of denoting that, since not only can 'const' appear in any position (at start, at end, or in-between), but any number of 'const' can appear.
