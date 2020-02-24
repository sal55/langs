## M Info

### Compiling the M compiler using mm.ma

There seems to be a unique problem with using a .ma amalgamation for the compiler itself. This is because it contains two copies of some library modules, eg. msysnew.m, one for use by the compiler, and one which is the target of a 'strinclude' statement and intended for the copy of the source.

These clash, so for the strinclude, it will use the first copy, which will have been modified as I use a destructive way to parse sources.

I don't have a solution at present. Suggest using mm.exe as supplied for now (re-download as I think one uploaded version was derived from mm.ma). (Another way is to use the discrete files for the libraries, which I'd have to upload.)

http://www.bcas.freeuk.com/mm.exe  for the M compiler binary (right-click and 'save-link-as' seems the way to get it to a specific location).

Any comments, you can contact me on bart4858@gmail.com.

For the feature list of M itself, that will be here:

   [mfeatures.md](mfeatures.md)
   
Currently empty, I will download a first version tomorrow (Tuesday CET); there's quite a lot of stuff in there!
