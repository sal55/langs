## Porting C Program

This is a port into my language of a small C program: https://github.com/MichaelDim02/houndsniff, which I found on [reddit](https://www.reddit.com/r/C_Programming/comments/kowj1i/i_wrote_a_hash_identification_tool_in_c/).

Files should be listed above. hash.m corresponds to main.c of original; select.m to select.c; hashhelp.txt contains the help text from main.c.

Build method using my 'bb' compiler:

    bb hash
    
Dependencies needed to run:

    sqlite3.dll
    hashes  (from original github project)
