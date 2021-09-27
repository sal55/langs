/* Simple demo of generating textual PCL code from a C program.
   Final lines show how the standalone 'pc' program can turn the submitted
   file into one of:
      * PCL (properly formatted, but needs a different output name)
      * ASM (x64 native code in my 'AA' syntax)
      * EXE (result can also be run from here)
*/

#include <stdio.h>
#include <stdlib.h>

int main(void) {
    int a;
    FILE* f=fopen("test.pcl","w");

    fprintf(f, "extproc printf i32\n");
        fprintf(f, "extparam u64\n");
        fprintf(f, "extvariadics\n");
    fprintf(f, "endext\n");

    fprintf(f, "proc test.start::\n");
        fprintf(f, "procentry\n");
        fprintf(f, "setargs 1 0\n");
        fprintf(f, "push \"Hello World! [C]\\n\" u64\n");
        fprintf(f, "callproc &printf\n");
        fprintf(f, "push 0\n");
        fprintf(f, "stop\n");
    fprintf(f, "end\n");
    fprintf(f, "endprogram\n");

    fclose(f);

//  system("pc -pcl -norts test.pcl -out:test2.pcl");

//  system("pc -asm -norts test.pcl");

    if (system("pc -exe test.pcl")==0) {
        system("test");
    }
}
