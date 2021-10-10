          csegment  
hello.start:
start::
          sub       Dstack, 40
          mov       D10,    L3
          call      puts*
L2:
          xor       A10,    A10
          call      exit*
          add       Dstack, 40
          ret       
;End 
          isegment  
          align     16
L3:
          db        "Hello, World!"
          db        0
