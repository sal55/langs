          csegment  
          align     16
test.start:
start::
          sub       Dstack,	40
          mov       D10,	L2
          call      printf*
          xor       A10,	A10
          call      exit*
;End 
          isegment  
          align     16
L2:
          db        "Hello, World! [pcl2]"
          db        10
          db        0
          csegment  
          align     16
          nop       
          nop       
