#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* allocstring(char* s){
	char *t = malloc(strlen(s)+1);
	strcpy(t, s);
	return t;
}

int main(void) {

  int i = 0, x = 0;
  int nums[] = {1, 2, 4, 5, 7, 12, 1300, 10000};
  //int nums[] = {3,4,5,7,8};  //sequential, for testing
  int lenNums = sizeof(nums)/sizeof(nums[0]);
  int start = nums[0];
  int end   = nums[lenNums-1];
  char *interval = "";
  char **intervals = malloc(sizeof(char*) * lenNums);

  //check for no open intervals
  if((end-start)==(lenNums-1)) {
    printf("Number set is sequential from %d to %d\n",start,end);
    exit(0);
  }   
    
  //find open intervals
  x = 0;
  for(i=0;i<lenNums;i++) {
    if(nums[i+1]-nums[i] > 1) {
      start = nums[i]+1;
      if(nums[i+1]-start >= 1) {
        end = nums[i+1]-1;
        //end of algorithm

        sprintf(interval,"(%d,%d)",start,end);
        intervals[x] = allocstring(interval);
        printf("%s x=%d ",intervals[x],x);
        x++;

      }
    }       
  }

printf("\n\n");

printf("Interval 1 = %s\n",intervals[0]);
printf("Interval 2 = %s\n",intervals[1]);
printf("Interval 3 = %s\n",intervals[2]);
printf("Interval 4 = %s\n",intervals[3]);
printf("Interval 5 = %s\n",intervals[4]);
printf("\n");
for(i=0;i<x;i++) {
  printf("%s ",intervals[i]);
}     


printf("\nDone");
exit(0);       
} 
