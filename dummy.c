//count characters in a string   
int countchr(char *str, char chr)
{int c=0,cnt=0;while(str[c]!='\0'){if(str[c]==chr){cnt++;}c++;}return cnt;}   
