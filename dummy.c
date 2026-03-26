//========================================================================
//C  125 LOC
//On my WSL system this C runs in 0.095 seconds using the unsorted words file
//========================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>   
#include <time.h>   
#include <ctype.h>  //for tolower and toupper

//example usage = $./2600words words_unsorted.txt 500 6

//string compare function for qsort
int comparechar( const void *a, const void *b){
    const char **chara = (const char **)a;
    const char **charb = (const char **)b;
    return strcmp(*chara, *charb);
}


int main(int argc, char *argv[]) {
    //validations
    if ((argc) < 4) {
        printf("Invalid input \nEnter program-name  word-file  rows  columns\n");
        printf("example: $./2600words words.txt 400  7\n\n");
        exit(0);
    }   

    if (atoi(argv[2]) * atoi(argv[3]) < 2600) {
        printf("Invalid input: enter rows * columns that total 2600+\n\n");
        exit(0);
    }
    
    int  i = 0, t = 0, wout = 0;                                    //counters
    int  lettercnt[26] = {0};                                    //hold count of words by first letter
    int  maxwordlen = 0;                                        //length of longest word in list
    int  start = 0, end = 0;                                    //used to extract 100 words per letter
    int  temp[100] = {0};                                        //holds the 100 random words for the letter
    int  wordcnt = 0, totwords = 0;                                //used to extract 100 words per letter
    char line[35] = "";                                            //buffer to hold line when reading file
    char therand[9];                                            //the current random value
    char usedlist[1000];                                        //stores the random numbers already used
    
    //===========================================================================
    //nitty gritty - read in the unsorted words
    //===========================================================================
    FILE *fin = fopen(argv[1],"r");                                //open file
    while (fgets(line,sizeof line, fin)!= NULL) {                //count lines = words, get max word length
        wordcnt++;
        if (strlen(line) > maxwordlen) {
            maxwordlen = strlen(line);
        }
    }   
    char theword[100];
    rewind(fin);                                                //pointer back to beginning
    char **wordsin  = malloc(sizeof(char*) * wordcnt);            //allocate memory
    while (fgets(theword,sizeof theword, fin) != NULL) {        //read line into buffer
        int wordlen = strlen(theword);                            //get length of word
        wordsin[i] = malloc(wordlen + 1);                        //allocate memory for the word
        strncpy(wordsin[i], theword, wordlen);                    //copy word into array
        wordsin[i][wordlen-1] = '\0';                            //add terminator - overwrites the \n in the file
        lettercnt[wordsin[i][0]-'a']++;                            //update count of words by first letter
        i++;                                                    //increment counter
    }
    fclose(fin);                                                //close handle to file
       
    //===========================================================================
    //fun begins
    //===========================================================================
    //sort master list of words
    //for each letter, determine the start and end positions of words beginning with that letter
    //generate random numbers between that start and end
    //check if that random number is in the usedlist array.  If not, add it to usedlist and temp arrays
    //when temp array has 100 unique randoms in it, add them to the master array, break and go to next letter
    //do one sort at the end
    qsort(wordsin, wordcnt, sizeof(char*), comparechar);        //sort the master
    char **wordsout = malloc(sizeof(char*) * 2600);                //final output goes into this array
    srand(time(NULL));   
    for (i = 0; i < 26; i++) {                                    //find start-end for each letter set
        start = (totwords += lettercnt[i]) - lettercnt[i];
        end = start + lettercnt[i] - 1;
        memset(usedlist, 0, sizeof(usedlist));
        memset(temp,     0, 100);
        t = 0;
        for (int j = 0; j < 200; j++) {
            int r = (rand() % (end - start + 1)) + start;   
            sprintf(therand," %d ", r);
            if (strstr(usedlist, therand) == NULL) {
                strncat(usedlist, therand, strlen(therand));
                temp[t++] = r;
                if (t > 100) {
                    for (int j = 0; j < 100; j++) {
                        sprintf(theword, "%s", wordsin[temp[j]]);
                        int wordlen = strlen(theword);                                               
                        wordsout[wout] = malloc(wordlen + 1);   
                        strncpy(wordsout[wout], theword, wordlen);                       
                        wordsout[wout++][wordlen] = '\0';                           
                    }
                break;
                }
            }
        }
    }
    qsort(wordsout, wout, sizeof(char*), comparechar);    //final sort of 2600 words
    
    
    //===================================================================================================
    //final output: print word counts by letter, print dupes, print random words by column then row
    //===================================================================================================
    printf("%d words loaded\n",wordcnt);
    if(wout == 2600) {
        printf("list of 2600 unique random words created\n");
        printf("\nLetter   Words In   Words Out\n");
        t = 0;
        for (i = 0; i < 26; i ++) {
            t = 0;
            for (int j = 0; j < wout; j++) {
                if (wordsout[j][0] == (i + 97)) {t++;}
            }   
            printf("  %2c    %6d        %d\n", i+97, lettercnt[i], t);
        }
       
    } else {
        printf("Errors occurred.  2600 words not produced.\n");
        exit(0);
    }

    //duplicate words
    printf("\nDuplicate words in proper case\n");
    for (i = 0; i < wordcnt-1; i++) {
        if (strcmp(wordsin[i], wordsin[i+1]) == 0) {
            sprintf(theword, "%s", wordsin[i]);
            for (int i = 0; theword[i] != '\0'; i++) {
                if (i == 0) {theword[i] = toupper(theword[i]);}
                if (i >  0) {theword[i] = tolower(theword[i]);}
            }
            printf("%s\n", theword);
        }
    }

    //print random words in column then row order
    int rows = atoi(argv[2]), cols = atoi(argv[3]), colwidth = 20;
    printf("\n\n2600 unique random words (%d rows x %d columns)\n", rows, cols);
    for (int r = 1; r <= rows; r++) {
        if (r <= wout) {
            int nbr = r;
            printf("%3d. %-*s", r, colwidth, wordsout[nbr-1]);
            for (int i = 0; i < cols-1; i++) {
                nbr += rows;
                if (nbr <= wout) {
                    printf("%-*s", colwidth, wordsout[nbr-1]);
                }
            }   
            printf("\n");
        }
    }
    
    //finito
    free(wordsin);
    free(wordsout);
    
    return 0;
}
