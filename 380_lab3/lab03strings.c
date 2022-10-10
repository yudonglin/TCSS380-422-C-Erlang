// Team 19 - Yudong Lin & Wei Wei Chien

#include "string.h"
#include <ctype.h>
#include <stdio.h>
#include <malloc.h>

void cleanString(char (*strArray)[]) {
    int i;
    for (i = 0; i < strlen(*strArray); ++i) {
        if (!isalpha((*strArray)[i])) {
            memmove(&(*strArray)[i], &(*strArray)[i + 1], strlen(*strArray) - i);
            i--;
        }
    }

}

char *makeAcronym(char (*strArray)[], int num) {
    char *ptr = malloc(sizeof(char) * (num + 1));
    char *words = strtok(*strArray, " ");
    int index = 0;
    while (words != NULL) {
        char *cUP = strdup(words);
        ptr[index] = toupper(*cUP);
        free(cUP);
        words = strtok(NULL, " ");
        index++;
    }
    ptr[index] = '\0';
    free(words);
    return ptr;
}

int main() {
    char str1[] = "1809 Lincoln!";
    char (*strPtr)[] = &str1;

    cleanString(strPtr);
    printf("%s\n", str1);


    char str2[] = "As seen on screen";
    strPtr = &str2;
    char *str3 = makeAcronym(strPtr, 4);
    printf("%s\n", str3);

    free(str3);
    return 0;
}