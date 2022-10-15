#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    char *myarg;
    char **ourarray = malloc(sizeof(char *) * argc);
    printf("hello world \n");
    printf("number of args=%d \n", argc);
    int j = argc - 1;
    for (int i = 0; i < argc; i++) {
        printf("arg %d=%s\n", i, argv[i]);
        char *s = malloc(sizeof(char) * strlen(argv[i]));
        strcpy(s, argv[i]);
        //ourarray[j] = argv[i];
        ourarray[j] = s;
        j--;
    }

    myarg = argv[0];
    printf("addr of myarg=%lu val of myarg=%s\n", myarg, myarg);
    printf("The NULL char that terminates my string=%d\n", *(myarg + 6));
    printf("myarg char 0=%d %c\n", *(myarg + 0), *(myarg + 0));
    printf("myarg char 1=%d %c\n", *(myarg + 1), *(myarg + 1));
    printf("myarg char 2=%d %c\n", *(myarg + 2), *(myarg + 2));
    printf("myarg char 3=%d %c\n", *(myarg + 3), *(myarg + 3));
    printf("myarg char 4=%d %c\n", *(myarg + 4), *(myarg + 4));
    printf("myarg char 5=%d %c\n", *(myarg + 5), *(myarg + 5));
    printf("myarg char 6=%d %c\n", *(myarg + 6), *(myarg + 6));

    for (int i = 0; i < argc; i++)
        printf("ourarray arg %d=%s\n", i, ourarray[i]);

    *(ourarray[0] + 1) = 'b';
    for (int i = 0; i < argc; i++)
        printf("ourarray[%d]=%s –- argv[%d]=%s\n", i, ourarray[i], i, argv[i]);

    return 0;
}
