// Team 19 - Yudong Lin & Wei Wei Chien
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* repeated  (char*  original, int n);

int main(void) {
    char * newStr = repeated("bon", 2);
	printf("%s\n", newStr);
    free(newStr);
    newStr = repeated("bon", 3);
	printf("%s\n", newStr);
    free(newStr);
    newStr = repeated("bon", 4);
	printf("%s\n", newStr);
    free(newStr);

	return 0;
}

char* repeated  (char*  original, int n) {
	int i, length = strlen(original);
        char*  newString = malloc (sizeof(char) * length * n + 1);
        /*
         * Modern gcc compiler and system hardware can correctly handle most utf-8 characters. In most cases, the
         * strlen() function can provide you with an accurate result regarding how many bytes the char array takes up in
         * the memory (That is why the strlen() function can show you the correct string length only when there is no
         * non-ASCII code inside your char array). In this case, you can even go ahead and remove the "sizeof(char) *"
         * and everything will still work just fine.
         */
    char*  helper = newString;
	for (i = 0; i < n; i++) {
		strcpy( helper, original);
		helper = helper + length;
	}
	return newString;
}


