// Yudong Lin & Wei Wei Chien
#include <stdio.h>
#include <string.h>

#define SIZE 10

void myprint(char[], int);

int main(void) {

	char mystring1[SIZE] = "strings";
	char mystring2[5];

	myprint(mystring1, SIZE);
	myprint(mystring2, 5);

	strcpy(mystring2, "game");
	
	myprint(mystring2, 5);

	strcpy(mystring2, mystring1);
      printf("my length is: %lu\n", strlen(mystring2));
	
	myprint(mystring1, SIZE);
	myprint(mystring2, 5);

	mystring2[3] = '\0';

	myprint(mystring1, SIZE);
	myprint(mystring2, 5);

	return 0;
}

void myprint(char mystring[], int mySize) {
	
	int i;

	// prints until null character regardless of actual string length
	printf("Echo: %s\n", mystring);

	// print all chars up to the designated size
	for (i = 0; i < mySize; i++)
		printf("index %d: char %c : ASCII val %d \n", i, mystring[i], mystring[i]);

	printf("\n-----\n");
}
