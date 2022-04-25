// Yudong Lin & Wei Wei Chien
#include<stdio.h>
int main (void)  {
	int *myptr, *t, k;
    printf("Location of myptr in the first place: %p\n", myptr);
    printf("Location of t: %p\n", &t);
    printf("Location of k: %p\n", &k);
	myptr = t = NULL; 
	myptr = &k;
    printf("Value of myptr: %d\n", *myptr);
    printf("Value of t: %p\n", t);
    printf("Value of k: %d\n", k);
	*myptr = 13;
    printf("Value of myptr: %d\n", *myptr);
    printf("Value of t: %p\n", t);
    printf("Value of k: %d\n", k);
	//*t = *myptr;
	k = 3;
	//*t = k;
	//myptr = 456;
    printf("Value of myptr: %d\n", *myptr);
    printf("Value of t: %p\n", t);
    printf("Value of k: %d\n", k);
    printf("Location of myptr afterwards: %p\n", myptr);
       return 0;
}
