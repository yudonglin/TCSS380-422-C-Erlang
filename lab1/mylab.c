// Yudong Lin & Wei Wei Chien
#include <stdio.h>

int multiply(int v[], int size);

int main(void) {

    int arr[5], i = 1;

    int n;
	for (n = 0; n < 5; n++){
        arr[n] = i++;
    }
	printf("the product of entered values is %d", multiply(arr, 5));
        return 0;
}

int multiply(int a[], int size) {
    int i;
	int total = 1;
    for (i = 0; i < size; i++){
        total *= a[i];
    }

	return total;
}