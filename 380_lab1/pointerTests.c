// Yudong Lin & Wei Wei Chien
#include<stdio.h>

void sortnum(double*, double*);

int main(void) {
	double num1 = 53, num2 = 30;

	sortnum(&num1, &num2);

	printf("smaller value %.2f\n", num1);
	printf("larger value %.2f\n", num2);

	return 0;

}


void sortnum(double* pt1, double* pt2) {

	double aux;
	if (*pt2 < * pt1) {
		aux = *pt2;
		*pt2 = *pt1;
		*pt1 = aux;
	}
}



