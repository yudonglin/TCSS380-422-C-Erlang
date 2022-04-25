/*
   Team 19 - Yudong Lin & Wei Wei Chien

   The program below uses a 1d array and a 2d jagged array.
   The numbers in the 1d array store the number of columns to be used for each row in the 2d array.
   For example, if the 1d array consists of numbers 4, 3, 5, then 
   the first row in the 2d array has 4 elements,
   the second row in the 2d array has 3 elements,
   and the third row in the 2d array has 5 elements.

   After replacing the comments below with code, running the program with the following values, 
   should generate the output as shown:

	Enter the number of rows for your matrix: 3
	Enter the number of columns for row 0: 4
	Enter the number of columns for row 1: 3
	Enter the number of columns for row 2: 5
	
	0 0 0 0
	0 0 0  
	0 0 0 0 0 

	0 1 0 3
	1 0 3  
	0 3 0 5 0 

   Run the program with various values
*/

#include <stdio.h>
#include <stdlib.h>

void fill(int** matrix, int *cols, int rows);
// 1. provide a prototype for myprint function
void myprint(int **matrix, int *cols, int rows);

int main(void) {

	int rows, columns, i;
	int *cols;		//  a pointer for a 1D array to store the array of ints that indicate the number of columns for each 2d row
	int **jagged;		//  a pointer for a jagged matrix

	printf("Enter the number of rows for your matrix: ");
	scanf("%d", &rows);

    cols = (int *) calloc(rows, sizeof(int));// 2. dynamically allocate a 1d int array of 'rows' number of elements
    if (cols == NULL){
        printf("Not enough memory is available!");
        return 1;
    }
	jagged = malloc(sizeof(int *) * rows);	// allocates a 1d array of pointers
    if (jagged == NULL){
        printf("Not enough memory is available!");
        return 1;
    }

	for (i = 0; i < rows; i++) {
		printf("Enter the number of columns for row %d: ", i);
		scanf("%d", &columns);
		cols[i] = columns;
		jagged[i] = (int *) calloc(columns, sizeof(int));// 3. allocate a 1d int array of 'columns' number of elements that initializes the array to zeros
        if (jagged[i] == NULL){
            printf("Not enough memory is available!");
            return 1;
        }
    }
       
       // prints 2D array before filling it with data
       myprint(jagged, cols, rows);
       // assigns the 2D array
	  fill(jagged, cols, rows);
       // prints 2D array after filling it with data
	  myprint(jagged, cols, rows);	

	return 0;
}

// 4. write the definition for myprint that prints jagged matrix contents
void myprint(int **matrix, int *cols, int rows){

    int i, j;

    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols[i]; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
}

void fill(int** matrix, int *cols, int rows) {

	int i, j;

	for (i = 0; i < rows; i++) 
		for (j = 0; j < cols[i]; j++)
			if ((i + j) % 2)
				matrix[i][j] = i + j;
}
