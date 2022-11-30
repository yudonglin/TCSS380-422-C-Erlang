/*
 *  pcmatrix module
 *  Primary module providing control flow for the pcMatrix program
 *
 *  Producer consumer bounded buffer program to produce random matrices in parallel
 *  and consume them while searching for valid pairs for matrix multiplication.
 *  Matrix multiplication requires the first matrix column count equal the
 *  second matrix row count.
 *
 *  A matrix is consumed from the bounded buffer.  Then matrices are consumed
 *  from the bounded buffer, one at a time, until an eligible matrix for multiplication
 *  is found.
 *
 *  Totals are tracked using the ProdConsStats Struct for each thread separately:
 *  - the total number of matrices multiplied (multtotal from each consumer thread)
 *  - the total number of matrices produced (matrixtotal from each producer thread)
 *  - the total number of matrices consumed (matrixtotal from each consumer thread)
 *  - the sum of all elements of all matrices produced and consumed (sumtotal from each producer and consumer thread)
 *  
 *  Then, these values from each thread are aggregated in main thread for output
 *
 *  Correct programs will produce and consume the same number of matrices, and
 *  report the same sum for all matrix elements produced and consumed.
 *
 *  Each thread produces a total sum of the value of
 *  randomly generated elements.  Producer sum and consumer sum must match.
 *
 *  University of Washington, Tacoma
 *  TCSS 422 - Operating Systems
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <assert.h>
#include <time.h>
#include "matrix.h"
#include "counter.h"
#include "prodcons.h"
#include "pcmatrix.h"

int main (int argc, char * argv[])
{
    // Process command line arguments
    int numw = NUMWORK;
    if (argc == 1) {
        BOUNDED_BUFFER_SIZE = MAX;
        NUMBER_OF_MATRICES = LOOPS;
        MATRIX_MODE = DEFAULT_MATRIX_MODE;
        printf("USING DEFAULTS: worker_threads=%d bounded_buffer_size=%d matricies=%d matrix_mode=%d\n", numw,
               BOUNDED_BUFFER_SIZE, NUMBER_OF_MATRICES, MATRIX_MODE);
    } else {
        if (argc == 2) {
            numw = atoi(argv[1]);
            BOUNDED_BUFFER_SIZE = MAX;
            NUMBER_OF_MATRICES = LOOPS;
            MATRIX_MODE = DEFAULT_MATRIX_MODE;
        }
        if (argc == 3) {
            numw = atoi(argv[1]);
            BOUNDED_BUFFER_SIZE = atoi(argv[2]);
            NUMBER_OF_MATRICES = LOOPS;
            MATRIX_MODE = DEFAULT_MATRIX_MODE;
        }
        if (argc == 4) {
            numw = atoi(argv[1]);
            BOUNDED_BUFFER_SIZE = atoi(argv[2]);
            NUMBER_OF_MATRICES = atoi(argv[3]);
            MATRIX_MODE = DEFAULT_MATRIX_MODE;
        }
        if (argc == 5) {
            numw = atoi(argv[1]);
            BOUNDED_BUFFER_SIZE = atoi(argv[2]);
            NUMBER_OF_MATRICES = atoi(argv[3]);
            MATRIX_MODE = atoi(argv[4]);
        }
        printf("USING: worker_threads=%d bounded_buffer_size=%d matricies=%d matrix_mode=%d\n", numw,
               BOUNDED_BUFFER_SIZE,
               NUMBER_OF_MATRICES, MATRIX_MODE);
    }

    time_t t;
    // Seed the random number generator with the system time
    srand((unsigned) time(&t));

    //
    // Demonstration code to show the use of matrix routines
    //
    // ----------------------------------------------------------

    bigmatrix = (Matrix **) malloc(sizeof(Matrix *) * BOUNDED_BUFFER_SIZE);

    printf("MATRIX MULTIPLICATION DEMO:\n\n");

    printf("main: begin \n");

    // initialize a global synchronized shared counter
    counters_t *synchronized_shared_counter = malloc(sizeof(counters_t));
    synchronized_shared_counter->cons = malloc(sizeof(counter_t));
    init_cnt(synchronized_shared_counter->cons);
    synchronized_shared_counter->prod = malloc(sizeof(counter_t));
    init_cnt(synchronized_shared_counter->prod);

    // an array of producer threads
    pthread_t prod_threads[numw];
    // an array of consumer threads
    pthread_t cons_threads[numw];

    // creating threads
    for (int i = 0; i < numw; ++i) {
        pthread_create(&prod_threads[i], NULL, prod_worker, synchronized_shared_counter);
        pthread_create(&cons_threads[i], NULL, cons_worker, synchronized_shared_counter);
    }

    // The sum of all elements of matrices produced.
    int sum_total_prod = 0;
    // The sum of all elements of matrices consumed.
    int sum_total_cons = 0;
    // The total number of matrices multiplied by consumer threads.
    int multi_total = 0;
    // The total number of matrices produced by producer threads.
    int matrix_total_prod = 0;
    // The total number of matrices consumed by consumer threads.
    int matrix_total_cons = 0;

    // pointer for retrieving the result ProdConsStats pointer from each thread
    void *returnValue;
    // join the all threads
    for (int i = 0; i < numw; ++i) {
        // join a producer thread
        pthread_join(prod_threads[i], &returnValue);
        // update data receive from producer thread and free the result pointer after updating the value
        sum_total_prod += ((ProdConsStats *) returnValue)->sumtotal;
        matrix_total_prod += ((ProdConsStats *) returnValue)->matrixtotal;
        free(returnValue);
        // join a consumer thread
        pthread_join(cons_threads[i], &returnValue);
        // update data receive from consumer thread and free the result pointer after updating the value
        sum_total_cons += ((ProdConsStats *) returnValue)->sumtotal;
        multi_total += ((ProdConsStats *) returnValue)->multtotal;
        matrix_total_cons += ((ProdConsStats *) returnValue)->matrixtotal;
        free(returnValue);
    }

    // display the result
    printf("\nIn conclusion:\n");
    printf("length of threads array: %d\n", numw);
    printf("BOUNDED_BUFFER_SIZE: %d\n", BOUNDED_BUFFER_SIZE);
    printf("Sum of Matrix elements --> Produced = %d Consumed = %d\n", sum_total_prod, sum_total_cons);
    printf("multiplied: %d\n", multi_total);
    printf("matrix total produced: %d\n", matrix_total_prod);
    printf("matrix total consumed: %d\n", matrix_total_cons);
    printf("all produced: %d\n", get_cnt(synchronized_shared_counter->prod));
    printf("all consumed: %d\n", get_cnt(synchronized_shared_counter->cons));

    free(synchronized_shared_counter);
    return 0;
    // ----------------------------------------------------------



    printf("Producing %d matrices in mode %d.\n", NUMBER_OF_MATRICES, MATRIX_MODE);
    printf("Using a shared buffer of size=%d\n", BOUNDED_BUFFER_SIZE);
    printf("With %d producer and consumer thread(s).\n", numw);
    printf("\n");

    // Here is an example to define one producer and one consumer
    pthread_t pr;
    pthread_t co;

    // Add your code here to create threads and so on


    // These are used to aggregate total numbers for main thread output
    int prs = 0; // total #matrices produced
    int cos = 0; // total #matrices consumed
    int prodtot = 0; // total sum of elements for matrices produced
    int constot = 0; // total sum of elements for matrices consumed
    int consmul = 0; // total # multiplications

    // consume ProdConsStats from producer and consumer threads [HINT: return from join]
    // add up total matrix stats in prs, cos, prodtot, constot, consmul

    printf("Sum of Matrix elements --> Produced=%d = Consumed=%d\n", prs, cos);
    printf("Matrices produced=%d consumed=%d multiplied=%d\n", prodtot, constot, consmul);

  return 0;
}
