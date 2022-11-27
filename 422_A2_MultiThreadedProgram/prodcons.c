/*
 *  prodcons module
 *  Producer Consumer module
 *
 *  Implements routines for the producer consumer module based on
 *  chapter 30, section 2 of Operating Systems: Three Easy Pieces
 *
 *  University of Washington, Tacoma
 *  TCSS 422 - Operating Systems
 */

// Include only libraries for this module
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "counter.h"
#include "matrix.h"
#include "pcmatrix.h"
#include "prodcons.h"


// Define Locks, Condition variables, and so on here
int fill = 0;
int use = 0;
int count = 0;

pthread_cond_t empty, full;
pthread_mutex_t mutex;

// Bounded buffer put() get()
int put(Matrix *value) {
    bigmatrix[fill] = value;
    fill = (fill + 1) % BOUNDED_BUFFER_SIZE;
    count++;
    return 0;
}

Matrix *get() {
    Matrix *temp = bigmatrix[use];
    use = (use + 1) % BOUNDED_BUFFER_SIZE;
    count--;
    return temp;
}

Matrix *try_get(ProdConsStats * global_stats) {
    Matrix *temp = NULL;
    pthread_mutex_lock(&mutex);
    while (count == 0) {
        pthread_cond_wait(&full, &mutex);
    }
    if (global_stats->multtotal < NUMBER_OF_MATRICES) {
        temp = get();
        global_stats->multtotal++;
        global_stats->matrixtotal++;
        global_stats->sumtotal += SumMatrix(temp);
    }
    pthread_cond_signal(&empty);
    pthread_mutex_unlock(&mutex);
    return temp;
}

// Matrix PRODUCER worker thread
void *prod_worker(void *arg) {
    // obtain the global stats
    ProdConsStats *global_stats = arg;

    Matrix *bm;
    while (1) {
        bm = GenMatrixRandom();
        pthread_mutex_lock(&mutex);
        if (global_stats->multtotal >= NUMBER_OF_MATRICES) {
            FreeMatrix(bm);
            pthread_mutex_unlock(&mutex);
            break;
        }
        while (count == BOUNDED_BUFFER_SIZE) {
            pthread_cond_wait(&empty, &mutex);
        }
        put(bm);
        global_stats->matrixtotal++;
        global_stats->sumtotal += SumMatrix(bm);
        pthread_cond_signal(&full);
        pthread_mutex_unlock(&mutex);
    }

    return NULL;
}

// Matrix CONSUMER worker thread
void *cons_worker(void *arg) {
    // obtain the global stats
    ProdConsStats *global_stats = arg;

    Matrix *m1;
    Matrix *m2;
    while (1) {
        // obtain first matrix m1
        m1 = try_get(global_stats);
        // the NUMBER_OF_MATRICES has been reach, exit
        if (m1 == NULL) {
            break;
        }

        // try to obtain a valid m2 and do the calculation
        while (1) {
            m2 = try_get(global_stats);
            // the NUMBER_OF_MATRICES has been reach, exit
            if (m2 == NULL) {
                return NULL;
            }

            // doing calculation
            Matrix *m3 = MatrixMultiply(m1, m2);

            DisplayMatrix(m1, stdout);
            printf("    X\n");
            DisplayMatrix(m2, stdout);
            printf("    =\n");

            // free m2
            FreeMatrix(m2);

            // MatrixMultiply successful
            if (m3 != NULL) {
                DisplayMatrix(m3, stdout);
                FreeMatrix(m1);
                FreeMatrix(m3);
                printf("------------------------------\n");
                break;
            } else {
                DisplayMatrix(m3, stdout);
                printf("Fail match, retry with a new m2\n");
            }
        }
    }
}
