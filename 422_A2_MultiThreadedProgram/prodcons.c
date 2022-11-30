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

Matrix *try_get(ProdConsStats *thread_stats, counters_t *synchronized_shared_counter) {
    Matrix *temp = NULL;
    pthread_mutex_lock(&mutex);
    while (count == 0) {
        pthread_cond_wait(&full, &mutex);
    }
    if (get_cnt(synchronized_shared_counter->cons) < NUMBER_OF_MATRICES) {
        temp = get();
        thread_stats->matrixtotal++;
        thread_stats->sumtotal += SumMatrix(temp);
        increment_cnt(synchronized_shared_counter->cons);
    } else {
        count--;
    }
    pthread_cond_signal(&empty);
    pthread_mutex_unlock(&mutex);
    return temp;
}

// Matrix PRODUCER worker thread
void *prod_worker(void *arg) {
    // obtain the synchronized shared counter
    counters_t *synchronized_shared_counter = arg;
    // define ProdConsStats locally, so it can be returned to the main thread from pthread_join
    ProdConsStats *thread_stats = malloc(sizeof(ProdConsStats));
    thread_stats->sumtotal = 0;
    thread_stats->matrixtotal = 0;

    Matrix *bm;
    while (1) {
        bm = GenMatrixRandom();
        pthread_mutex_lock(&mutex);
        while (count >= BOUNDED_BUFFER_SIZE) {
            pthread_cond_wait(&empty, &mutex);
        }
        if (get_cnt(synchronized_shared_counter->prod) >= NUMBER_OF_MATRICES) {
            FreeMatrix(bm);
            count++;
            pthread_cond_signal(&full);
            pthread_mutex_unlock(&mutex);
            break;
        }
        put(bm);
        thread_stats->matrixtotal++;
        thread_stats->sumtotal += SumMatrix(bm);
        increment_cnt(synchronized_shared_counter->prod);
        pthread_cond_signal(&full);
        pthread_mutex_unlock(&mutex);
    }
    return thread_stats;
}

// Matrix CONSUMER worker thread
void *cons_worker(void *arg) {
    // obtain the synchronized shared counter
    counters_t *synchronized_shared_counter = arg;
    // define ProdConsStats locally, so it can be returned to the main thread from pthread_join
    ProdConsStats *thread_stats = malloc(sizeof(ProdConsStats));
    thread_stats->sumtotal = 0;
    thread_stats->multtotal = 0;
    thread_stats->matrixtotal = 0;

    Matrix *m1;
    Matrix *m2;
    while (1) {
        // obtain first matrix m1
        m1 = try_get(thread_stats, synchronized_shared_counter);
        // the NUMBER_OF_MATRICES has been reach, exit
        if (m1 == NULL) {
            break;
        }

        // try to obtain a valid m2 and do the calculation
        while (1) {
            m2 = try_get(thread_stats, synchronized_shared_counter);
            // the NUMBER_OF_MATRICES has been reach, exit
            if (m2 == NULL) {
                FreeMatrix(m1);
                return thread_stats;
            }

            // doing calculation
            Matrix *m3 = MatrixMultiply(m1, m2);

            thread_stats->multtotal++;

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
                printf("------------------------------\n");
            }
        }
    }
    return thread_stats;
}