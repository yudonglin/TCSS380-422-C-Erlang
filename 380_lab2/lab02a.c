// Team 19 - Yudong Lin & Wei Wei Chien
// when compiling this code use:  gcc lab02a.c -lm
// since gcc will not link in the math library without the flag -lm

#include<stdio.h>
#include<math.h>

// 6. provide a function prototype for mysum
double mysum(int start, int step, int stop, double (*fn) (double));

int main() {

   int i = 1, j = 10;

   // 1. create a function pointer called ptr capable of taking one argument of type double
   // and returning a double
    double (*ptr) (double);

   // 2. assign function log (from the math library to ptr)
    ptr = &log;

   printf("sum of logs from %d to %d: %.4f\n", i, j, mysum(i, 1, j, ptr));

   
   // 3. assign function sqrt (from the math library to ptr)
    ptr = &sqrt;

   printf("sum of roots from %d to %d: %.4f\n", i, j, mysum(i, 1, j, ptr));

   
   // 4. assign function exp (from the math library to ptr)
    ptr = &exp;
   

   printf("sum of powers of e from %d to %d: %.4f\n", i, j, mysum(i, 1, j, ptr));


   return 0;
}

// returns the sum of logs, square roots, or powers of e between start and stop
// given a step/increment value
double mysum(int start, int step, int stop, double (*fn) (double)/* 5. create function pointer parameter called fn */ ) {
   
   int i;
   double accumulator = 0;

   for (i = start; i <= stop; i = i + step) {
	accumulator += fn(i); /* 6. call fn with i as argument */
   }

   return accumulator;

}
