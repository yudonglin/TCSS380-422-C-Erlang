/*
Team 19 - Yudong Lin & Wei Wei Chien

comment #1: square.c does not contain the main() function required when you are trying to compile and run the program
 directly.

comment #2: First, because we define four_bytes as a type aliasing of integer, the four_bytes in the main() function was
 replaced by int during the preprocessing stage. Secondly, since we define integer PRINT as a constant integer equal to
 1, the gcc realizes that the second if statement will never be executed; thus, the second if statement is removed.

comment #3: Similar to what we have brought up in comment #2, since we define integer PRINT as a constant integer equal
 to 2, the first section of the if-statement will never be executed; thus, it is removed during the preprocessing stage.
 The second if-statement itself was also removed. Since the second section will always be executed, the program
 considered it unnecessary to even check the second if-statement itself.

comment #4: Because of the preprocessing, the first if-statement and the section under if-statement were removed; thus,
 it will never be executed. Since the second section under the if-statement will always be executed. We now see the
 result produced by macro MY_SQUARE (100). Meanwhile, we also add our custom macro MY_DOUBLE and a printf statement. As
 a result, we now see the output produced by our macro MY_DOUBLE(MY_X), which is 20.

comment #5: After compiling main.c with -S flag, a file named "main.s" is created. It contains the assembly code
 generated using the main.c file.

comment #6: After compiling main.c with -c flag, a file named "main.o" is created. It contains the object code generated
 using the main.c file.

comment #7: ./myprog
*/

#include "square.c"

#define MY_X 10
#define MY_SQUARE(x) ((x) * (x))
#define MY_DOUBLE(N) ((N) * (2))
#define PRINT  2
#define my_bytes int

int main() {

   // this program shows the usage of pre-processor directives 

    my_bytes	x, s1, s2;

   #if PRINT == 1
       printf("Enter any integer: ");
       scanf("%d",&x);
       s1 = Square(x);
       printf("Function: The square of %d is %d\n", x, s1);
   #endif
   #if PRINT == 2
      s1 = MY_X;
      s2 = MY_SQUARE(s1);
      printf("Macro: The square of %d is %d\n", MY_X, s2);
   #endif

      printf("Macro: The double of %d is %d\n", MY_X, MY_DOUBLE(MY_X));
}
