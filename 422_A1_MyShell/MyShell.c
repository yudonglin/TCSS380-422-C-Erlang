#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <readline/readline.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>

#define PROMPT "MyShell> "
#define ARG_MAX 255
#define SPLIT_BY " "

/**
DONOT change the existing function definitions. You can add functions, if necessary.
*/

static int *status;

/**
  @brief Fork a child to execute the command using execvp. The parent should wait for the child to terminate
  @param args Null terminated list of arguments (including program).
  @return returns 1, to continue execution and 0 to terminate the MyShell prompt.
 */
int execute(char **args) {
    // create share memory
    status = mmap(NULL, sizeof *status, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    // set status to 1 (correct)
    *status = 1;
    // fork a child process
    int rc = fork();
    // child - run execvp
    if (rc == 0) {
        execvp(args[0], args);
        *status = 0;
        exit(0);
    }
        // parent - wait child
    else if (rc > 0) {
        wait(NULL);
        return *status;
    }
        // if fork fail
    else {
        *status = 0;
        exit(0);
    }
    return 0;
}


/**
  @brief gets the input from the prompt and splits it into tokens. Prepares the arguments for execvp
  @return returns char** args to be used by execvp
 */
char **parse(void) {
    // read commands from prompt
    char *cmd = readline(PROMPT);
    // allocated space for tokens
    char **tokens = malloc(ARG_MAX + 1 * sizeof(char *));
    // the index for each argument
    int index = 0;
    // get the pointer that points to the first argument
    char *ptr = strtok(cmd, SPLIT_BY);
    // go through the list and assign the argument string to the pointer
    while (ptr != NULL) {
        tokens[index++] = ptr;
        ptr = strtok(NULL, SPLIT_BY);
    }
    // make sure the list is null terminated at the correct spot
    tokens[index] = NULL;
    // return the arguments
    return tokens;
}


/**
   @brief Main function should run infinitely until terminated manually using CTRL+C or typing in the exit command
   It should call the parse() and execute() functions
   @param argc Argument count.
   @param argv Argument vector.
   @return status code
 */
int main(int argc, char **argv) {
    char **args;

    while (1) {

        // get the command from prompt
        args = parse();

        // if empty
        if (args[0] == NULL) {
            continue;
        }
            // if the user is trying to exit
        else if (strcmp(args[0], "exit") == 0) {
            break;
        }

        // executing the command
        if (execute(args) == 0) {
            printf("Error executing command!\n");
        }

        // free the space that was allocated
        free(args);
    }

    //return EXIT_SUCCESS;
    return EXIT_SUCCESS;
}
