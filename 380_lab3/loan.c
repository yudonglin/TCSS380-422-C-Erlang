// Team 19 - Yudong Lin & Wei Wei Chien

#include <malloc.h>
#include "loan.h"
#include "string.h"

struct loan {
    float balance;
    float apr;
    int length;
};

LoanType create(float balance, float apr, int length) {
    LoanType ptr = malloc(sizeof(struct loan));
    if (ptr != NULL) {
        ptr->balance = balance;
        ptr->apr = apr;
        ptr->length = length;
    }
    return ptr;
}

void destroy(LoanType loanPtr) {
    free(loanPtr);
}

LoanType consolidate(LoanType loanPtr1, LoanType loanPtr2) {
    return create(
            loanPtr1->balance + loanPtr2->balance,
            loanPtr1->apr > loanPtr2->apr ? loanPtr2->apr : loanPtr1->apr,
            loanPtr1->length < loanPtr2->length ? loanPtr2->length : loanPtr1->length
    );
}

void print(LoanType loanPtr) {
    char *ptr = toString(loanPtr);
    printf("%s", ptr);
    free(ptr);
}

char *toString(LoanType loanPtr) {
    char strFormat[] = "balance %.2f USD, APR %.2f%c, length %d months\n";
    char *ptr = malloc(strlen(strFormat) * 8 + 1);
    sprintf(ptr, strFormat, loanPtr->balance, loanPtr->apr, 37, loanPtr->length);
    return ptr;
}
