// Team 19 - Yudong Lin & Wei Wei Chien

#ifndef LAB03_LOAN_H
#define LAB03_LOAN_H

typedef struct loan *LoanType;

LoanType create(float balance, float apr, int length);

void destroy(LoanType loanPtr);

LoanType consolidate(LoanType loanPtr1, LoanType loanPtr2);

void print(LoanType loanPtr);

char *toString(LoanType loanPtr);

#endif //LAB03_LOAN_H
