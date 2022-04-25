// Team 19 - Yudong Lin & Wei Wei Chien

#include <stdio.h>
#include "loan.h"

int main() {
    LoanType loan1 = create(100.11f, 33.44f, 30);
    printf("Loan 1: ");
    print(loan1);
    LoanType loan2 = create(200.95f, 70.04f, 11);
    printf("Loan 2: ");
    print(loan2);

    LoanType loan3 = consolidate(loan1, loan2);
    printf("Consolidate Loan 1 and 2: ");
    print(loan3);

    destroy(loan1);
    destroy(loan2);
    destroy(loan3);

    return 0;
}
