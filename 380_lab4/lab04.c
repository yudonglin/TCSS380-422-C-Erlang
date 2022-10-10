//
// Created by yudong on 5/1/22.
//
#include <stdio.h>

int sum(int boundary);

int main() {
    printf("%d", sum(-133));

    return 0;
}

int sum(int boundary) {
    int i, sum = 0;
    for (i = 1; i <= boundary; i++)
        sum += i;
    return sum;
}