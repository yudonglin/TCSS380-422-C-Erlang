#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

long toByteSize(char size[], char format[]) {
    long l_size = atol(size);
    if (strcmp(format, "GB") == 0) {
        return l_size * 1024 * 1024 * 1024;
    } else if (strcmp(format, "MB") == 0) {
        return l_size * 1024 * 1024;
    } else if (strcmp(format, "KB") == 0) {
        return l_size * 1024;
    }
    return l_size;
}

int main(int argc, char *argv[]) {

    if (argc >= 5) {
        long virtual_address_space = toByteSize(argv[1], argv[2]);

        long page_size = toByteSize(argv[3], argv[4]);

        long numOfPages = virtual_address_space / page_size;

        int virtual_address = (int) log2(virtual_address_space);

        int vpn = (int) log2(numOfPages);

        printf("Address space: %ld byte\n", virtual_address_space);

        printf("Page size: %ld byte\n", page_size);

        printf("Virtual address: %d bit\n", virtual_address);

        printf("VPN: %d bit\n", vpn);

        printf("Offset: %d bit\n", virtual_address - vpn);

        printf("number of pages: %ld (2^%d)\n", numOfPages, vpn);

        long pte = (long) ceil((float) vpn / 8);

        printf("PTE size (Page Table Entry Size): %ld bytes\n", pte);

        long page_table_size = numOfPages * pte;

        if (page_table_size > 1024 * 1024) {
            printf("Page table size: %ld MB\n", page_table_size / (1024 * 1024));
        } else if (page_table_size > 1024) {
            printf("Page table size: %ld KB\n", page_table_size / 1024);
        } else {
            printf("Page table size: %ld bytes\n", page_table_size);
        }

        return 0;
    } else {
        printf("Invalid argv  %d!\n", argc);
        return 1;
    }
}



