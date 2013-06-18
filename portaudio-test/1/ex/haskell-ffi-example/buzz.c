#include "buzz.h"
#include "stdio.h"

void buzz(int array_length, int array[]) {
    int i;
    printf("There are %i members in the array: ", array_length);

    for (i = 0; i < array_length; i++) {
        printf(" %i", array[i]);
    }

    printf("\n");
}

