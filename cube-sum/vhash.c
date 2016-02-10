#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include "uthash.h"

// Largest x where 2x^3 doesn't overflow.
unsigned long long cube_max = 2097151;

struct my_struct {
    unsigned long long cube_n;
    unsigned long long cube_n3;
    UT_hash_handle hh;
};

struct my_struct *cubes = NULL;



void main(int argc, char *argv[]) {
    struct my_struct *s;
    struct timeval tv1, tv2;

    /*
    for (unsigned long long n = 1; n <= 9999; n++) {
        s = (struct my_struct*)malloc(sizeof(struct my_struct));
        s->cube_n = n;
        s->cube_n3 = n * n * n;
        HASH_ADD(hh, cubes, cube_n3, sizeof(unsigned long long), s);
    }
    */

    gettimeofday(&tv1, NULL);

    for (int argn = 1; argn < argc; argn++) {
        unsigned long long a = atoll(argv[argn]);
        unsigned long long a3 = a * a * a;
        for (unsigned long long b = 1; b <= a - 2; b++) {
            unsigned long long a3b3 = a3 + b * b * b;
            unsigned long long c_min = ceil(cbrt((double)a3b3 / 2));
            for (unsigned long long c = c_min; c <= a - 1; c++) {
                unsigned long long d3 = a3b3 - c * c * c;
                unsigned long long d = round(cbrt((double)d3));
                HASH_FIND(hh, cubes, &d3, sizeof(unsigned long long), s);
                //if (s != NULL) {
                if (d * d * d == d3) {
                    //printf("(%lld,%lld,%lld,%lld)\n", a, b, c, s->cube_n);
                    printf("(%lld,%lld,%lld,%lld)\n", a, b, c, d);
                }
            }
        }
    }

    gettimeofday(&tv2, NULL);
    fprintf(stderr, "%fs\n",
        (double)(tv2.tv_sec - tv1.tv_sec) +
        (double)(tv2.tv_usec - tv1.tv_usec) / 1000000);
}
