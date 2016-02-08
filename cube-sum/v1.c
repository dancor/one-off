#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

void main(int argc, char *argv[]) {
    struct my_struct *s;
    struct timeval tv1, tv2;

    gettimeofday(&tv1, NULL);

    unsigned long long a = atoll(argv[1]);
    unsigned long long a3 = a * a * a;
    for (unsigned long long b = 1; b <= a - 2; b++) {
        unsigned long long a3b3 = a3 + b * b * b;
        unsigned long long c_min = ceil(cbrt((double)a3b3 / 2));
        for (unsigned long long c = c_min; c <= a - 1; c++) {
            unsigned long long d3 = a3b3 - c * c * c;
            unsigned long long d = round(cbrt((double)d3));
            if (d * d * d == d3) {
                printf("(%lld,%lld,%lld,%lld)\n", a, b, c, d);
            }
        }
    }

    gettimeofday(&tv2, NULL);
    fprintf(stderr, "%lld: %fs\n", a,
        (double)(tv2.tv_sec - tv1.tv_sec) +
        (double)(tv2.tv_usec - tv1.tv_usec) / 1000000);
}
