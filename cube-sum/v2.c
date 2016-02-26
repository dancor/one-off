// Note that for a <= 1392200:
// cbrt((float)(2*a*a*a) / 2) == a
//
// But for higher a, (double) is needed; (float) differs by 1.
//
// For this reason, this (float) version requires a <= 1392200.

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

typedef uint64_t n_t;

typedef uint8_t cache_t;

void main(int argc, char *argv[]) {
    struct my_struct *s;
    struct timeval tv1, tv2, tv3;
    cache_t *cache;
    cache_t *cache2;
    int cache_len = 1048573; // 2x 1 MiB: 150k 22s
    int cache2_len = 1048571;
    int misses = 0;

    gettimeofday(&tv1, NULL);

    cache = (cache_t*)malloc(cache_len * sizeof(cache_t));
    memset(cache, 0, cache_len * sizeof(cache_t));
    cache2 = (cache_t*)malloc(cache2_len * sizeof(cache_t));
    memset(cache, 0, cache2_len * sizeof(cache_t));

    n_t a = atoll(argv[1]);
    n_t a3 = a * a * a;

    for (n_t x = 1; x <= a; x++) {
        n_t x3 = x * x * x;
        cache[x3 % cache_len] = 1;
        cache2[x3 % cache2_len] = 1;
    }

    gettimeofday(&tv2, NULL);
    fprintf(stderr, "%s: %fs\n", argv[1],
        (double)(tv2.tv_sec - tv1.tv_sec) +
        (double)(tv2.tv_usec - tv1.tv_usec) / 1000000);

    for (n_t b = 1; b <= a - 2; b++) {
        n_t a3b3 = a3 + b * b * b;
        n_t c_min = ceil(cbrt((float)a3b3 / 2));
        for (n_t c = c_min; c <= a - 1; c++) {
            n_t d3 = a3b3 - c * c * c;

            if (cache[d3 % cache_len] == 0) continue;
            if (cache2[d3 % cache2_len] == 0) continue;

            n_t d = round(cbrt((float)d3));
            if (d * d * d == d3) {
                n_t d = round(cbrt((float)d3));
                printf("%lu,%lu,%lu,%lu\n", a, b, c, d);
            } else {
                misses++;
            }
        }
    }

    gettimeofday(&tv3, NULL);
    fprintf(stderr, "%s: %fs\n", argv[1],
        (double)(tv3.tv_sec - tv2.tv_sec) +
        (double)(tv3.tv_usec - tv2.tv_usec) / 1000000);
    fprintf(stderr, "misses: %d\n", misses);
}
