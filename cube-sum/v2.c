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
    struct timeval tv1, tv2;
    cache_t *cache;
    // 1 GB
    int cache_len = 134217728 * 8;
    // cache_pow 27 

    //int misses = 0;
    //int hits = 0;

    gettimeofday(&tv1, NULL);

    cache = (cache_t*)malloc(cache_len * sizeof(cache_t));
    memset(cache, 0, cache_len * sizeof(cache_t));

    n_t a = atoll(argv[1]);
    n_t a3 = a * a * a;

    for (n_t x = 1; x <= a; x++) {
        n_t x3 = x * x * x;
        cache[x3 % cache_len] = 1;
        /*
        uint32_t cache_i = x3 % cache_len;
        if (cache[cache_i] == 0) {
            cache[cache_i] = 1;
        } else {
            cache[cache_i] = 2;
            //printf("Collision: %lu, %lu\n", x, cache[cache_i]);
        }
        */
    }


    for (n_t b = 1; b <= a - 2; b++) {
        n_t a3b3 = a3 + b * b * b;
        n_t c_min = ceil(cbrt((float)a3b3 / 2));
        for (n_t c = c_min; c <= a - 1; c++) {
            n_t d3 = a3b3 - c * c * c;

            if (cache[d3 % cache_len] == 0) continue;

            n_t d = round(cbrt((float)d3));
            if (d * d * d == d3) {
                //hits++;
                n_t d = round(cbrt((float)d3));
                printf("(%lu,%lu,%lu,%lu)\n", a, b, c, d);
            } else {
                //misses++;
            }
        }
    }
    //printf("%d hits\n", hits);
    //printf("%d misses\n", misses);

    gettimeofday(&tv2, NULL);
    fprintf(stderr, "%s: %fs\n", argv[1],
        (double)(tv2.tv_sec - tv1.tv_sec) +
        (double)(tv2.tv_usec - tv1.tv_usec) / 1000000);
}
