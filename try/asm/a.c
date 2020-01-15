#include <smmintrin.h>
#include <immintrin.h>
#include <iostream>

using namespace std;

void add_no_AVX(int size, int *first_array, int *second_array) {
    for (int i = 0; i < size; ++i) {
        first_array[i] += second_array[i];
    }
}

void add_AVX(int size, uint8_t *a1, uint8_t *a2) {
    int i = 0;
    __m256i *p1 = (__m256i *) a1;
    __m256i *p2 = (__m256i *) a2;
    for (; i + 32 <= size; ++i, ++p1, ++p2) {
        cout << "LOL" << endl;
        //*p1 = _mm256_adds_epu8(*p1, *p2);
        *p1 = *p2;
    }
}

#define SIZE 32

int main() {
    uint8_t a[SIZE];
    uint8_t b[SIZE];
    int i = 0;
    for (; i < SIZE; i++) {
        a[i] = i;
        b[i] = 100 + i;
    }
    add_AVX(SIZE, a, b);
    cout << (256 << 1) << endl << unsigned(a[1]) << endl;
}
