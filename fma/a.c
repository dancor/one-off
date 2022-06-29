// _mm256_fmadd_ps -> vfmadd132ps (or maybe other vfmadd???ps)
#include <immintrin.h>
#include <stdio.h>
#include <string.h>
void p8f(__m256 v){float x[8]; memcpy(x, &v, sizeof(x));
  printf("%2.2f %2.2f %2.2f %2.2f %2.2f %2.2f %2.2f %2.2f\n",
    x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]);}
int main() {
  __m256 a = _mm256_setr_ps(2,3,4,5,6,7,8,9);
  __m256 b = _mm256_setr_ps(10,100,1000,10000,
    1000000,10000000,10000000,1000000);
  //__m256 a = _mm256_set_ps(9,8,7,6,5,4,3,2);
  //__m256 b = _mm256_set_ps(10000000,10000000,1000000,100000,
  //  10000,1000,100,10);
  p8f(a);
  p8f(b);
  p8f(_mm256_fmadd_ps(a, b, a));
  /*
  float a = 2.12341234, b = 3.12341243, c = 4.1234124;
  a = a * b + c;
  d = _mm_fmadd_ps(a, b, c);
  printf("%f\n", a);
  */
}
