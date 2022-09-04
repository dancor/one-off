#pragma once
#include <errno.h>
#include <string.h>
#include "types.h"
#define noe(a) negOneErrs((a), __FILE__, __LINE__);
inline i32 negOneErrs(const i32 r, const char *file, const int line) {
  if (r != -1) return r;
  fprintf(stderr, "Aborting at %s line %d; error %s.\n", 
    file, line, strerror(errno));
  exit(1);
}
#define nie(a) negI32Errs((a), __FILE__, __LINE__);
inline i32 negI32Errs(const i32 r, const char *file, const int line) {
  if (r >= 0) return r;
  fprintf(stderr, "Aborting on value %d at %s line %d; error %s.\n", 
    r, file, line, strerror(errno));
  exit(1);
}
#define nue(a) nullErrs((a), __FILE__, __LINE__);
inline void* nullErrs(void* r, const char *file, const int line) {
  if (r) return r;
  fprintf(stderr, "Aborting at %s line %d; error %s.\n", 
    file, line, strerror(errno));
  exit(1);
}
#define zie(a) zeroI32Errs((a), __FILE__, __LINE__);
inline i32 zeroI32Errs(const i32 r, const char *file, const int line) {
  if (r) return r;
  fprintf(stderr, "Aborting at %s line %d; error %s.\n", 
    file, line, strerror(errno));
  exit(1);
}
#ifdef __CUDACC__
#define cuc(a) cudaCheck((a), __FILE__, __LINE__);
inline cudaError_t cudaCheck(const cudaError_t r, const char *file, const int line) {
  if (!r) return r;
  fprintf(stderr, "Aborting on CUDA error %s at %s line %d.\n", 
    cudaGetErrorString(r), file, line);
  exit(1);
}
#endif
