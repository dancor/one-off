#pragma once
#include <errno.h>  // errno
#include <stdlib.h> // exit
#include <stdio.h>  // stderr
#include <string.h> // strerror
#include "types.h"  // i32
inline i32 negI32Errs(const i32 r, const char *file, const int line) {
  if (r >= 0) return r;
  fprintf(stderr, "Aborting on value %d at %s line %d; error %s.\n", 
    r, file, line, strerror(errno));
  exit(1);}
inline void nullErrs(const void* r, const char *file, const int line) {
  if (r) return;
  fprintf(stderr, "Aborting at %s line %d; error %s.\n", 
    file, line, strerror(errno));
  exit(1);}
inline void zeroI32Errs(const i32 r, const char *file, const int line) {
  if (r) return;
  fprintf(stderr, "Aborting at %s line %d; error %s.\n", 
    file, line, strerror(errno));
  exit(1);}
#define nie(a) negI32Errs((a), __FILE__, __LINE__)
#define nue(a) {nullErrs((a), __FILE__, __LINE__);}
#define zie(a) {zeroI32Errs((a), __FILE__, __LINE__);}
#ifdef __CUDACC__
inline void cudaCheck(const cudaError_t r, const char *file, const int line) {
  if (!r) return;
  fprintf(stderr, "Aborting on CUDA error %s at %s line %d.\n", 
    cudaGetErrorString(r), file, line);
  exit(1);}
#define cuc(a) {cudaCheck((a), __FILE__, __LINE__);}
#endif
/* check for gl loaded for this
inline void glErrDie(const char *file, int line) {
  GLenum e = glGetError(); if (!e) return; while (e) {
    switch (e) {
    case GL_INVALID_ENUM:      fprintf(stderr, "INVALID_ENUM\n");      break;
    case GL_INVALID_VALUE:     fprintf(stderr, "INVALID_VALUE\n");     break;
    case GL_INVALID_OPERATION: fprintf(stderr, "INVALID_OPERATION\n"); break;
    case GL_STACK_OVERFLOW:    fprintf(stderr, "STACK_OVERFLOW\n");    break;
    case GL_STACK_UNDERFLOW:   fprintf(stderr, "STACK_UNDERFLOW\n");   break;
    case GL_OUT_OF_MEMORY:     fprintf(stderr, "OUT_OF_MEMORY\n");     break;
    case GL_INVALID_FRAMEBUFFER_OPERATION:
      fprintf(stderr, "INVALID_FRAMEBUFFER_OPERATION\n"); break;
    default: fprintf(stderr, "UNKNOWN ERROR\n"); break;}
    e = glGetError();}
  fprintf(stderr, "Aborting on OpenGL error at %s line %d.\n", file, line);
  exit(e);}
void glCustomErr(const char *e, const char *file, int line) {
  fprintf(stderr, "Aborting at %s line %d: %s\n", file, line, e);
  glErrDie(file, line);
  exit(1);}
#define ged() glErrDie(__FILE__, __LINE__) 
#define gce(e) glCustomErr(e, __FILE__, __LINE__)*/
