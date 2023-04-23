#pragma once
#include <iostream>
#include <source_location>
using namespace std;
template<typename A> A
nge(A a, const source_location&l=source_location::current()){ // negErrs
  const A zero = 0; if (a >= zero) return a;
  cerr<<"Aborting@nge:"<<l.file_name()<<":"<<l.line()<<":"<<l.function_name()
    <<":"<<a<<"\n";exit(1);}
template<typename A> A
zre(A a, const source_location&l=source_location::current()){ // zeroErrs
  if (a) return a;
  cerr<<"Aborting@zre:"<<l.file_name()<<":"<<l.line()<<":"<<l.function_name()
    <<":"<<a<<"\n";exit(1);}
#ifdef __CUDACC__
inline cudaError_t cuc(const cudaError_t r,
    const source_location&l=source_location::current()){ // cudaCheck
  if (!r) return r;
  cerr<<"Aborting@cuc:"<<l.file_name()<<":"<<l.line()<<":"<<l.function_name()
    <<":"<<a<<":"<<cudaGetErrorString(r)<<"\n";exit(1);}
#endif
