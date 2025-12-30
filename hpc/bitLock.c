#include "i.h"
u1t tryGetLock(u8t *w) {u1t r; asm volatile("lock btsq $63, %0\nsetc %1":"+m"(
  *w),"=r"(r)::"memory","cc"); return r;} // ret 0 iff we got the lock
void releaseLock(u8t *w) {asm volatile("":::"memory"); *w &= ~(1ULL<<63);}
int main() {
  u8t w = 0;
  bool ret = tryGetLock(&w);
  printf("ret: %u\n", ret);
  ret = tryGetLock(&w);
  printf("ret: %u\n", ret);
  ret = tryGetLock(&w);
  printf("ret: %u\n", ret);
  printf("releasing now\n");
  releaseLock(&w);
  ret = tryGetLock(&w);
  printf("ret: %u\n", ret);
  ret = tryGetLock(&w);
  printf("ret: %u\n", ret);
}
/*
  // It's smarter to use _mm_pause() to tell the thread to be low-power and do
  // no speculative overhead, rather than yield the thread which is thousands
  // of cycles and we don't want to have extra threads around anyway using up
  // eg caches.
  while (tryGetLock(&w)) _mm_pause();
*/
