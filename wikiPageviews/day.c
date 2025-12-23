#include "i.h"
#include <pthread.h>
#include <xxhash.h>
#include <zlib.h>
// bucketN is per thread and then we merge to one
#define bucketN 16777216
#define ceilLog2NThreads 5
#define gzBufSize 65536
#define gzCompressedBufSize 65536
#define maxTitleLen 255
#define nThreads 32
// titlesAlloc is 2**30
#define titlesAlloc 1073741824
typedef u4t BucketI, EntryN, TitleOffset, ViewN;
#define pfBucketI "u"
#define pfViewN "u"
typedef u1t TitleLen;
typedef char *Title;
typedef u8t Hash;
typedef struct {
  char **filenames;
  Title titles, titlesOver, titlesP;
  TitleOffset *titleOffsets;
  ViewN *viewNs;
  EntryN entryN;
} WorkerArg;
typedef struct {TitleOffset titleOffset; ViewN viewN;} OutEntry;
WorkerArg as[nThreads];
pthread_barrier_t barriers[ceilLog2NThreads];

OutEntry *aToOutEntries(WorkerArg *a) {
  OutEntry *arr = eoz(malloc(a->entryN * sizeof(OutEntry))), *p = arr;
  for (BucketI i = 0; i < bucketN; i++) {
    if (!a->viewNs[i]) continue;
    p->titleOffset = a->titleOffsets[i];
    p++->viewN = a->viewNs[i];
  }
  return arr;
}
void radixSortDesc(OutEntry *a, EntryN n) {
  OutEntry *tmp = eoz(malloc(n * sizeof(*a)));
  static u4t cnt[65536];
  for (u4t pass = 0; pass < 2; pass++) {
    memset(cnt, 0, sizeof(cnt)); u4t c, sum = 0;
    // calculate the counts for each bucket
    for (EntryN i = 0; i < n; i++) cnt[(u2t)(a[i].viewN >> (pass * 16))]++;
    // prefix-sum those counts into start indexes
    for (EntryN i = 65536; i--;) {c = cnt[i]; cnt[i] = sum; sum += c;}
    // place correctly
    for (EntryN i = 0; i < n; i++)
      tmp[cnt[(u2t)(a[i].viewN >> (pass * 16))]++] = a[i];
    OutEntry *swap = a; a = tmp; tmp = swap;
  }
  free(tmp);
}
void gzDie(gzFile gz) {
  int errnum;
  fprintf(stderr, "gzread error: %s\n", gzerror(gz, &errnum));
  exit(1);
}
// Repeat gzread() until you fill len or the file ends. Die on any error.
// Return the total number of bytes read. So If we return less than len, the
// file ended. Even when we return len, maybe the next call will return 0.
//
// Although len is u4t, it must also be <= INT_MAX (**not** UINT_MAX) because
// of how we use gzread() which uses int as a return value. This constraint
// isn't checked.
u4t gzreads(gzFile gz, char *buf, u4t len) {
  if (unlikely(!len)) return 0;
  u4t ret = 0;
  for (;;) {
    i4t r = gzread(gz, buf, len);
    if (unlikely(-1 == r)) gzDie(gz);
    if (unlikely(!r)) {
      if (likely(gzeof(gz))) return ret;
      gzDie(gz);
    }
    buf += r; ret += r; if (likely(len == ret)) return len;
  }
}
void hInsert(WorkerArg *a, const char* title, TitleLen titleLen, ViewN viewN) {
  //printf("hInsert %u [%.*s]\n", viewN, titleLen, title);
  Hash hash = XXH3_64bits(title, titleLen);
  BucketI b = hash % bucketN;
  for (u4t inc = 1;; inc += 2) {
    if (likely(!a->viewNs[b])) {
      a->titleOffsets[b] = a->titlesP - a->titles;
      eoz(a->titlesP + titleLen + 1 <= a->titlesOver);
      memcpy(a->titlesP, title, titleLen); a->titlesP += titleLen;
      *a->titlesP++ = '\0';
      a->viewNs[b] = viewN;
      a->entryN++;
      return;
    }
    if (!memcmp(a->titles + a->titleOffsets[b], title, titleLen)) {
      eoz(!__builtin_uadd_overflow(a->viewNs[b], viewN, &a->viewNs[b]));
      return;
    }
    //eoz(inc < 99); // 1 .gz failed < 9 but passed < 99..
    eoz(inc < 99999);
    b += inc; b %= bucketN; // quadratic probing
  }
}
void merge1(WorkerArg *dst, const WorkerArg *src) {
  for (BucketI i = 0; i < bucketN; i++) {
    ViewN v = src->viewNs[i];
    if (!v) continue;
    const char *title = src->titles + src->titleOffsets[i];
    hInsert(dst, title, strlen(title), v);
  }
}
// Given a line from a Wiki pageview file (format:
// f"{wikiProject} {pagetitleUnderscoreForSpace} {viewN} 0\n"), skip the line
// in the usual case that wikiProject is neither "en" nor "en.m". Otherwise,
// put the pagetitle (prefix if over maxTitleLen) and viewN in our hashtable.
void procPageviewLine(WorkerArg *a, char *l, u4t len) {
  char *lOver = l + len;
  if (likely('e' != *l || 'n' != l[1])) return;
  if (unlikely('.' == l[2] && 'm' == l[3] && ' ' == l[4])) l += 5;
  else if (unlikely(' ' == l[2])) l += 3;
  else return;

  // uppercase first letter; pageview stats are from before that step
  if (*l >= 'a' && *l <= 'z') *l = *l - 'a' + 'A';
  // filter out certain classes of pagetitle
  if (unlikely(!strncmp(l, "Category:", 9))) return;
  if (unlikely(!strncmp(l, "File:", 5))) return;
  if (unlikely(!strncmp(l, "Help:", 5))) return;
  if (unlikely(!strncmp(l, "List_of", 7))) return;
  if (unlikely(!strncmp(l, "Portal:", 7))) return;
  if (unlikely(!strncmp(l, "Special:", 8))) return;
  if (unlikely(!strncmp(l, "Talk:", 5))) return;
  if (unlikely(!strncmp(l, "Template:", 9))) return;
  if (unlikely(!strncmp(l, "Template_talk:", 14))) return;
  if (unlikely(!strncmp(l, "User:", 5))) return;
  if (unlikely(!strncmp(l, "User_talk:", 10))) return;
  if (unlikely(!strncmp(l, "Wikipedia:", 10))) return;

  char *space = memchr(l, ' ', lOver - l); eoz(space);
  u4t titleLen = min(space - l, maxTitleLen);
  ViewN viewN = atoi(space + 1); eoz(viewN);
  hInsert(a, l, titleLen, viewN);
}
// Find any lines ending in '\n' in buf .. bufOver-1 to process.
// Move any remaining line segment to buf and update bufOver to just after it.
void procLines(WorkerArg *a, char *buf, char **bufOverP) {
  char *cur = buf, *lineStart = buf;
  for (;;) {
    if (unlikely(cur >= *bufOverP)) break;
    if (unlikely('\n' == *cur)) {
      procPageviewLine(a, lineStart, cur + 1 - lineStart);
      lineStart = cur + 1;
    }
    cur++;
  }
  u4t endLen = cur - lineStart;
  memmove(buf, lineStart, endLen);
  *bufOverP = buf + endLen;
}
void *workerFn(void *arg) {
  u4t tid = (intptr_t)arg;
  WorkerArg *a = as + tid;
  a->titles = eoz(malloc(titlesAlloc));
  a->titlesOver = a->titles + titlesAlloc;
  a->titlesP = a->titles;
  a->viewNs = eoz(calloc(bucketN, sizeof(ViewN)));
  a->titleOffsets = eoz(malloc(bucketN * sizeof(TitleOffset)));
  char buf[gzBufSize], *bufRightOver = buf + gzBufSize;
  for (u4t f = 0; a->filenames[f]; f++) {
    gzFile gz = gzopen(a->filenames[f], "rb"); if (!gz) gzDie(gz);
    gzbuffer(gz, gzCompressedBufSize);
    char *bufOver = buf;
    for (;;) {
      u4t freeSpace = bufRightOver - bufOver; eoz(freeSpace);
      u4t gotLen = gzreads(gz, bufOver, freeSpace);
      bufOver += gotLen;
      procLines(a, buf, &bufOver);
      if (gotLen < freeSpace) break; // eof
    }
    gzclose(gz);
  }
  for (u4t t = nThreads, i = 0; i < ceilLog2NThreads;) {
    fprintf(stderr, "Thread %u awaits barrier %u.\n", tid, i);
    pthread_barrier_wait(barriers + i++);
    u4t ceilN = (t + 1) / 2, floorN = t / 2;
    if (tid >= floorN) break;
    merge1(a, as + tid + ceilN);
    t = ceilN;
  }
  return 0;
}
int cmpOutEntries(const void *x, const void *y) {
  const OutEntry *a = x, *b = y;
  return strcmp(as[0].titles + a->titleOffset, as[0].titles + b->titleOffset);
}
int main(int argc, char **argv) {
  u4t nFiles = argc - 1,
      maxFilesPerThread = (nFiles + nThreads - 1) / nThreads;
  for (u4t t = 0; t < nThreads; t++) {
    as[t].entryN = 0;
    as[t].filenames = calloc(maxFilesPerThread, sizeof(char*));
  }
  for (u4t i = 0, pos = 0; i < nFiles; i++) {
    as[i % nThreads].filenames[pos] = argv[i + 1];
    if (nThreads - 1 == i % nThreads) pos++;
  }
  for (u4t i = 0, cnt = nThreads; i < ceilLog2NThreads; i++, cnt /= 2)
    eoz(!pthread_barrier_init(barriers + i, NULL, cnt));
  pthread_t threads[nThreads];
  for (u4t t = 0; t < nThreads; t++)
    eoz(!pthread_create(threads + t, NULL, workerFn, (void*)(intptr_t)t));
  for (u4t t = 0; t < nThreads; t++) eoz(!pthread_join(threads[t], NULL));
  fprintf(stderr, "Threads done.\n");
  OutEntry *out = aToOutEntries(as);
  fprintf(stderr, "OutEntries made.\n");
  radixSortDesc(out, as[0].entryN);
  fprintf(stderr, "Radix sort done.\n");
  EntryN start = 0;
  while (start < as[0].entryN) {
    EntryN end = start + 1;
    while (end < as[0].entryN && out[end].viewN == out[start].viewN) end++;
    if (end - start > 1)
      qsort(out + start, end - start, sizeof(OutEntry), cmpOutEntries);
    start = end;
  }
  fprintf(stderr, "Secondary sort done.\n");
  for (size_t i = 0; i < as[0].entryN; i++) 
    printf("%u %s\n", out[i].viewN, as[0].titles + out[i].titleOffset);
  fprintf(stderr, "Print done; everything done.\n");
}
