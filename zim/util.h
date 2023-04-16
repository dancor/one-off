#pragma once
#include "types.h"
inline int strEq(const char *a, const char *b) {return 0 == strcmp(a, b);}
inline int strEqN(const char *a, const char *b, const size_t n) {
  return 0 == strncmp(a, b, n);}
inline int strDiff(const char *a, const char *b) {return 0 != strcmp(a, b);}
inline int strDiffN(const char *a, const char *b, const size_t n) {
  return 0 != strncmp(a, b, n);}
inline u8 startsW(const char *a, const char *b) { // a startsW b
  return strEqN(a, b, strlen(b));}
inline char* escStr(const char *a) { // do \ -> \\ too..
  char *b0 = (char*)malloc(3 * strlen(a) + 1), *b = b0;
nextChar:
  if ((u8)*a > 31) {*b++ = *a++; goto nextChar;}
  if (*a) {*b = '\\'; switch (*a) {
    case  7: b[1] = 'a'; b += 2; break; case  8: b[1] = 'b'; b += 2; break;
    case  9: b[1] = 't'; b += 2; break; case 10: b[1] = 'n'; b += 2; break;
    case 11: b[1] = 'v'; b += 2; break; case 12: b[1] = 'f'; b += 2; break;
    case 13: b[1] = 'r'; b += 2; break;
    case 14: b[1] = '1'; b[2] = '4'; b += 3; break;
    case 15: b[1] = '1'; b[2] = '5'; b += 3; break;
    case 16: b[1] = '1'; b[2] = '6'; b += 3; break;
    case 17: b[1] = '1'; b[2] = '7'; b += 3; break;
    case 18: b[1] = '1'; b[2] = '8'; b += 3; break;
    case 19: b[1] = '1'; b[2] = '9'; b += 3; break;
    case 20: b[1] = '2'; b[2] = '0'; b += 3; break;
    case 21: b[1] = '2'; b[2] = '1'; b += 3; break;
    case 22: b[1] = '2'; b[2] = '2'; b += 3; break;
    case 23: b[1] = '2'; b[2] = '3'; b += 3; break;
    case 24: b[1] = '2'; b[2] = '4'; b += 3; break;
    case 25: b[1] = '2'; b[2] = '5'; b += 3; break;
    case 26: b[1] = '2'; b[2] = '6'; b += 3; break;
    case 27: b[1] = '2'; b[2] = '7'; b += 3; break;
    case 28: b[1] = '2'; b[2] = '8'; b += 3; break;
    case 29: b[1] = '2'; b[2] = '9'; b += 3; break;
    case 30: b[1] = '3'; b[2] = '0'; b += 3; break;
    case 31: b[1] = '3'; b[2] = '1'; b += 3; break;
    default: b[2] = *a + '0'; b += 2;} a++; goto nextChar;}
  *b = 0; return b0;}
