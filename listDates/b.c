#include <stdio.h>
#include <time.h>
int main() {
  struct tm d = {0};
  char c = 'e', buf[80];
  int n = 768;
  d.tm_year = 2029 - 1900; // years since 1900
  for (int i = 0; i < 1000; i++, n++) {
    d.tm_mday++;
    mktime(&d);
    strftime(buf, sizeof(buf), "%Y-%m-%d", &d);
    if (n > 999) {n = 0; c++;}
    printf("%s %c%.3i ", buf, c, n);
    strftime(buf, sizeof(buf), "%a", &d); puts(buf);
  }
}
