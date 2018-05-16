// We will find monoids. We use additive notation for the monoid operation,
// since 0 as identity is convenient with 0 indexed arrays.
//
// This program doesn't yet exclude isomorphic copies. The 11 addition tables
// that it finds for 3-element monoids only correspond to 7 distinct monoids,
// for example.

#include <stdio.h>
#include <stdbool.h>

// n is the number of monoid elements.
#define n 3
int f[n][n];

bool check_associative() {
  // check a + (b + c) = (a + b) + c for all a,b,c
  // this is automatically true when a = b = c
  // it is also automatically true if a or b or c is the identity element
  for (int a = 1; a < n; a++) {
    for (int b = 1; b < n; b++) {
      int fab = f[a][b];
      for (int c = 1; c < n; c++) {
        if (a == b && a == c) continue;
        int fbc = f[b][c];
        if (f[a][fbc] != f[fab][c]) {
          //printf("Not associative on: %d %d %d", a, b, c);
          return false;
        }
      }
    }
  }
  return true;
}

void clear_grid() {
  for (int y = 1; y < n; y++) {
    for (int x = 1; x < n; x++) {
      f[y][x] = 0;
    }
  }
}

bool increment_grid() {
  for (int y = n - 1; y >= 1; y--) {
    for (int x = n - 1; x >= 1; x--) {
      if (f[y][x] < n - 1) {
        f[y][x]++;
        return true;
      }
      f[y][x] = 0;
    }
  }
  return false;
}

int main() {
  for (int i = 0; i < n; i++) {
    f[0][i] = i;
    f[i][0] = i;
  }
  clear_grid();
  int count = 0;
  do {
    if (!check_associative()) {
      continue;
    }
    count++;
    //continue;
    for (int y = 0; y < n; y++) {
      for (int x = 0; x < n; x++) {
        printf("%d ", f[y][x]);
      }
      printf("\n");
    }
    printf("\n");
  } while (increment_grid());
  printf("Count: %d\n", count);
}
