#include <stdio.h>
#include <stdlib.h>

void die(char *s) {
    fputs(s, stderr);
    fputs("\n", stderr);
    exit(EXIT_FAILURE);
}

int main() {
    unsigned char b[32 * 4096];
    size_t b_read;
    while (b_read = fread(b, 1, sizeof(b), stdin)) {

    }
    return EXIT_SUCCESS;
}
