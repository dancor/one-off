#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>
// 10ffFF highest in unicode standard, but FffFF highest in unifont
#define MAX_CODEPOINT 0x10ffFF
void printRange(uint32_t start, uint32_t end, bool is_defined) {
  printf("%u..%u %c %i\n", start, end, is_defined ? 'y' : 'n', end + 1 - start);
}
int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <unifont.hex>\n", argv[0]); return 1;}
  const char *filename = argv[1];
  FILE *fp = fopen(filename, "r");
  if (!fp) {perror("Error opening file"); return 1;}
  printf("Parsing file: %s\n", filename);
  bool is_defined[MAX_CODEPOINT + 1]; // 1_048_576 bytes
  memset(is_defined, 0, sizeof(is_defined));
  char *line = NULL;
  size_t len = 0;
  ssize_t read;
  uint32_t max_defined_cp = 0;
  while ((read = getline(&line, &len, fp)) != -1) {
    char *colon = strchr(line, ':');
    if (!colon) {
      fprintf(stderr, "Warning: Malformed line (no colon): %s", line);
      return 1;
    }
    *colon = '\0';
    char *endptr;
    unsigned long cp = strtoul(line, &endptr, 16);
    if (endptr == line || *endptr != '\0' || cp > MAX_CODEPOINT) {
      fprintf(stderr, "Warning: Invalid codepoint on line: %s", line);
      return 1;
    }
    is_defined[(uint32_t)cp] = true;
    if ((uint32_t)cp > max_defined_cp) max_defined_cp = (uint32_t)cp;
  }
  if (line) free(line);
  fclose(fp);
  printf("Highest defined codepoint: U+%04X\n", max_defined_cp);
  if (max_defined_cp == 0 && !is_defined[0]) {
    printf("No codepoints were defined in the file.\n"); return 0;
  }
  uint32_t range_start = 0;
  bool current_status = is_defined[0];
  for (uint32_t i = 1; i <= max_defined_cp; i++) 
    if (is_defined[i] != current_status) {
      printRange(range_start, i - 1, current_status);
      range_start = i;
      current_status = is_defined[i];
    }
  printRange(range_start, max_defined_cp, current_status);
}
