#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "uthash.h" // Include the uthash header
#define MAX_BITMAP_LEN 65
// Structure for our hash table items.
// The UT_hash_handle handles the internal hash table mechanics.
typedef struct {
  char bitmap[MAX_BITMAP_LEN]; // The key
  unsigned long first_codepoint; // The value
  UT_hash_handle hh; // Makes this structure hashable
} BitmapMap;
// A pointer to our hash table, initially NULL.
BitmapMap *bitmaps = NULL;
int main() {
  const char *filename = "u.hex";
  FILE *file = fopen(filename, "r");
  if (file == NULL) { perror("Error opening file"); return 1; }
  char line[256];
  int duplicates_found = 0;
  printf("Searching for duplicate bitmaps in %s using uthash...\n", filename);
  while (fgets(line, sizeof(line), file)) {
    unsigned long codepoint;
    char bitmap[MAX_BITMAP_LEN];
    if (sscanf(line, "%lx:%64s", &codepoint, bitmap) == 2) {
      BitmapMap *entry = NULL;
      // Look for the bitmap in the hash table.
      // HASH_FIND_STR is a macro provided by uthash.
      HASH_FIND_STR(bitmaps, bitmap, entry);
      if (entry != NULL) {
        // Duplicate found!
        duplicates_found++;
        printf("Duplicate found: U+%04lX and U+%04lX have the same bitmap.\n",
             entry->first_codepoint, codepoint);
      } else {
        // New bitmap, add it.
        entry = (BitmapMap *)malloc(sizeof(BitmapMap));
        if (entry == NULL) {
          fprintf(stderr, "Error: Memory allocation failed.\n");
          exit(EXIT_FAILURE);
        }
        strncpy(entry->bitmap, bitmap, MAX_BITMAP_LEN - 1);
        entry->bitmap[MAX_BITMAP_LEN - 1] = '\0';
        entry->first_codepoint = codepoint;
        // HASH_ADD_STR adds the new entry to the hash table.
        HASH_ADD_STR(bitmaps, bitmap, entry);
      }
    }
  }
  fclose(file);
  // Clean up the hash table memory.
  BitmapMap *current, *tmp;
  HASH_ITER(hh, bitmaps, current, tmp) {
    HASH_DEL(bitmaps, current);
    free(current);
  }
  if (duplicates_found == 0) printf("No duplicate bitmaps found.\n");
  else
    printf("\nSearch complete. Found %d sets of duplicate bitmaps.\n",
      duplicates_found);
}
