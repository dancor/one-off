#include "../../templates/cpp/err.h"
#include "../../templates/c/types.h"
int main(int argc, char **argv) {switch (argc) {
  case 3: switch (argv[1][0]) {
    case 'a': if (!argv[1][1]) {
      printf("TODO Alarm to add: %s.\n", argv[2]);
      exit(0);}
    case 'r': if (!argv[1][1]) {
      printf("TODO Alarm to remove: %s.\n", argv[2]);
      exit(0);}} break;
  case 2: if (argv[1][0] == 's') {printf("TODO Alarm serve.\n"); exit(0);}
    break;
  case 1: printf("TODO Alarm list:\n"); exit(0);} printf("Usage.\n");
  exit(-1);}
