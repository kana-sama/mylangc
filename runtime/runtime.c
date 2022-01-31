#include <stdio.h>


extern long int read() {
  long int result;

  printf("> ");
  fflush(stdout);
  scanf("%ld", &result);

  return result;
}

extern void write(long int n) {
  printf("%ld\n", n);
  fflush(stdout);
}