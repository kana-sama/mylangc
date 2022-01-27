#include <stdio.h>


extern long int Lread() {
  long int result;

  printf("> ");
  fflush(stdout);
  scanf("%ld", &result);

  return result;
}

extern void Lwrite(long int n) {
  printf("%ld\n", n);
  fflush(stdout);
}