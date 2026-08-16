#include <stdio.h>

extern void my_sub(float *, float *, float *);

int main() {
  float a = 3.0, b = 6.0, c = 0.0;

  printf("Before calling Fortran, a = %.3f, b = %.3f, c = %.3f\n", a, b, c);
  my_sub(&a, &b, &c);
  printf("After calling Fortran, a = %.3f, b = %.3f, c = %.3f\n", a, b, c);

  return 0;
}
