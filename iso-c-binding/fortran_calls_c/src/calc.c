#include <stdio.h>

void calc(float *a, float *b, float *c) {
  *c = *a + *b;
  printf("[C] %.3f+ %.3f = %.3f\n", *a, *b, *c);
}