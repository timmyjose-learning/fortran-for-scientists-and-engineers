#include <stdio.h>

typedef struct {
  int n;
  float data1;
  float data2;
} MyTypeT;

void c_sub(MyTypeT *obj, char c[]) {
  obj->data2 = obj->n * obj->data1;
  printf("String: %s\n", c);
}