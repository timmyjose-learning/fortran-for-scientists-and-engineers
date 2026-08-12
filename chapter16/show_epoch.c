#include <stdio.h>
#include <time.h>

int main() {
  unsigned long long epoch = time(NULL);
  printf("%llu\n", epoch);

  return 0;
}