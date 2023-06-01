#include <stdio.h>
#include <stdlib.h>

void printint(int i) {
  printf("%d", i);
}

void printbool(int b) {
  printf(b ? "True" : "False");
}

// our string value is a struct { length: int, values: int [] }
// for now ignore input
void printstring(char *arr) {
  printf("%s", arr);
}

