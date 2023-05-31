#include <stdio.h>
#include <stdlib.h>

void printint(int i) {
  printf("%d", i);
}

void printbool(int b) {
  printf(b ? "True" : "False");
}

// Otherwise, this function has no way of knowing how big the array is.
void print_array(char *arr, size_t size) {
    int i;
    for (i = 0; i < size; i++) {
        puts(&arr[i]);
    }
}

// our string value is a struct { length: int, values: int [] }
// for now ignore input
void printstring() {
  char test_string[5] = "horse";
  // Size must be passed!
  size_t size = sizeof(test_string) / sizeof(test_string[0]);
  print_array(test_string, size);
}


