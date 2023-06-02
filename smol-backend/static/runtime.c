#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

// yolo: https://stackoverflow.com/questions/8465006/how-do-i-concatenate-two-strings-in-c
char* stringconcat(const char *s1, const char *s2)
{
    const size_t len1 = strlen(s1);
    const size_t len2 = strlen(s2);
    char *result = malloc(len1 + len2 + 1); // +1 for the null-terminator
    // in real code you would check for errors in malloc here
    memcpy(result, s1, len1);
    memcpy(result + len1, s2, len2 + 1); // +1 to copy the null-terminator
    return result;
}
