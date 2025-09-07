#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include "printer.h"

int64_t bird_main() asm("bird_main");

//allocate heap for compiler
//int64_t * heap_ptr = malloc(sizeof(int64_t)*1000000);

int main(int argc, char** argv) {
  int64_t* heap_cursor = malloc(sizeof(int64_t)*1000000);
  int64_t result = bird_main(heap_cursor);
  printValue(result);
  free (heap_cursor);
  return 0;
}
