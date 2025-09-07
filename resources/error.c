#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include "error.h"

void stopWithError(int64_t type) {
  switch (type) {
    case 1:
      printf("Expected an int with arithmetic.\n");
      break;
    case 2:
      printf("Expected a boolean, but no boolean was found :( \n");
      break;
    case 3:
      printf("Expected a tuple, but no tuple was found :( \n)");
      break;
    case 4:
      printf("Tuple index out of bounds \n");
    case 5:
      printf("Expected a closure but got something else :( \n");
    default:
      printf("Unknown error %"PRId64" occurred.\n", type);
      break;
  }
  exit(type);
}
