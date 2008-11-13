#include <stdlib.h>
#include <stdio.h>

#include "testF2C.h"

int c_library_function(int exp1[100],float alloc[],int len_alloc)
{
  int x1 = 123;
  printf("C c_library_function: len_alloc = %d\n",len_alloc);
  printf("C x1 = %d\n",x1);
  return(x1);
}
