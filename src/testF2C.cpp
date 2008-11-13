#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <string.h>
#include <iostream>

#include "testF2C.h"

int c_library_function(int exp1[100],float alloc[],int len_alloc)
{
  int x1 = 123;
  printf("C c_library_function: len_alloc = %d\n",len_alloc);
  printf("C x1 = %d\n",x1);
  alloc[0] = 1.0;
  alloc[1] = 2.0;
  alloc[2] = 3.0;
  alloc[3] = 4.0;
  alloc[len_alloc-1] = (float)len_alloc;
  return(x1);
}


class Bob
{
 public:
  Bob() { name = "fred"; }
  std::string name;
};

void* createBob()
{
  std::cout << "C Creating a new Bob object" << std::endl;
  Bob* bob = new Bob();
  std::cout << "C bob->name = \"" << bob->name.c_str() << "\"" << std::endl;
  std::cout << "C (void*)bob = " << (void*)bob << std::endl;
  return (void*)bob;
}

void bobPrintName(void* _ptr)
{
  std::cout << "C bobPrintName(" << _ptr << ")" << std::endl;
  Bob* bob = (Bob*)_ptr;
  std::cout << "C bob->name = \"" << bob->name.c_str() << "\"" << std::endl;
}
