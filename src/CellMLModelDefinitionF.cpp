#include <iostream>
#include <wchar.h>
#include <string>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "CellMLModelDefinitionF.h"
#include "CellMLModelDefinition.hpp"
#include "ccgs_required_functions.h"

void* create_cellml_model_definition_f(const char* uri,unsigned int length_arg)
{
  std::cout << "in C: uri = \"" << uri << "\"" << std::endl;
  std::cout << "in C: length_arg = \"" << length_arg << "\"" << std::endl;
  return(void*)NULL;
}
