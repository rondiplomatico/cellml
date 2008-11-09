#include <iostream>
#include <wchar.h>
#include <string>
#include <string.h>
#include <stdlib.h>

#include "CellMLModelDefinition.hpp"

static char* getAbsoluteURI(const char* uri);

int main(int argc,char* argv[])
{
  if (argc < 2) 
  {
    std::cerr << "usage: " << argv[0] << " <URL>" << std::endl;
    return(-1);
  }
  char* inputURI = getAbsoluteURI(argv[1]);
  /*
   * Create the model defintion object
   */
  CellMLModelDefinition* def = new CellMLModelDefinition(inputURI);
  // we don't want to save the generated files (also the default behaviour)
  def->saveTempFiles(false);
  /*
   * Instantiate the model definition
   */
  if (def->instantiate() == 0)
  {
    std::cout << "Instaniated the model." << std::endl;

  }
  else
  {
    std::cerr << "Error instantiating the CellML model defintion" << std::endl;
  }
  delete def;
  free(inputURI);
  return(0);
}

static char* getAbsoluteURI(const char* uri)
{
  if (uri)
  {
    if (strstr(uri,"://") != NULL)
    {
      /*printf("URI (%s) already absolute.\n",uri);*/
      char* abs = (char*)malloc(strlen(uri)+1);
      strcpy(abs,uri);
      return(abs);
    }
    else if (uri[0]=='/')
    {
      /*printf("URI (%s) absolute path, making absolute URI: ",uri);*/
      char* abs = (char*)malloc(strlen(uri)+1+7);
      sprintf(abs,"file://%s",uri);
      /*printf("%s\n",abs);*/
      return(abs);
    }
    else
    {
      /* relative filename ? append absoulte path */
      /*printf("URI (%s) is relative path, making absolute URI: ",uri);*/
      int size = pathconf(".",_PC_PATH_MAX);
      char* cwd = (char*)malloc(size);
      getcwd(cwd,size);
      char* abs = (char*)malloc(strlen(cwd)+strlen(uri)+1+8);
      sprintf(abs,"file://%s/%s",cwd,uri);
      free(cwd);
      /*printf("%s\n",abs);*/
      return(abs);
    }
  }
  return((char*)NULL);
}
