#include <iostream>
#include <wchar.h>
#include <string>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "CellMLModelDefinition.hpp"
#include "ccgs_required_functions.h"

#define REAL_FORMAT "%0.8le"

static char* getAbsoluteURI(const char* uri);

int main(int argc,char* argv[])
{
  if (argc < 3) 
  {
    std::cerr << "usage: " << argv[0] << " <URL> <output>" << std::endl;
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
    double* constants = (double*)calloc(def->nConstants,sizeof(double));
    double* rates = (double*)calloc(def->nRates,sizeof(double));
    double* states = (double*)calloc(def->nRates,sizeof(double));
    double* algebraic = (double*)calloc(def->nAlgebraic,sizeof(double));
    // simple Euler integration for testing
    double dt = 0.01;
    double tabT = 0.5;
    double tStart = 0.0;
    double tEnd = 6.283185307179586232;
    // initialise first
    def->SetupFixedConstants(constants,rates,states);
    def->ComputeRates(tStart,states,rates,constants,algebraic);
    def->EvaluateVariables(tStart,constants,rates,states,algebraic);
    double time = tStart;
    uint32_t i;
    FILE* output = fopen(argv[2],"w");
    fprintf(output,REAL_FORMAT,time);
    for (i=0;i<def->nRates;i++) fprintf(output,"\t"REAL_FORMAT,states[i]);
    for (i=0;i<def->nAlgebraic;i++) fprintf(output,"\t"REAL_FORMAT,
      algebraic[i]);
    fprintf(output,"\n");
    while (time < tEnd)
    {
      double te = time + tabT;
      while (1)
      {
	// integrate from time to te
	def->ComputeRates(time,states,rates,constants,algebraic);
	for (i=0;i<def->nRates;i++) states[i] += rates[i] * dt;
	if (fabs(te-time) < 1.0e-10) break;
	time += dt;
	if (time > te) time = te;
      }
      def->EvaluateVariables(time,constants,rates,states,algebraic);
      fprintf(output,REAL_FORMAT,time);
      for (i=0;i<def->nRates;i++) fprintf(output,"\t"REAL_FORMAT,states[i]);
      for (i=0;i<def->nAlgebraic;i++) fprintf(output,"\t"REAL_FORMAT,
	algebraic[i]);
      fprintf(output,"\n");
      time = te;
    }
    fclose(output);
    free(constants);
    free(rates);
    free(states);
    free(algebraic);
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
