#include <stdlib.h>
#include <string>
#include <string.h>
#include <wchar.h>
#include <iostream>
#include <vector>

#include <IfaceCellML_APISPEC.hxx>
#include <IfaceCCGS.hxx>
#include <CeVASBootstrap.hpp>
#include <MaLaESBootstrap.hpp>
#include <CCGSBootstrap.hpp>
#include <CellMLBootstrap.hpp>

#include "CellMLModelDefinition.hpp"
#include "SimulationDescription.hpp"
#include "RDFGraph.hpp"
#include "utils.hxx"

/*
 * Prototype local methods
 */
static char* getURIFromURIWithFragmentID(const char* uri);
//static char* wstring2string(const wchar_t* str);
static wchar_t* string2wstring(const char* str);
static std::wstring getModelAsCCode(iface::cellml_api::Model* model);

CellMLModelDefinition::CellMLModelDefinition(const char* url)
{
  std::cout << "Creating CellMLModelDefinition from the URL: " 
	    << url << std::endl;
  // first need to create a RDF graph of the data in the source document
  RDFGraph* rdfGraph = new RDFGraph();
  rdfGraph->buildFromURL(url);
  //rdfGraph->print(stdout);
  // can we find a valid simulation in the model defintion graph?
  mSimulationDescription = rdfGraph->createSimulationDescription();
  delete rdfGraph;
  if (mSimulationDescription && mSimulationDescription->isValid())
  {
    std::cout << "Have a valid simulation description." << std::endl;
    std::cout << "  CellML model URI: " << mSimulationDescription->modelURI() 
	      << std::endl;
  }
}

CellMLModelDefinition::~CellMLModelDefinition()
{
  if (mSimulationDescription) delete mSimulationDescription;
}

int CellMLModelDefinition::instantiate()
{
  int code = -1;
  if (!mSimulationDescription->isValid())
  {
    std::cerr << "CellMLModelDefinition::instantiate -- "
	      << "invalid simulation description." << std::endl;
    return -1;
  }
  RETURN_INTO_STRING(url,
    getURIFromURIWithFragmentID(mSimulationDescription->modelURI()));
  RETURN_INTO_WSTRING(URL,string2wstring(url.c_str()));
  RETURN_INTO_OBJREF(cb,iface::cellml_api::CellMLBootstrap,
    CreateCellMLBootstrap());
  RETURN_INTO_OBJREF(ml,iface::cellml_api::ModelLoader,cb->modelLoader());
  iface::cellml_api::Model* model = (iface::cellml_api::Model*)NULL;
  try
  {
    model = ml->loadFromURL(URL.c_str());
  }
  catch (...)
  {
    std::wcerr << L"Error loading model URL: " << URL.c_str() << std::endl;
    return -2;
  }
  std::wstring codeString = getModelAsCCode(model);
  if (codeString.length() > 1)
  {
    printf("Got the model as C-code:\n#####\n%S\n#####\n",codeString.c_str());
    code = 0;
  }
  else
  {
    std::wcerr << L"Error getting C-code for model URL: " << URL.c_str()
	       << std::endl;
    code = -3;
  }
  model->release_ref();
  return 0;
}

/*
 * Local methods
 */
// Bit of a hack, but will do the job until the full URI functionality in the current trunk CellML API makes it into a release - or could look at using the xmlURIPtr that comes with libxml2...
static char* getURIFromURIWithFragmentID(const char* uri)
{
  char* u = (char*)NULL;
  if (uri)
  {
    const char* hash = strchr(uri,'#');
    if (hash)
    {
      int l = strlen(uri) - strlen(hash);
      u = (char*)malloc(l+1);
      strncpy(u,uri,l);
      u[l] = '\0';
    }
    else
    {
      /* assume no fragment and return copy of original string */
      u = (char*)malloc(strlen(uri)+1);
      strcpy(u,uri);
    }
  }
  return(u);
}

/*char* wstring2string(const wchar_t* str)
{
  if (str)
  {
    size_t len = wcsrtombs(NULL,&str,0,NULL);
    if (len > 0)
    {
      len++;
      char* s = (char*)malloc(len);
      wcsrtombs(s,&str,len,NULL);
      return(s);
    }
  }
  return((char*)NULL);
}
*/

wchar_t* string2wstring(const char* str)
{
  if (str)
  {
    wchar_t* s;
    size_t l = strlen(str);
    s = (wchar_t*)malloc(sizeof(wchar_t)*(l+1));
    memset(s,0,(l+1)*sizeof(wchar_t));
    mbsrtowcs(s,&str,l,NULL);
    return(s);
  }
  return((wchar_t*)NULL);
}

static std::wstring getModelAsCCode(iface::cellml_api::Model* model)
{
  std::wstring code;
  RETURN_INTO_OBJREF(cgb,iface::cellml_services::CodeGeneratorBootstrap,
    CreateCodeGeneratorBootstrap());
  RETURN_INTO_OBJREF(cg,iface::cellml_services::CodeGenerator,
    cgb->createCodeGenerator());
  /* The trunk MaLaES has been updated since the 1.5 release, so define a
   * "custom" MaLaES here
   */
  RETURN_INTO_OBJREF(mbs,iface::cellml_services::MaLaESBootstrap,
    CreateMaLaESBootstrap());
  RETURN_INTO_OBJREF(mt,iface::cellml_services::MaLaESTransform,
    mbs->compileTransformer(
      L"opengroup: (\r\n"
      L"closegroup: )\r\n"
      L"abs: #prec[H]fabs(#expr1)\r\n"
      L"and: #prec[20]#exprs[&&]\r\n"
      L"arccos: #prec[H]acos(#expr1)\r\n"
      L"arccosh: #prec[H]acosh(#expr1)\r\n"
      L"arccot: #prec[1000(900)]atan(1.0/#expr1)\r\n"
      L"arccoth: #prec[1000(900)]atanh(1.0/#expr1)\r\n"
      L"arccsc: #prec[1000(900)]asin(1/#expr1)\r\n"
      L"arccsch: #prec[1000(900)]asinh(1/#expr1)\r\n"
      L"arcsec: #prec[1000(900)]acos(1/#expr1)\r\n"
      L"arcsech: #prec[1000(900)]acosh(1/#expr1)\r\n"
      L"arcsin: #prec[H]asin(#expr1)\r\n"
      L"arcsinh: #prec[H]asinh(#expr1)\r\n"
      L"arctan: #prec[H]atan(#expr1)\r\n"
      L"arctanh: #prec[H]atanh(#expr1)\r\n"
      L"ceiling: #prec[H]ceil(#expr1)\r\n"
      L"cos: #prec[H]cos(#expr1)\r\n"
      L"cosh: #prec[H]cosh(#expr1)\r\n"
      L"cot: #prec[900(0)]1.0/tan(#expr1)\r\n"
      L"coth: #prec[900(0)]1.0/tanh(#expr1)\r\n"
      L"csc: #prec[900(0)]1.0/sin(#expr1)\r\n"
      L"csch: #prec[900(0)]1.0/sinh(#expr1)\r\n"
      L"diff: #lookupDiffVariable\r\n"
      L"divide: #prec[900]#expr1/#expr2\r\n"
      L"eq: #prec[30]#exprs[==]\r\n"
      L"exp: #prec[H]exp(#expr1)\r\n"
      L"factorial: #prec[H]factorial(#expr1)\r\n"
      L"factorof: #prec[30(900)]#expr1 % #expr2 == 0\r\n"
      L"floor: #prec[H]floor(#expr1)\r\n"
      L"gcd: #prec[H]gcd_multi(#count, #exprs[, ])\r\n"
      L"geq: #prec[30]#exprs[>=]\r\n"
      L"gt: #prec[30]#exprs[>]\r\n"
      L"implies: #prec[10(950)] !#expr1 || #expr2\r\n"
      L"int: #prec[H]defint(func#unique1, BOUND, CONSTANTS, RATES, VARIABLES, "
      L"#bvarIndex, pret)#supplement double func#unique1(double* BOUND, "
      L"double* CONSTANTS, double* RATES, double* VARIABLES, int* pret) { return #expr1; }\r\n"
      L"lcm: #prec[H]lcm_multi(#count, #exprs[, ])\r\n"
      L"leq: #prec[30]#exprs[<=]\r\n"
      L"ln: #prec[H]log(#expr1)\r\n"
      L"log: #prec[H]arbitrary_log(#expr1, #logbase)\r\n"
      L"lt: #prec[30]#exprs[<]\r\n"
      L"max: #prec[H]multi_max(#count, #exprs[, ])\r\n"
      L"min: #prec[H]multi_min(#count, #exprs[, ])\r\n"
      L"minus: #prec[500]#expr1 - #expr2\r\n"
      L"neq: #prec[30]#expr1 != #expr2\r\n"
      L"not: #prec[950]!#expr1\r\n"
      L"or: #prec[10]#exprs[||]\r\n"
      L"plus: #prec[500]#exprs[+]\r\n"
      L"power: #prec[H]pow(#expr1, #expr2)\r\n"
      L"quotient: #prec[1000(0)] (double)(((int)#expr2) == 0 ? #expr1 / 0.0 : (int)(#expr1) / (int)(#expr2))\r\n"
      L"rem: #prec[1000(0)] (double)(((int)#expr2) == 0 ? (#expr1) / 0.0 : (int)(#expr1) % (int)(#expr2))\r\n"
      L"root: #prec[1000(900)] pow(#expr1, 1.0 / #degree)\r\n"
      L"sec: #prec[900(0)]1.0 / cos(#expr1)\r\n"
      L"sech: #prec[900(0)]1.0 / cosh(#expr1)\r\n"
      L"sin: #prec[H] sin(#expr1)\r\n"
      L"sinh: #prec[H] sinh(#expr1)\r\n"
      L"tan: #prec[H] tan(#expr1)\r\n"
      L"tanh: #prec[H] tanh(#expr1)\r\n"
      L"times: #prec[900] #exprs[*]\r\n"
      L"unary_minus: #prec[950]- #expr1\r\n"
      L"units_conversion: #prec[500(900)]#expr1*#expr2 + #expr3\r\n"
      L"units_conversion_factor: #prec[900]#expr1*#expr2\r\n"
      L"units_conversion_offset: #prec[500]#expr1+#expr2\r\n"
      L"xor: #prec[25(30)] (#expr1 != 0) ^ (#expr2 != 0)\r\n"
      L"piecewise_first_case: #prec[1000(5)](#expr1 ? #expr2 : \r\n"
      L"piecewise_extra_case: #prec[1000(5)]#expr1 ? #expr2 : \r\n"
      L"piecewise_otherwise: #prec[1000(5)]#expr1)\r\n"
      L"piecewise_no_otherwise: #prec[1000(5)]0.0/0.0)\r\n"
      L"eulergamma: #prec[999]0.577215664901533\r\n"
      L"exponentiale: #prec[999]2.71828182845905\r\n"
      L"false: #prec[999]0.0\r\n"
      L"infinity: #prec[900]1.0/0.0\r\n"
      L"notanumber: #prec[999]0.0/0.0\r\n"
      L"pi: #prec[999] 3.14159265358979\r\n"
      L"true: #prec[999]1.0\r\n"));
  cg->transform(mt);
  try
  {
    RETURN_INTO_OBJREF(cci,iface::cellml_services::CodeInformation,
      cg->generateCode(model));
    wchar_t* m = cci->errorMessage();
    if (!wcscmp(m,L""))
    {
      std::cout << "whoo hoo!" << std::endl;
      code = L"Fred";
    }
    else
    {
      std::wcerr << "Error generating code: " << m << std::endl;
    }
    free(m);
  }
  catch (...)
  {
    std::wcerr << L"Error generating the code information for model"
	       << std::endl;
  }
  return code;
}
