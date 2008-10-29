#include <iostream>
#include <wchar.h>
#include <string>
#include <string.h>

#include "SimulationList.hpp"

int main(int argc,char* argv[])
{
  if (argc < 2) 
  {
    std::cerr << "usage: " << argv[0] << " <URL>" << std::endl;
    return(-1);
  }
  wchar_t* URL;
  size_t l = strlen(argv[1]);
  URL = new wchar_t[l + 1];
  memset(URL, 0, (l + 1) * sizeof(wchar_t));
  const char* mbrurl = argv[1];
  mbsrtowcs(URL, &mbrurl, l, NULL);
  /*
   * Search the given URL for defined simulations.
   */
  SimulationList* simulations = new SimulationList(URL);
  delete simulations;
  return(0);
}
