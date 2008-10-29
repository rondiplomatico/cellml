#include <string>
#include <wchar.h>
#include <iostream>
#include "SimulationList.hpp"

SimulationList::SimulationList(const wchar_t* url)
{
  std::wstring URL(url);
  std::wcout << L"SimulationList to be constructed from the URL: " 
	     << URL.c_str() << std::endl;
}
