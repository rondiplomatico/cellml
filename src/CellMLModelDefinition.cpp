
#include <string>
#include <wchar.h>
#include <iostream>

#include "CellMLModelDefinition.hpp"
#include "RDFGraph.hpp"

CellMLModelDefinition::CellMLModelDefinition(const char* url)
{
  std::cout << "Creating CellMLModelDefinition from the URL: " 
	    << url << std::endl;
  // first need to create a RDF graph of the data in the source document
  RDFGraph* rdfGraph = new RDFGraph();
  rdfGraph->buildFromURL(url);
  rdfGraph->print(stdout);
  delete rdfGraph;
}

