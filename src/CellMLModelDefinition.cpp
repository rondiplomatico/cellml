
#include <string>
#include <wchar.h>
#include <iostream>
#include <vector>

#include "CellMLModelDefinition.hpp"
#include "SimulationDescription.hpp"
#include "RDFGraph.hpp"

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
