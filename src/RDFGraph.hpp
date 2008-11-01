#ifndef _RDFGRAPH_H_
#define _RDFGRAPH_H_

#include <redland.h>

/**
 * A basic wrapper providing the RDF functionality required for openCMISS.
 */
class RDFGraph
{
 public:
  RDFGraph();
  ~RDFGraph();

  void print(FILE* file);
  /**
   * Build a RDF graph from a given source URL.
   * Create a RDF graph from the given source RDF/XML document. Expects a 
   * straight RDF/XML document only.
   * @param url The URL of the source document.
   * @return 0 on success, non-zero for error.
   */
  int buildFromURL(const char* url);

 private:
  librdf_world* world;
  librdf_storage* storage;
  librdf_model* rdfmodel;
  librdf_parser* parser;
  librdf_uri* uri;
};

#endif // _RDFGRAPH_H_
