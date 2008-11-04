#ifndef _CELLMLMODELDEFINITION_H_
#define _CELLMLMODELDEFINITION_H_

/**
 * The primary object used to define a CellML model for use in openCMISS.
 *
 * This is the interface object sitting between a CellML description of a mathematical model and the use of that model in openCMISS.
 */
class CellMLModelDefinition
{
 public:
  /**
   * Construct a model definition from a given source document at the givien URL.
   * @param url The URL of the source document form which to create the model defintion.
   */
  CellMLModelDefinition(const char* url);
  /**
   * Destructor.
   */
  ~CellMLModelDefinition();

  /**
   * Instantiate the model definition into simulat-able code.
   * @return 0 if success; non-zero otherwise.
   */
  int instantiate();

 private:
  class SimulationDescription* mSimulationDescription;
};

#endif // _CELLMLMODELDEFINITION_H_
