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
   * Default constructor
   */
  CellMLModelDefinition();
  /**
   * TODO: Need to do a copy constructor
   */
  CellMLModelDefinition(const CellMLModelDefinition& src) {}

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

  /**
   * Set the compile command to use to compile the generated code into a dynamic shared object.
   * @command The compile command.
   */
  void compileCommand(const std::string& command)
  {
    mCompileCommand = command;
  }
  /**
   * Get the current compile command used to compile generated code.
   * @return The current compile command.
   */
  std::string compileCommand()
  {
    return mCompileCommand;
  }

  /**
   * Set the save temporary files flag.
   * @state Should be set to true to save generated temporary files.
   */
  void saveTempFiles(bool state)
  {
    mSaveTempFiles = state;
  }
  /**
   * Get the current state of the save temporary files flag.
   * @return The state of the save temporary files flag.
   */
  bool saveTempFiles()
  {
    return mSaveTempFiles;
  }

 private:
  class SimulationDescription* mSimulationDescription;
  std::string mTmpDirName;
  bool mTmpDirExists;
  std::string mCodeFileName;
  bool mCodeFileExists;
  std::string mDsoFileName;
  bool mDsoFileExists;
  std::string mCompileCommand;
  bool mSaveTempFiles;
  // loaded from the generated and compiled DSO
  void* mHandle;
  int (*mGetNbound)();
  int (*mGetNrates)();
  int (*mGetNalgebraic)();
  int (*mGetNconstants)();
  /* Initialise all variables which aren't state variables but have an
   * initial_value attribute, and any variables & rates which follow.
   */
  void (*mSetupFixedConstants)(double* CONSTANTS,double* RATES,double* STATES);
  /* Compute all rates which are not static
   */
  void (*mComputeRates)(double VOI,double* STATES,double* RATES,
    double* CONSTANTS,double* ALGEBRAIC);
  /* Compute all variables not computed by initConsts or rates
   *  (i.e., these are not required for the integration of the model and
   *   thus only need to be called for output or presentation or similar
   *   purposes)
   */
  void (*mEvaluateVariables)(double VOI,double* CONSTANTS,double* RATES,
    double* STATES,double* ALGEBRAIC);
};

#endif // _CELLMLMODELDEFINITION_H_
