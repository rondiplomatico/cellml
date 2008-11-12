#ifndef _CELLMLMODELDEFINITIONF_H_
#define _CELLMLMODELDEFINITIONF_H_

#ifdef __cplusplus
extern "C"
{
#endif

  /**
   * Create the CellMLModelDefinition from the provided URI.
   * @param uri The URI of the model definition.
   * @param length_arg The hidden Fortran string length parameter
   * @return A newly created cellml_model_definition_f object.
   */
  void* create_cellml_model_definition_f(const char* uri,
    unsigned int length_arg);

#ifdef __cplusplus
} /* extern C */
#endif

#endif // _CELLMLMODELDEFINITIONF_H_

