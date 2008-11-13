#ifndef _TESTF2C_H_
#define _TESTF2C_H_

#ifdef __cplusplus
extern "C"
{
#endif

  int c_library_function(int exp1[100],float alloc[],int len_alloc);
  void* createBob();
  void bobPrintName(void* _ptr);

#ifdef __cplusplus
} /* extern C */
#endif

#endif // _TESTF2C_H_

