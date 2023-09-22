#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void Barnard(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void ScoreS(void *, void *, void *, void *, void *, void *, void *, void *);
extern void WaldS(void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"Barnard", (DL_FUNC) &Barnard, 10},
    {"ScoreS",  (DL_FUNC) &ScoreS,   8},
    {"WaldS",   (DL_FUNC) &WaldS,    8},
    {NULL, NULL, 0}
};

void R_init_Barnard(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
