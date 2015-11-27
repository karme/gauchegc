#include <gauche.h>
#include <gauche/extend.h>

extern void Scm_Init_lapacklib(ScmModule *);

void Scm_Init_ggcnumlapack(void)
{
    ScmModule *mod;

    SCM_INIT_EXTENSION(ggcnumlapack);
    mod = SCM_MODULE(SCM_FIND_MODULE("ggc.numerical.lapack", TRUE));
    Scm_Init_lapacklib(mod);
}

/* 
   Dummy MAIN for libg2c.
   Should be fixed by linking options
*/

void MAIN__() {}
