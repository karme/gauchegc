#include <gauche.h>
#include <gauche/extend.h>

void Scm_Init_ggcnumextra()
{
  ScmModule *module = SCM_MODULE(SCM_FIND_MODULE("ggc.numerical.extra", TRUE));
  Scm_Init_ggc_numerical_extra(module);
}
