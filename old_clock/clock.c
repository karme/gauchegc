#include <gauche.h>
#include <time.h>

static ScmObj clock_subr(ScmObj *args, int nargs, void *data)
{
  int clk;

  clk = (int) clock();
  return SCM_MAKE_INT(clk);
}

static ScmObj clocks_per_sec_subr(ScmObj *args, int nargs, void *data)
{
  return SCM_MAKE_INT(CLOCKS_PER_SEC);
}

void Scm_Init_ggcclock(void)
{
    ScmModule *module = SCM_MODULE(SCM_FIND_MODULE("ggc.clock", TRUE));
    ScmObj subr;
    subr = Scm_MakeSubr(clock_subr, NULL, 0, 0, SCM_MAKE_STR("clock"));
    SCM_DEFINE(module, "clock", subr);
    subr = Scm_MakeSubr(clocks_per_sec_subr, NULL, 0, 0, SCM_MAKE_STR("clocks-per-sec"));
    SCM_DEFINE(module, "clocks-per-sec", subr);
}






