#include <stdio.h>

/* typedef struct{ */
/*   double time_symptom; */
/*   double time_infection; */
/*   double time_recovery; */
/*   int subj_idx; */
/*   double *lambda; */
/*   double *lambdaint; */
/* }Individual; */

typedef struct{
  double time_symptom;
  double time_infection;
  double time_recovery;
  double *lambda;
}Individual;

int sir(double Alpha, double Beta, char* rseed, int *newidx, double *newday);

gsl_rng *R_GLOBAL;
