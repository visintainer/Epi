#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_vector.h>

int parser(int n,char *array[],char ***flag,char ***value,int *nflags);
char *get_value(char *flag[],char *value[],int nflags,char opt[]);
int get_line(char **line,FILE *fp);
int get_param(char file[], char sep);
int get_data(char file[], char sep);
void percent(int n,int d,int s, FILE *out);
int get_proxy(char proxypath[],char sep);
int read_matrix(double ** mat, char * file, char sep);
/* int get_single_proxy(char * proxypath,char sep); */

typedef struct{
  double time_symptom;
  double time_infection;
  double time_recovery;
  int subj_idx;
  double *lambda;
  double *lambdaint;
}Individual;

int sir(double Alpha, double Beta, char* rseed, int *newidx, double *newday);

gsl_rng *R_GLOBAL;

int *epi;
double *lambda;
int N;
int NI;
double ALPHAstart;
double BETAstart;
double DURATIONstart;
double IMMUNEstart;
int ITER;
double sigmaALPHA;
double sigmaBETA;
double sigmaDURATION;
double sigmaIMMUNE;
double LAT;
int firstPROXY;
int lastPROXY;
int firstILI;
int * SI;
double * Z;
int * AS;
double * ILI;
int ***AdjL;
double *** ADJL;
double ** SingAdj;
int MODE;
int MAXTIME;
