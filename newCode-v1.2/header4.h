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
int read_matrix(double ** mat, char * file, int midx, char sep);

typedef struct{
  double time_symptom;
  double time_infection;
  double time_recovery;
  int subj_idx;
  double *lambda;
  double *lambdaint;
}Individual;

void  epi(Individual *individuals,int N,double LAT,double gamma);
void  lambda(Individual *individuals,int N,double beta,double alpha,int maxt);
double likelihood(Individual *individuals,int N,int maxt, double* pint);

