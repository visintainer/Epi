#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
//#include<malloc.h>
#include "header.h"
//#include "leak_detector_c.h"
#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))
#define ceil(X) (X-(int)(X) > 0 ? (int)(X+1) : (int)(X))

Individual *individuals;

double Alpha = 0.;
double Beta = 0.;

int N = 74;
int nI = 25;
double tlat=3;
char * rseed = "0";

gsl_rng *R_GLOBAL;

FILE *fpd;
FILE *fpi;
FILE *fout;
FILE *foutsim;
int merda;
int count;

void cumul(int nIsim, double *day_data,double *newday,int *a, int *b){
  int l,i,M;
  /* for (i=0;i<nI;i++){ */
  /*   fprintf(stderr,"data a %e\n",day_data[i]); */
  /* } */
  /* for (i=0;i<nIsim;i++){ */
  /*   fprintf(stderr,"new b %e\n",newday[i]); */
  /* } */
  /* for (i=0;i<nIsim;i++){ */
  /*   fprintf(stderr,"a prima %d\n",a[i]); */
  /* } */
  M = max(day_data[nI-1],newday[nIsim-1]);
  M=150;
  for (i=0; i<M; i++){
    for (l=0; l<nI; l++){
      if((day_data[l] < (i+1)))// && (day_data[l] > i-1))
	a[i] ++;
    }
    for (l=0; l<nIsim; l++){
      if((newday[l] < (i+1)))// && (day_data[l] > i-1))
	b[i] ++;
    }
  }
  
  /* for (i=0;i<nIsim;i++){ */
  /*   fprintf(stderr,"a %d\n",a[i]); */
  /* } */
  /* for (i=0;i<107;i++){ */
  /*   fprintf(stderr,"a[%d] %d\n",i,a[i]); */
  /* } */
}
long double likelihood(int nIsim, double *day_data,double *newday){
  int i,steps;
  long double L = 1.;
  steps = max(day_data[nI-1],newday[nIsim-1]);
  steps=150;
  //fprintf(stderr,"steps %d\n",steps);
  int a[steps];
  int b[steps];
  for (i=0;i<steps;i++){
    a[i] = 0;
    b[i] = 0;
  }
  cumul(nIsim, day_data, newday, a, b);
  for (i=0; i<steps; i++){
    L = L * gsl_ran_poisson_pdf(a[i], b[i]);
    //fprintf(stderr,"poisson %e %d %d\n",gsl_ran_poisson_pdf(a[i], b[i]),a[i],b[i]);
  }
  return L;
}


int main(){

  int maxt,i,idx;
  double day;
  int REP = 100000;
  int rep = 0;
  double alphastart = -2.;//-2.8;//-2.05906;//-2.6;//0.002; ///0.001563487;///0.002; /// 0.001563487;
  double betastart = -1.;//-0.09691001;//-0.5228787;//-0.2218487;//-0.58787;///0.3; /// 0.2873568;
  //double alphastart= 0.001563487;///0.002; /// 0.001563487;
  //double betastart = 0.2873568;///0.3; /// 0.2873568;
  double newalpha, newbeta; 
  double penalita = 0.1;
  long double accept;
  long double newlkh;
  long double tmplkh;
  long double lkh[REP];
  double Alpha[REP];
  double Beta[REP];
  double newday[N];
  double acceptedday[N];
  double acceptedAlpha;
  double acceptedBeta;
  long double acceptedlkh;
  int newidx[N];
  //int idx_data[nI];
  double day_data[nI];
  double deltaalpha = 0.001;
  double deltabeta = 0.001;
  int nIsim;//,acceptednIsim;

  char *adjfile = "/home/roberto/Work/OLD-Epi/R-data-preparation/data-ready-ORDIN/Adj_0_74x74";
  char *sep = "\t";

  epi = (int *)calloc(N,sizeof(int));
  lambda = (double *)calloc(N,sizeof(double));
  
  SingAdj = (double **)calloc(N,sizeof(double*));
  read_matrix(SingAdj,adjfile, *sep);
  /* int j; */
  /* for(i=0;i<N;i++){ */
  /*   for (j=0;j<N-1;j++) */
  /*     fprintf(stdout,"%d\t",(int) SingAdj[i][j]); */
  /*   fprintf(stdout,"%d\n",(int) SingAdj[i][N-1]); */
  /* } */
  /* exit(1); */
 
  //atexit(report_mem_leak);

  setenv("GSL_RNG_SEED", rseed , 1);
  gsl_rng_env_setup();
  R_GLOBAL = gsl_rng_alloc (gsl_rng_default);

  individuals=(Individual*)calloc(N,sizeof(Individual));
  for(i=0;i<N;i++){ //azzero individui
    individuals[i].time_symptom=-1;
    individuals[i].time_infection=-1;
    individuals[i].time_recovery=-1;
  }
  maxt=0;
  epi = (int *)calloc(N,sizeof(int));  
  fpd=fopen("ORIG-date.dat","r");
  fpi=fopen("ORIG-idx.dat","r");
  
  for(i=0;i<nI;i++){ // leggo dati originali 
    merda = fscanf(fpi,"%d",&idx);
    merda = fscanf(fpd,"%lf",&day);
    if (day > maxt) // inizializzo individui
      maxt = day;
    individuals[idx].time_symptom = day;
    day_data[i] = day;
    //idx_data[i] = idx;
  }
  newalpha = alphastart;
  newbeta = betastart;
  Alpha[rep] = alphastart;
  Beta[rep] = betastart;
  
  for(i=0;i<N;i++)  
    newday[i]=0;
  nIsim = sir(powf(10.,Alpha[rep]), powf(10.,Beta[rep]), rseed, newidx, newday);
  //  acceptednIsim=nIsim;
  for(i=0;i<N;i++)  
    acceptedday[i]=0;
  for(i=0;i<nIsim;i++)  
    acceptedday[i] = newday[i];
  for (i=0; i<nIsim; i++)
    fprintf(stderr,"newday %e\n",newday[i]);
  
  newlkh = likelihood(nIsim, day_data, newday); //Prima Likelihood

  count = 0;
  while (newlkh == 0){ //Prima Likelihood fin che e' == a 0
    //aggiorno a e b
    /* newalpha = Alpha[rep]*exp(deltaalpha*gsl_ran_gaussian(R_GLOBAL,1.)); */
    /* newbeta = Beta[rep]*exp(deltabeta*gsl_ran_gaussian(R_GLOBAL,1.)); */
    newalpha = Alpha[rep]+(deltaalpha*gsl_ran_gaussian(R_GLOBAL,1.));
    newbeta = Beta[rep]+(deltabeta*gsl_ran_gaussian(R_GLOBAL,1.));
    for(i=0;i<N;i++)  
      newday[i]=0;
    nIsim = sir(powf(10.,newalpha), powf(10.,newbeta), rseed, newidx, newday);
    newlkh = likelihood(nIsim, day_data, newday); 
    count ++;
  }
  Alpha[rep] = newalpha;
  Beta[rep] = newbeta;
  lkh[rep] = newlkh;
  acceptedlkh = newlkh;
  acceptedAlpha = newalpha;
  acceptedBeta = newbeta;
  fprintf(stderr,"Prima LKH= %Le\n",lkh[rep]);
  count = 0;
  //////////////////////////////
  for (rep=1; rep<REP; rep++){ //REPLICHE
    //aggiorno a e b
    // Calcolo LKH
    tmplkh = 0.;
    //   fprintf(stderr,"newL %Le \t oldL %Le\n",tmplkh,lkh[rep-1]);
    while(tmplkh ==0){ // fin che non parte l'epidemia
      /* newalpha = Alpha[rep-1]*exp(deltaalpha*gsl_ran_gaussian(R_GLOBAL,1.)); */
      /* newbeta = Beta[rep-1]*exp(deltabeta*gsl_ran_gaussian(R_GLOBAL,1.)); */
      newalpha = Alpha[rep-1]+(deltaalpha*gsl_ran_gaussian(R_GLOBAL,1.));
      newbeta = Beta[rep-1]+(deltabeta*gsl_ran_gaussian(R_GLOBAL,1.));
      // Chiamo sir
      for(i=0;i<N;i++)  
	newday[i]=0;
      nIsim = sir(powf(10.,newalpha), powf(10.,newbeta), rseed, newidx, newday);
      tmplkh = likelihood(nIsim, day_data, newday); //Likelihood
    }
    accept = (tmplkh/lkh[rep-1]);
    
    // Se nuova lkh > della vecchia o la vecchia e' 0 o pesco male
    if((accept >= 1) || (lkh[rep-1] == 0) || (gsl_ran_flat(R_GLOBAL,0,1)<accept)){ 
      for(i=0;i<N;i++)// cancello e riscrivo accday
	acceptedday[i]=0;
      for(i=0;i<nIsim;i++)  
	acceptedday[i] = newday[i];
      acceptedlkh = tmplkh;
      acceptedAlpha = newalpha;
      acceptedBeta = newbeta;
      newlkh = 0;
      while(newlkh ==0){ // fin che non parte l'epidemia
	// Ricalcolo sir
	for(i=0;i<N;i++)  
	  newday[i]=0;
	nIsim = sir(powf(10.,newalpha), powf(10.,newbeta), rseed, newidx, newday);
	// ricalcolo LKH
	newlkh = likelihood(nIsim, day_data, newday); //Likelihood
      }
      lkh[rep] = max(newlkh,(tmplkh*penalita)); 
      if(lkh[rep]<1e-300){
	if(tmplkh>1e-300){
	  lkh[rep] = 1e-300;
	}
      	if(tmplkh<1e-300){
	  lkh[rep] = tmplkh;
	}
      }
      count ++;
      /* lkh[rep] = tmplkh; */
      Alpha[rep] = newalpha;
      Beta[rep] = newbeta;
      //      fprintf(stderr,"era meglio ");
      fout = fopen("mcmc-out","a");
      fprintf(fout, "%d\t%Le\t%e\t%e\t%e\n",rep,acceptedlkh,log(acceptedlkh),acceptedAlpha,acceptedBeta);
      fclose(fout);
      foutsim = fopen("mcmc-sim-out","a");
      for (i=0;i<N;i++)
	fprintf(foutsim, "%e\t",acceptedday[i]);
      fprintf(foutsim, "\n");
      fclose(foutsim);
      fprintf(stderr, "Accept %d ",count);
    }
    else{
      tmplkh = lkh[rep-1];
      newlkh = 0;
      while(newlkh ==0){ // fin che non parte l'epidemia
	// Ricalcolo sir
	for(i=0;i<N;i++)  
	  newday[i]=0;
	nIsim = sir(powf(10.,Alpha[rep-1]), powf(10.,Beta[rep-1]), rseed, newidx, newday);
	// ricalcolo LKH
	newlkh = likelihood(nIsim, day_data, newday); //Likelihood
      }
      lkh[rep] = max(newlkh,(tmplkh*penalita)); 
      if(lkh[rep]<1e-120){
	if(tmplkh>1e-120){
	  lkh[rep] = 1e-120;
	}
      	if(tmplkh<1e-120){
	  lkh[rep] = tmplkh;
	}
      }
      Alpha[rep] = Alpha[rep-1];
      Beta[rep] = Beta[rep-1];
      //fprintf(stderr,"era peggio ");
      fout = fopen("mcmc-out","a");
      fprintf(fout, "%d\t%Le\t%e\t%e\t%e\n",rep,acceptedlkh,log(acceptedlkh),acceptedAlpha,acceptedBeta);
      fclose(fout);
      ///DEVOSCRIVERE QUELLI ACCETTATI PER ULTIMI
      foutsim = fopen("mcmc-sim-out","a");
      for (i=0;i<N;i++)
	fprintf(foutsim, "%e\t",acceptedday[i]);
      fprintf(foutsim, "\n");
      fclose(foutsim);
    }
  }
  return 1;
}


