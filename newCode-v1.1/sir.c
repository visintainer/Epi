#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include "header.h"

extern int * epi;
extern double * lambda;

int lambdasir(int N, double Alpha, double Beta, double *lambda, int *epi){  
  int i,l;
  int contTOT[N],contI[N];
  for (i=0; i<N; i++){ 
    contTOT[i]=0;
    contI[i]=0;
    lambda[i]=0.;
    if (epi[i] == 0){  // Per tutti i suscettibili
      for (l=0; l<N; l++){ // Per ogni altro soggetto
	if (epi[l] == 1){  // Se l'altro soggetto e' infetto
	  contI[i] += SingAdj[i][l];  // conto quanti contatti hanno avuto
	}
	contTOT[i] += SingAdj[i][l];
      }
      if(contTOT[i] == 0){
	lambda[i] = (Alpha);
      }
      else{
	lambda[i] = (Alpha+(Beta*(double)contI[i])/(double)contTOT[i]);
      }
    }
  }
  return(1);
}

int sir(double Alpha, double Beta, char* rseed, int *newidx, double *newday){

  int N = 74;
  int i,l,nS,nI,nR;
  int idxsogg;
  int *idx = newidx;
  int count = 0;
  double t = 0;
  double newt;
  double infrate = 1.;
  double recrate = 0.;
  double rate = 1.;
  double tlat=3;
  double Gamma;
  double event,pesca;
  double lambdasum;
  double ii;
  double *date = newday;
  
  //  Beta = 0.;

  for (i=1; i<N; i++){
    epi[i] = 0;
    lambda[i] = 0.;
  }
  epi[0] = 1;
  t = 0-gsl_ran_flat(R_GLOBAL,0,tlat);//media 1.5

  date[0] = (t+tlat);
  idx[0] = 0;

  nS = N-1;
  nI = 1;
  nR = 0;
  
  Gamma = 0.3703704;
  
  lambdasir(N, Alpha, Beta, lambda, epi);
  lambdasum = 0;
  for (l=0;l<N;l++)
    lambdasum +=lambda[l];
  infrate = lambdasum;
  recrate = nI*Gamma;
  rate=(infrate+recrate);
  
  while((t<=200)&&(nS>0)){ // fino alla fine dei tempi
  //while((nI+nR)<=25){ // fino a quando si fanno 25 infetti
    newt=gsl_ran_exponential(R_GLOBAL,1./rate);  //estraggo il prossimo tempo
    t+=newt;
    event=gsl_ran_flat(R_GLOBAL,0,rate);  // estraggo che evento e'
    if(event < infrate){  // se e' un' infezione
      count ++;
      ///////////pesco un soggetto e lo metto 1 in epi
       lambdasum = 0;
       for (l=0;l<N;l++)
	 lambdasum += lambda[l];
       pesca = gsl_ran_flat (R_GLOBAL,0,lambdasum);
       ii = 0;
       idxsogg = -1;
       for(l = 0; l<N; l++){
	 ii += lambda[l];
	 if (ii >= pesca){
	   idxsogg = l;
	   l = N+1;
	 }
       }
       epi[idxsogg] = 1;
       nS-=1;
       nI+=1;

       date[count] = t+tlat;
       idx[count] = idxsogg;
    }
    else{// se e' una guarigione
      idxsogg = (int)gsl_ran_flat(R_GLOBAL,0,nI);
      i = 0;
      for(l=0;l<N;l++){
	if(epi[l]==1){
	  if(i == idxsogg){
	    epi[l] = 2;
	    l = N+1;
	  }
	  i++; 
	}
      }
      nI-=1;
      nR+=1;
    }
    lambdasir(N, Alpha, Beta, lambda, epi);
    lambdasum = 0;
    for (l=0;l<N;l++)
      lambdasum +=lambda[l];
    infrate = lambdasum;
    recrate = nI*Gamma; // times Gamma
    rate=(infrate+recrate);

    /* for (i=0;i<N;i++) */
    /*   fprintf(stderr,"%e\t",lambda[i]); */
    /* fprintf(stderr,"\n"); */
    /* for (i=0;i<N;i++) */
    /*   fprintf(stderr,"%d\t",epi[i]); */
    /* fprintf(stderr,"\n"); */
  }
  return count;
}
