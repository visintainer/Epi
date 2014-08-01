#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "header.h"

/* extern int N; */
/* extern int NI; */
/* extern double ALPHAstart; */
/* extern double BETAstart; */
/* extern double DURATIONstart; */
/* extern double IMMUNEstart; */
/* extern int ITER; */
/* extern double sigmaALPHA; */
/* extern double sigmaBETA; */
/* extern double sigmaDURATION; */
/* extern double sigmaIMMUNE; */
/* extern double LAT; */
/* extern int firstPROXY; */
/* extern int lastPROXY; */
/* extern int firstILI; */
/* extern int * SI; */
/* extern double * Z; */
/* extern int * AS; */
/* extern double * ILI; */
/* extern int ***AdjL; */
/* extern double *** ADJL; */
/* extern int MODE; */

int get_param(char file[], char sep){
  int out_get_line=2;
  char * line;
  char * string_who;
  char * string_value;
  FILE *fp;
  string_who = (char *)calloc(100,sizeof(char));
  string_value = (char *)calloc(100,sizeof(char));
  if(!(fp = fopen(file,"r"))){
    printf("merda! in get param\n");
    return 1;
  }
  while(out_get_line>=2){
    out_get_line=get_line(&line,fp);
    if(out_get_line<3){
      switch(out_get_line){
      case 2:
	fprintf(stderr,"read_param: line of file %s does not end in newline\n",file);
	break;
      case 1:
	fprintf(stderr,"read_param: file %s contains an empty line\n",file);
	return 1;
	break;
      case 0:
	fclose(fp);
	return 0;
	break;
      case -1:
	fprintf(stderr,"read_param: get_line error on file %s\n",
		file);
	return 1;
      default:
	fprintf(stderr,"read_param: unrecognized exit status of get_line on file %s\n",file);
	return 1;
	break;
      }
    }
    sscanf(line,"%s", string_who);
    line = (char *)strchr(line, sep);
    line++;
    sscanf(line,"%s", string_value);
    if(strcmp(string_who,"N")==0)
      N = atoi(string_value);
    if(strcmp(string_who,"NI")==0)
      NI = atoi(string_value);
    if(strcmp(string_who,"ALPHAstart")==0)
      ALPHAstart = atof(string_value);
    if(strcmp(string_who,"BETAstart")==0)
      BETAstart = atof(string_value);
    if(strcmp(string_who,"DURATIONstart")==0)
      DURATIONstart = atof(string_value);
    if(strcmp(string_who,"IMMUNEstart")==0)
      IMMUNEstart = atof(string_value);
    if(strcmp(string_who,"iter")==0)
      ITER = atoi(string_value);
    if(strcmp(string_who,"sigmaALPHA")==0)
      sigmaALPHA = atof(string_value);
    if(strcmp(string_who,"sigmaBETA")==0)
      sigmaBETA = atof(string_value);
    if(strcmp(string_who,"sigmaDURATION")==0)
      sigmaDURATION = atof(string_value);
    if(strcmp(string_who,"sigmaIMMUNE")==0)
      sigmaIMMUNE = atof(string_value);
    if(strcmp(string_who,"lat")==0)
      LAT = atof(string_value);
    if(strcmp(string_who,"firstPROXY")==0)
      firstPROXY = atoi(string_value);
    if(strcmp(string_who,"lastPROXY")==0)
      lastPROXY = atoi(string_value);
    if(strcmp(string_who,"firstILI")==0){
      firstILI = atoi(string_value);
      /* int i=6; */
      /* fprintf(stderr,"in mcmc AS[%d]=%d\n",i,firstILI); */
    }
  }
 
  return 0;
}

int get_data(char file[], char sep){
  int i;
  double maxZ;
  int out_get_line=2;
  char * line;
  char * string_who;
  char * string_value;
  FILE *fp;
  maxZ=0;
  string_who = (char *)calloc(100,sizeof(char));
  string_value = (char *)calloc(100,sizeof(char));
  if(!(fp = fopen(file,"r"))){
    printf("merda! in get data\n");
    return 1;
  }
  while(out_get_line>=2){
    out_get_line=get_line(&line,fp);
    if(out_get_line<3){
      switch(out_get_line){
      case 2:
	fprintf(stderr,"read_param: line of file %s does not end in newline\n",file);
	break;
      case 1:
	fprintf(stderr,"read_param: file %s contains an empty line\n",file);
	return 1;
	break;
      case 0:
	fclose(fp);
	return 0;
	break;
      case -1:
	fprintf(stderr,"read_param: get_line error on file %s\n",
		file);
	return 1;
      default:
	fprintf(stderr,"read_param: unrecognized exit status of get_line on file %s\n",file);
	return 1;
	break;
      }
    }
    sscanf(line,"%s", string_who);
    line = (char *)strchr(line, sep);
    line++;
    if(strcmp(string_who,"SI")==0){
      for (i = 0; i < NI; i++){
    	sscanf(line,"%s", string_value);
    	SI[i] = atoi(string_value);
    	line = (char *)strchr(line, sep);
      	line++;
      }
    }
    if(strcmp(string_who,"Z")==0){
      for (i = 0; i < NI; i++){
    	sscanf(line,"%s", string_value);
    	Z[i] = atof(string_value);
    	line = (char *)strchr(line, sep);
      	line++;
	if(Z[i]>maxZ)
	  maxZ=Z[i];
      }
    }
    if(strcmp(string_who,"AS")==0){
      for (i = 0; i < N; i++){
    	sscanf(line,"%s", string_value);
    	AS[i] = atoi(string_value);
    	line = (char *)strchr(line, sep);
      	line++;
      }
    }
    if(strcmp(string_who,"ILIdaybyday")==0){
      for (i = 0; i <= (lastPROXY); i++){
	if (i >= firstILI){
	  sscanf(line,"%s", string_value);
	  ILI[i] = atof(string_value);
	  line = (char *)strchr(line, sep);
	  line++;
	}
      }
    }
  }
  return 0;
}

int get_proxy(char * proxypath,char sep){
  /* int i=8; */
  /* fprintf(stderr,"in mcmc AS[%d]=%d\n",i,23); */
  int ff;
  char fff[20];
  int midx = 0;
  fprintf(stderr,"MODE in sir %d\n",MODE);
  ADJL = (double ***)calloc((lastPROXY-firstPROXY+1), sizeof(double**));
  for (ff = firstPROXY; ff <= lastPROXY; ff++){
    /* fprintf(stderr,"proxy numero %d\n",ff); */
    char * file = (char *)calloc(1000,sizeof(char));
    file = strcat(file,proxypath);
    /* fprintf(stderr,"proxy file %s\n",file); */
    if((MODE == 2)||(MODE==3))
      file = strcat(file,"/Adj_");
    if((MODE == 0)||(MODE==1))
      file = strcat(file,"/Adj_");
    sprintf(fff,"%d",ff);
    //if((MODE == 2)||(MODE==3)){
    //sprintf(fff,"%d",0); //// PRENDOLASOMMATOTALE!!! 0
    //  sprintf(fff,"%s","0binary"); //// PRENDOLASOMMATOTALE BINARIZZATA!!! 0binary
    //}
    file = strcat(file,fff);
    /* fprintf(stderr,"proxy file %s\n",file); */
    ADJL[midx] = (double **) calloc ((N+1), sizeof(double*));
    read_matrix(ADJL[midx], file, sep);
    midx++;
    /* fprintf(stderr,"proxy dopo numero %d\n",midx); */
    free(file);
  }
  return 0;
}   

int read_matrix(double ** mat, char * file, char sep){
  int i;
  int lidx;
  int out_get_line=2;
  char * line;
  char * string_value;
  FILE *fp;
  string_value = (char *)calloc(100,sizeof(char));
  if(!(fp = fopen(file,"r"))){ 
    printf("merda! in read matrix\n");
    return 1;
  }
  lidx = 0;
  while(out_get_line>=2){
    out_get_line=get_line(&line,fp);
    if(out_get_line<3){
      switch(out_get_line){
      case 2:
	fprintf(stderr,"read_param: line of file %s does not end in newline\n",file);
	break;
      case 1:
	fprintf(stderr,"read_param: file %s contains an empty line\n",file);
	return 1;
	break;
      case 0:
	fclose(fp);
	free(string_value);
	return 0;
	break;
      case -1:
	fprintf(stderr,"read_param: get_line error on file %s\n",
		file);
	return 1;
      default:
	fprintf(stderr,"read_param: unrecognized exit status of get_line on file %s\n",file);
	return 1;
	break;
      }
    }
    // Load SINGLE MATRICES
    mat[lidx] = (double *) calloc(N, sizeof(double));
    /* fprintf(stderr,"inreadmatrix N DEVEESSERE 74CAZZO!!%d\n",N); */
    for (i = 0; i < N; i++){
      /* fprintf(stderr,"inreadmatrix %d\n",i); */
      sscanf(line,"%s", string_value);
      mat[lidx][i] = atof(string_value);
      line = (char *)strchr(line, sep);
      line++;
    }
    lidx++;
  }
  return 0;
}
