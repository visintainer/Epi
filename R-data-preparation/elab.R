library(lattice)     
library(nettools)
library(igraph)
source("functions.R")

## LOAD DATA AND GENERATE DATES DFROM TIME
## Proxy <- read.csv("Proximity.csv", header=TRUE, as.is=TRUE)
## Proxy$date <- as.Date(Proxy$time)
## Symp <- read.csv("FluSymptoms.csv", header=TRUE, as.is=TRUE)
## Symp$date <- as.Date(Symp$time)

## save(Symp,Proxy,file='1-data.RData')
load('1-data.RData')

OrigD <- "1970-01-01"
gap <- 7

Soggetti <- unique(Proxy$user.id)

## Either Sore Throat Or Runny Nose
dis1 <- Symp[,3] | Symp[,4]
## Fever
dis2 <- as.logical(Symp[,5])
## sore.throat and nausea.vomiting.diarrhea
## or
## sore.throat and fever
dis3 <- (Symp[,3] & Symp[,6]) | (Symp[,3] & Symp[,5])

dis1 <- fillgap(dis1,gap)
dis2 <- fillgap(dis2,gap)
dis3 <- fillgap(dis3,gap)

events1 <- getevents(dis1)
events2 <- getevents(dis2)
events3 <- getevents(dis3)

## Get Dates
start1.D <- min(events1$da1)
stop1.D <- max(events1$da2)
start2.D <- min(events2$da1)
stop2.D <- max(events2$da2)
start3.D <- min(events3$da1)
stop3.D <- max(events3$da2)

symptoms1 <- getsymptoms(events1,start1.D,stop1.D)
symptoms1 <- symptoms1[unique(events1$id),]
symptoms2 <- getsymptoms(events2,start2.D,stop2.D)
symptoms2 <- symptoms2[unique(events2$id),]
symptoms3 <- getsymptoms(events3,start3.D,stop3.D)
symptoms3 <- symptoms3[unique(events3$id),]

plotsympt(symptoms1,start1.D,stop1.D,OrigD, main="Symptoms1")
plotsympt(symptoms2,start2.D,stop2.D,OrigD, main="Symptoms2")
plotsympt(symptoms3,start3.D,stop3.D,OrigD, main="Symptoms3")

## save.image("1bis-data.RData")
load("1bis-data.RData")

## Get Conections during events
connections1 <- getconnections(events1,Proxy,start1.D,stop1.D,OrigD)
connections2 <- getconnections(events2,Proxy,start2.D,stop2.D,OrigD)
connections3 <- getconnections(events3,Proxy,start3.D,stop3.D,OrigD)
## save(connections1,connections2,connections3,file='2-data.RData')
load('2-data.RData')

## Get unique connections during events
uniqueconnections1 <- getuniqueconnections(events1,Proxy,start1.D,stop1.D,OrigD)
uniqueconnections2 <- getuniqueconnections(events2,Proxy,start2.D,stop2.D,OrigD)
uniqueconnections3 <- getuniqueconnections(events3,Proxy,start3.D,stop3.D,OrigD)
## save(uniqueconnections1,uniqueconnections2,uniqueconnections3,file='3-data.RData')
load('3-data.RData')

## Get Conections
connectionsALL <- getallconnections(Proxy,start1.D,stop1.D,OrigD,Soggetti)

## Get unique connections
uniqueconnectionsALL <- getalluniqueconnections(Proxy,start1.D,stop1.D,OrigD,Soggetti)
## save(connectionsALL,uniqueconnectionsALL,file='4-data.RData')
load('4-data.RData')

## Generate AdjList
AdjL <- list()
for (d in names(IDconnectionsALL.D.S)){
    AdjL[[d]] <- matrix(0, length(Soggetti), length(Soggetti))
    colnames(AdjL[[d]]) <- Soggetti
    row.names(AdjL[[d]]) <- Soggetti
    for (s in names(IDconnectionsALL.D.S[[d]])){
        T <- table(IDconnectionsALL.D.S[[d]][[s]])
        AdjL[[d]][s,names(T)] <- T
    }
}

## save(AdjL,ddM,IDconnectionsALL.D.S,IDconnectionsALL.S.D,file='5-data.RData')
load('5-data.RData')

#############################
load('1-data.RData')
load('1bis-data.RData')
load('2-data.RData')
load('3-data.RData')
load('4-data.RData')
load('5-data.RData')
#############################

for (n in 1:length(IDconnectionsALL.D.S)){
    write.table(AdjL[[n]],
                file=paste("data-ready/Adj_",as.numeric(as.Date(names(IDconnectionsALL.D.S)[n])), sep=""),
                quote = FALSE,
                sep="\t",
                row.names=FALSE,
                col.names=FALSE)
}

names(AdjL) <-as.numeric(as.Date(names(AdjL)))

######## GENERATE CUMULATIVE ADJ FOR ndays
######## AND WITH FIRST ROW AND COLUMN == 0
ndays <- 7
d1 <- dim(AdjL[[1]])[1]
d2 <- dim(AdjL[[1]])[2]
if (d1!=d2){print("matricinonquadrateCAZZO!")}
for (n in 1:length(IDconnectionsALL.D.S)){
    toprint <- matrix(0,nrow=d1,ncol=d2)
    print(n)
    if (n<=(ndays)){
        print("in")
        for (m in 1:n){
            toprint <- toprint + AdjL[[m]]
        }
    }else{
        print("out")
        for (m in (n-ndays+1):n){
            toprint <- toprint + AdjL[[m]]
        }
    }
    toprint <- rbind(rep(0,d1),toprint)
    toprint <- cbind(rep(0,(d1+1)),toprint)
    diag(toprint) <- 0
    write.table(toprint,file=paste("data-ready-ORIG/Adj_",as.numeric(as.Date(names(IDconnectionsALL.D.S)[n])), sep=""),
                quote = FALSE,
                sep="\t",
                row.names=FALSE,
                col.names=FALSE)
}

######## GENERATE CUMULATIVE ADJ FOR ndays
### BUT JUST 1 - 0: PRESENCE - ABSENCE OF LINK
######## AND WITH FIRST ROW AND COLUMN == 0
ndays <- 7
d1 <- dim(AdjL[[1]])[1]
d2 <- dim(AdjL[[1]])[2]
if (d1!=d2){print("matricinonquadrateCAZZO!")}
for (n in 1:length(IDconnectionsALL.D.S)){
    toprint <- matrix(0,nrow=d1,ncol=d2)
    print(n)
    if (n<=(ndays)){
        print("in")
        for (m in 1:n){
            toprint <- toprint + AdjL[[m]]
        }
    }else{
        print("out")
        for (m in (n-ndays+1):n){
            toprint <- toprint + AdjL[[m]]
        }
    }
    toprint <- rbind(rep(0,d1),toprint)
    toprint <- cbind(rep(0,(d1+1)),toprint)
    toprint[which(toprint!=0)] <- 1
    diag(toprint) <- 0
    write.table(toprint,file=paste("data-ready-ORIG/Adj_binary_",as.numeric(as.Date(names(IDconnectionsALL.D.S)[n])), sep=""),
                quote = FALSE,
                sep="\t",
                row.names=FALSE,
                col.names=FALSE)
}


######## GENERATE CUMULATIVE ADJ FOR ALL DAYS
######## AND WITH FIRST ROW AND COLUMN == 0
allM <- matrix(0,nrow=d1,ncol=d2) 
for(i in 1:length(AdjL)){
    allM <- allM+AdjL[[i]]}
allM <- rbind(rep(0,d1),allM)
allM <- cbind(rep(0,(d1+1)),allM)
write.table(allM,file="data-ready-ORIG/Adj_0",quote = FALSE,sep="\t",row.names=FALSE,col.names=FALSE)

ck <- 0
a <- 3
b <- 10
for (i in 0:3)
    ck <- ck+(AdjL[[4-i]])[a,b]

ck

######## GENERATE CUMULATIVE ADJ FOR ALL DAYS
### BUT JUST 1 - 0: PRESENCE - ABSENCE OF LINK
######## AND WITH FIRST ROW AND COLUMN == 0
allM <- matrix(0,nrow=d1,ncol=d2) 
for(i in 1:length(AdjL)){
    allM <- allM+AdjL[[i]]}
allM[which(allM!=0)] <- 1
allM <- rbind(rep(0,d1),allM)
allM <- cbind(rep(0,(d1+1)),allM)
write.table(allM,file="data-ready-ORIG/Adj_0binary",quote = FALSE,sep="\t",row.names=FALSE,col.names=FALSE)

######## GENERATE TEST ALL1

tst <- matrix(rep(1,5625),75,75)

for (n in 1:length(IDconnectionsALL.D.S)){
    write.table(tst,
                file=paste("data-test-all1/Adj_",as.numeric(as.Date(names(IDconnectionsALL.D.S)[n])), sep=""),
                quote = FALSE,
                sep="\t",
                row.names=FALSE,
                col.names=FALSE)
}



######## GENERATE CUMULATIVE ADJ FOR ndays WITH ORDERED INDX!!!
######## AND WITH FIRST ROW AND COLUMN == 0
##sss <- sort(as.numeric(colnames(AdjL[[1]])), index.return=TRUE)
ordidx <- sort(as.numeric(colnames(AdjL[[1]])))
ordidx <- as.character(ordidx)
ndays <- 7
ccc <- c()
AdjL7 <- list()
d1 <- dim(AdjL[[1]])[1]
d2 <- dim(AdjL[[1]])[2]
if (d1!=d2){print("matricinonquadrateCAZZO!")}
toprint <- matrix(0,nrow=d1,ncol=d2)
for (n in 1:length(IDconnectionsALL.D.S)){
    toprint <- matrix(0,nrow=d1,ncol=d2)
    print(n)
    if (n<=(ndays)){
        print("in")
        for (m in 1:n){
            toprint <- toprint + AdjL[[m]]
        }       
    }else{
        print("out")
        for (m in (n-ndays+1):n){
            toprint <- toprint + AdjL[[m]]
        }
    }
    toprint <- toprint[ordidx,]
    toprint <- toprint[,ordidx]
    diag(toprint) <- 0
    ccc <- c(ccc,toprint[1,2])
    toprint <- rbind(rep(0,d1),toprint)
    toprint <- cbind(rep(0,(d1+1)),toprint)
    AdjL7[[n]] <- toprint
    write.table(toprint,file=paste("data-ready-ORDIN/Adj_",as.numeric(as.Date(names(IDconnectionsALL.D.S)[n])), sep=""),
                quote = FALSE,
                sep="\t",
                row.names=FALSE,
                col.names=FALSE)
}
names(AdjL7) <- names(AdjL)

######## GENERATE CUMULATIVE ADJ FOR ndays WITH ORDERED INDX!!!
######## AND WITH FIRST ROW AND COLUMN == 0
##sss <- sort(as.numeric(colnames(AdjL[[1]])), index.return=TRUE)
ordidx <- sort(as.numeric(colnames(AdjL[[1]])))
ordidx <- as.character(ordidx)
ndays <- 7
ccc <- c()
AdjL7 <- list()
d1 <- dim(AdjL[[1]])[1]
d2 <- dim(AdjL[[1]])[2]
if (d1!=d2){print("matricinonquadrateCAZZO!")}
toprint <- matrix(0,nrow=d1,ncol=d2)
for (n in 1:length(IDconnectionsALL.D.S)){
    toprint <- matrix(0,nrow=d1,ncol=d2)
    print(n)
    if (n<=(ndays)){
        print("in")
        for (m in 1:n){
            toprint <- toprint + AdjL[[m]]
        }       
    }else{
        print("out")
        for (m in (n-ndays+1):n){
            toprint <- toprint + AdjL[[m]]
        }
    }
    toprint <- toprint[ordidx,]
    toprint <- toprint[,ordidx]
    toprint[which(toprint!=0)] <- 1
    diag(toprint) <- 0
    ccc <- c(ccc,toprint[1,2])
    toprint <- rbind(rep(0,d1),toprint)
    toprint <- cbind(rep(0,(d1+1)),toprint)
    AdjL7[[n]] <- toprint
    write.table(toprint,file=paste("data-ready-ORDIN/Adj_binary_",as.numeric(as.Date(names(IDconnectionsALL.D.S)[n])), sep=""),
                quote = FALSE,
                sep="\t",
                row.names=FALSE,
                col.names=FALSE)
}
names(AdjL7) <- names(AdjL)


###### TEST

AAAU <- read.table("data-ready-ORDIN/Adj_14338")
AAA <- read.table("data-ready/Adj_14338")

all(AAAU[sss$ix,sss$ix] == AAA)


######## GENERATE CUMULATIVE ADJ FOR ALL DAYS WITH ORDERED INDX!!!
######## AND WITH FIRST ROW AND COLUMN == 0
d1 <- dim(AdjL[[1]])[1]
d2 <- dim(AdjL[[1]])[2]
toprint <- matrix(0,nrow=d1,ncol=d2)
allM <- matrix(0,nrow=d1,ncol=d2) 
for(i in 1:length(AdjL)){
    allM <- allM+AdjL[[i]]}

allM <- allM[ordidx,]
allM <- allM[,ordidx]
diag(allM) <- 0
toprint <- allM
toprint <- rbind(rep(0,d1),toprint)
toprint <- cbind(rep(0,(d1+1)),toprint)
write.table(toprint,file="data-ready-ORDIN/Adj_0",quote = FALSE,sep="\t",row.names=FALSE,col.names=FALSE)



######## GENERATE CUMULATIVE ADJ FOR ALL DAYS
### BUT JUST 1 - 0: PRESENCE - ABSENCE OF LINK
######## AND WITH FIRST ROW AND COLUMN == 0
allM <- matrix(0,nrow=d1,ncol=d2) 
for(i in 1:length(AdjL)){
    allM <- allM+AdjL[[i]]}
allM[which(allM!=0)] <- 1
allM <- rbind(rep(0,d1),allM)
allM <- cbind(rep(0,(d1+1)),allM)
write.table(allM,file="data-ready-ORDIN/Adj_0binary",quote = FALSE,sep="\t",row.names=FALSE,col.names=FALSE)


############ TEST
colnames(AdjL[[1]])

ccc <- c()
for(i in 1:length(AdjL)){ccc <- c(ccc,AdjL[[i]][2,31])}
sum(ccc)


#########

Za <- events3[-c(3,6,10),-3]
Z <- array(Za[,2])
Zadacopiare <- cbind(Za[,1],as.numeric(Za[,2]))
Zadacopiare <- Zadacopiare[order(Zadacopiare[,2]),]
Infect <- Zadacopiare[,1]
Z <- Zadacopiare[,2]
AS <- as.numeric(colnames(allM))
SI <- Zadacopiare[,1]
## SS <- AS[!(AS%in%SI)]
## SI <- SS <- NA
