for (i in 1:10)
    dev.off()
data <- read.table("mcmc-out")
## N L LL A B
day.sim <- read.table("mcmc-sim-out")
day.data <- t(read.table("ORIG-date-fake.dat"))[1,]
REP <- dim(data)[1]
REP2=100
s0=(dim(data)[1]-REP2)+1
sf=dim(data)[1]
day.sim=day.sim[s0:sf,]
#data=data[s0:sf,]
M=100;
a=c(1:M)*0
for (i in 1:M)
    for (l in 1:length(day.data))
        if(day.data[l] < i)
            a[i]=a[i]+1
b=matrix(nrow=REP2,ncol=M,0)
for(iexp in 1:REP2){
    simulato <- as.numeric(day.sim[iexp,])
    idxx <- which(simulato>0)
    simulato <- simulato[idxx]
    for (i in 1:M)
        for (l in 1:length(simulato)){
            if(simulato[l] < i)
                b[iexp,i]=b[iexp,i]+1
        }
}
sample <- seq(1,REP,1)
cut <- 0
par(mfrow=c(3, 2))
ML <- mean(data[((REP*cut):REP),3])
MA <- mean(data[((REP*cut):REP),4])
MB <- mean(data[((REP*cut):REP),5])
boxplot(b[((REP2*cut):REP2),], axis=1,col="blue",main="Simulations",ylim=c(0,30))
lines(a,type="l",lwd=2,col="red")
lines(b[REP2-1,],type="l",lwd=2,col="orange")
lines(b[1,],type="l",lwd=2,col="darkgreen")
REP <- dim(data)[1]
plot(data[sample,3],type="l",main=paste("Log Lkh:",ML))
plot(10**data[sample,4],type="l",main=paste("Alpha:",MA))
abline(h=0.001563487,col="green4",lwd=2);
plot(10**data[sample,5],type="l",main=paste("Beta:",MB))
abline(h=0.6,col="green4",lwd=2);
plot(data[sample,4],type="l",main=paste("Alpha:",MA))
abline(h=log10(0.001563487),col="green4",lwd=2);
plot(data[sample,5],type="l",main=paste("Beta:",MB))
abline(h=log10(0.6),col="green4",lwd=2);
## plot(day.data,type="l",lwd=2,col="red",main="Simulations")
## for (i in sample)
##     lines(as.integer(day.sim[i,]),type="l",lwd=1,col="black")
## boxplot(day.sim, axis=1,col="blue",main="Simulations",ylim=c(0,110))
## lines(day.data,type="l",lwd=2,col="red")

x11()
plot(b[7,],ylim=c(0,100),col=0)
for(i in 1:10)
 lines(b[i,])
x11()
plot(b[99,])
