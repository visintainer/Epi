library("gridExtra")

fillgap <- function(a,gap){
    ww <- which(a)
    for (i in 1:(length(ww) - 1)){
        if ((ww[i+1] - ww[i]) < gap)
            a[ww[i]:ww[i+1]] <- TRUE
    }
    return(a)
}

getevents <- function(dis){
    Symp1 <- Symp[dis,]
    id <- c(Symp1$user_id[1])
    da1 <- c(Symp1$date[1])
    da2 <- c()
    val <- 0
    for (d in 1:(dim(Symp1)[1] - 1)){
        if (!((Symp1$date[d+1] <= (Symp1$date[d] + gap + 1)) &&
            (Symp1$user_id[d] == (Symp1$user_id[d+1])))){
            da2 <- c(da2,Symp1$date[d])
            da1 <- c(da1,Symp1$date[d+1])
            id <- c(id,Symp1$user_id[d+1])
        }
    }
    da2 <- c(da2,Symp1$date[d+1])
    da2 <- as.Date(da2,origin=OrigD)
    events1 <- data.frame(id,da1,da2)
    return(events1)
}

getsymptoms <- function(events1,start.D,stop.D){
    MatResSynt <- matrix(0,80,(stop.D-start.D)+1)
    colnames(MatResSynt) <- as.character(as.Date(start.D:stop.D, origin=OrigD))
    for (i in 1:(dim(events1)[1])){
        MatResSynt[events1$id[i],as.character(as.Date(events1$da1[i]:events1$da2[i], origin=OrigD))] <- 1
    }
    return(MatResSynt)
}

## Returns all the connections of the Subjects who have an event
getconnections <- function(events,Proxy,start.D,stop.D,OrigD){
    Sog <- unique(events$id)
    MatResConn <- matrix(0,length(Sog),(stop.D-start.D)+1)
    colnames(MatResConn) <- as.character(as.Date(start.D:stop.D, origin=OrigD))
    rownames(MatResConn) <- as.character(Sog)
    for (i in colnames(MatResConn)){
        print(i)
        P <- Proxy[Proxy$date==i,c(1,2)]
        for (j in Sog){
            MatResConn[as.character(j),i] <- sum(P==j)
        }
    }
    return(MatResConn)
}

## Returns all the UNIQUE connections of the Subjects who have an event
getuniqueconnections <- function(events,Proxy,start.D,stop.D,OrigD){
    Sog <- unique(events$id)
    MatResConn <- matrix(NA,length(Sog),(stop.D-start.D)+1)
    colnames(MatResConn) <- as.character(as.Date(start.D:stop.D, origin=OrigD))
    rownames(MatResConn) <- as.character(Sog)
    for (i in colnames(MatResConn)){
        print(i)
        P <- Proxy[Proxy$date==i,c(1,2)]
        for (j in Sog){
            MatResConn[as.character(j),i] <- length(unique(unlist(P[(which(P[,1]==j | P[,2]==j)),])))
        }
    }
    return(MatResConn)
}

## Returns all the connections of EVERYBODY
getallconnections <- function(Proxy,start.D,stop.D,OrigD,Soggetti){
    print(start.D)
    print(stop.D)
    MatResConn <- matrix(0,length(Soggetti),(stop.D-start.D)+1)
    print(dim(MatResConn))
    colnames(MatResConn) <- as.character(as.Date(start.D:stop.D, origin=OrigD))
    rownames(MatResConn) <- as.character(Soggetti)
    for (i in colnames(MatResConn)){
        print(i)
        P <- Proxy[Proxy$date==i,c(1,2)]
        if(dim(P)[1]){
            for (j in Soggetti){
                MatResConn[as.character(j),i] <- sum(P==j)
            }
        }
    }
    return(MatResConn)
}

## Returns all the connections of EVERYBODY with the ID of contacts
getallIDconnections.D.S <- function(Proxy,start.D,stop.D,OrigD,Soggetti){
    DD <- c(as.character(as.Date(start.D:stop.D,origin=OrigD)))
    ListConn <- list()
    for (i in DD){
        P <- Proxy[Proxy$date==i,c(1,2)]
        if(dim(P)[1]){
            ListConn[[i]] <- list()
            for (j in Soggetti){
                li <- c(P[which(P[,2] == j),1],P[which(P[,1] == j),2])
                print(li)
                if(length(li) != 0){
                    ListConn[[i]][[as.character(j)]] <- li
                }
            }
        }
    }
    return(ListConn)
}

getallIDconnections.S.D <- function(Proxy,start.D,stop.D,OrigD,Soggetti){
    DD <- c(as.character(as.Date(start.D:stop.D,origin=OrigD)))
    ListConn <- list()
    for (j in Soggetti){
        print(j)
        ListConn[[as.character(j)]] <- list()
        P <- Proxy[which((Proxy[,2] == j) | (Proxy[,1] == j)),c(1,2,5)]
        ListConn[[as.character(j)]] <- list()
        for (i in DD){
            if(dim(P[P$date==i,])[1]){
                li <- c(P[P$date==i,c(1,2)][(P[P$date == i,c(1,2)]) != j])
                if(length(li) != 0){
                    ListConn[[as.character(j)]][[i]] <- li
                }
            }
        }
    }
    return(ListConn)
}

## Returns all the UNIQUE connections of EVERYBODY
getalluniqueconnections <- function(Proxy,start.D,stop.D,OrigD,Soggetti){
    MatResConn <- matrix(NA,length(Soggetti),(stop.D-start.D)+1)
    colnames(MatResConn) <- as.character(as.Date(start.D:stop.D, origin=OrigD))
    rownames(MatResConn) <- as.character(Soggetti)
    for (i in colnames(MatResConn)){
        print(i)
        P <- Proxy[Proxy$date==i,c(1,2)]
        for (j in Soggetti){
            MatResConn[as.character(j),i] <- length(unique(unlist(P[(which(P[,1]==j | P[,2]==j)),])))
        }
    }
    return(MatResConn)
}

plotsympt <- function(MatResSynt,start.D,stop.D,OrigD,main=MAIN){
    A <- xyplot(colSums(MatResSynt)~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main=main)   
    Q <- apply(MatResSynt,2,quantile,c(0.05,0.5,0.95))
    B <- xyplot(Q["5%",] + Q["50%",] + Q["95%",]~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Symptoms Mean", auto.key=TRUE)
    #M <- apply(MatResSynt,2,mean)
    #C <- xyplot(M~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Symptoms Mean", auto.key=TRUE)
    D <- xyplot((colSums(MatResSynt)/(dim(MatResSynt)[1]))~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Ave Symptoms Sum")
    #D2 <- xyplot((colSums(MatResSynt2)/(dim(MatResSynt2)[1]))~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Ave Uniquee Symptoms Sum")
    grid.arrange(A)
}

plotconn <- function(MatResConn,MatResConn2,start.D,stop.D,OrigD){
    A <- xyplot(colSums(MatResConn)~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Connections Sum")
    A2 <- xyplot(colSums(MatResConn2)~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Unique Connections Sum")
    #Q <- apply(MatResSynt,2,quantile,c(0.05,0.5,0.95))
    #B <- xyplot(Q["5%",] + Q["50%",] + Q["95%",]~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Connections Mean", auto.key=TRUE)
    #M <- apply(MatResSynt,2,mean)
    #C <- xyplot(M~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Connections Mean", auto.key=TRUE)
    D <- xyplot((colSums(MatResConn)/(dim(MatResConn)[1]))~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Ave Connections Sum")
    D2 <- xyplot((colSums(MatResConn2)/(dim(MatResConn2)[1]))~as.Date(c(start.D:stop.D), origin=OrigD), type="b", main="Ave Unique Connections Sum")
    grid.arrange(A,A2,D,D2,ncol=2)
}

borda <- function(M){
    el <- unique(M[1,])
    if (length(el) != length(M[1,])){
        print("MERDA")}
    posl <- c()
    for (e in 1:dim(M)[2]){
        pos <- 0
        for (l in 1:dim(M)[1]){
            pos <- (pos + which(M[l,] == e))
        }
        posl <- c(posl,(pos/(dim(M)[1])))
    }
    return(posl)
}

ldist <- function(M){
#    dd <- 0
#    for (i in 1:dim(M)[1]){
        dist(M, method="canberra", diag=FALSE)
}
