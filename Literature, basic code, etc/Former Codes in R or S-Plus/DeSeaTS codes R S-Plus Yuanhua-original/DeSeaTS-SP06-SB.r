########## Non-rosust smooth of DI monthly data ##########
########## with two local dummy variables.        ##########
########## Programmer: Yuanhua Feng                 ##########
########## Date: July 2006                          ##########


############## The data  ##############
x<-read.table("Trust-DI-month.txt")
y<-x[,3]

s<-12
p<-1   
n<-length(y)           ## number of observations

tb<-95    ################ The position where the breaks occur
y[tb]<-(y[tb-2*s]+y[tb-s]+y[tb+s]+y[tb+2*s])/4   ################# The first correction in the data


kn<-2           
h<-trunc(2.5*s)


##################### The function calculation ##########################
#################### Please do not change this part #################
################## constants, vectors and matrices ##################
DeSeaTS.SB<-function(y, s, p, kn, h, tb)
{

n<-length(y)           ## number of observations
ye<-matrix(0,2,n)      ## definition of the matrix of estimates
ht<-2*h+1              ## total bandwith

t1<-tb
t2<-tb

xg<-matrix(0,p+s,ht)   ## data matrix for the trend
xs<-xg                 ## data matrix for the season
xw<-xg                 ## weighted data matrix


  for(j in (1:(p+1))){xg[j,1:ht]<-(((1:ht)-h-1)/(h+0.5))**(j-1)}
  for(j in (1:(trunc((s-1)/2)))){
	xs[p+2*j,  1:ht]<-cos(2*j*pi*(1:ht)/s)
	xs[p+2*j+1,1:ht]<-sin(2*j*pi*(1:ht)/s)
	}

  if(s/2==trunc(s/2)){
	xs[p+s,1:ht]<-cos(pi*(1:ht))
	}

xt<-rbind(xg[1:(p+1),], xs[(p+2):(p+s),])



hr<-c(2*h-(0:h)) #pointwise right bandwith

##############Smoothing for points not involving the change points
     for(i in (1:(t1-h-1))){
	     if(i<=h+1){
                   wk<-(1-(((1:ht)-i)/(hr[i]+0.5))**2)**(kn-1)

                   for(j in (1:(p+s))){xw[j,]<-xt[j,]*wk}



                   yw<-solve(xw%*%t(xt))%*%xw
       
                   xgt<-t(xg[,i])
                   xst<-t(xs[,i])

                   gw<-xgt%*%yw
                   ye[1,i]<-gw%*%cbind(y[1:ht])
                   sw<-xst%*%yw
                   ye[2,i]<-sw%*%cbind(y[1:ht])

                   ye[1,n-i+1]<-gw%*%cbind(y[n:(n-ht+1)])
                   ye[2,n-i+1]<-sw%*%cbind(y[n:(n-ht+1)])

                  }

	     if(i>h+1 && i<n-h){
                           ye[1,i]<-gw%*%cbind(y[(i-h):(i+h)])
                           ye[2,i]<-sw%*%cbind(y[(i-h):(i+h)])
                          }
        }

       for(i in ((t1+h):(t2-h-1))){
	                   ye[1,i]<-gw%*%cbind(y[(i-h):(i+h)])
                      ye[2,i]<-sw%*%cbind(y[(i-h):(i+h)])
                      }
      for(i in ((t2+h):(n-h-1))){
	                   ye[1,i]<-gw%*%cbind(y[(i-h):(i+h)])
                      ye[2,i]<-sw%*%cbind(y[(i-h):(i+h)])
                      }

                     xs<-rbind(xs[1,], xs)

############## Smoothing for points involving the first jump point
     for(i in ((t1-h):(t1+h-1))){
                     dc<-i-t1+h+1
                     dv<-xg[1,]
                     dv[1:(ht-dc)]<-0
                     
                     xg1<-rbind(xg[1:(p+1),], dv, xg[(p+2):(p+s),])
                     xt<-rbind(xg1[1:(p+2),], xs[(p+3):(p+s+1),])
                     xw<-xt

                   wk<-(1-(((1:ht)-h-1)/(hr[h+1]+0.5))**2)**(kn-1)

                   for(j in (1:(p+s+1))){xw[j,]<-xt[j,]*wk}



                   yw<-solve(xw%*%t(xt))%*%xw
       
                   xgt<-t(xg1[,h+1])
                   xst<-t(xs[,h+1])

                   gw<-xgt%*%yw
                   sw<-xst%*%yw

                   ye[1,i]<-gw%*%cbind(y[(i-h):(i+h)])
                   ye[2,i]<-sw%*%cbind(y[(i-h):(i+h)])

                 }


############## Smoothing for points involving the sendon point with jump in the first derivative
     for(i in ((t2-h):(t2+h-1))){
                     dc<-i-t2+h+1
                     dv<-xg[2,]
                     dv[1:(ht-dc)]<-0
                     
                     xg1<-rbind(xg[1:(p+1),], dv, xg[(p+2):(p+s),])
                     xt<-rbind(xg1[1:(p+2),], xs[(p+3):(p+s+1),])
                     xw<-xt

                   wk<-(1-(((1:ht)-h-1)/(hr[h+1]+0.5))**2)**(kn-1)

                   for(j in (1:(p+s+1))){xw[j,]<-xt[j,]*wk}



                   yw<-solve(xw%*%t(xt))%*%xw
       
                   xgt<-t(xg1[,h+1])
                   xst<-t(xs[,h+1])

                   gw<-xgt%*%yw
                   sw<-xst%*%yw

                   ye[1,i]<-gw%*%cbind(y[(i-h):(i+h)])
                   ye[2,i]<-sw%*%cbind(y[(i-h):(i+h)])

                 }


ge<-ye[1,]
se<-ye[2,]

drop(ye)
}

ye<-DeSeaTS.SB(y,s,p, kn, h, tb)
ge<-ye[1,]
se<-ye[2,]
t0<-1987
Residuen<-y-ge-se
Deseasonal<-y-se
Year<-((1:n)-1)/s+t0




#############################################################
#############################################################
        par(mfrow=c(2,1), mai=c(0.8, 0.9, 0.8, 0.9))

matplot(Year, cbind(y, ge), type="l", lty=c(1, 3), col=c(5,6))
title("The milk price time series and the fitted trend, corrected at two change-points", cex=0.6)
title(xlab="Observation Year")

matplot(Year, cbind(se, se*0), type="l", lty=c(1, 2), col=c(3,1))
title(" ... .... ", cex=0.6)
title(xlab="Observation Year")



