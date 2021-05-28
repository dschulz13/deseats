########## An S-PLUS function for decomposing seasonal  ##########
########## time series by LWR with a fixed bandwidth h. ##########

####  I.  Name of the function: DeSeats(y,s,p,kn,h)           ####
#### II.  Programmer: Yuanhua Feng                            ####
#### III. Date: Januar 02, 2002                               ####
#### IV. References:  1. Heiler and Feng (1996)               ####
####                  2. Heiler and Feng (2000)               ####  
####                  3. Feng (1999)                          ####
####                  4. Feng and Heiler (2000)               ####


################# Begin of the function ################                                     ####
DeSeaTS<-function(y,s,p,kn,h)
{
################### The inputs   #######################
## y - the input seasonal time series data
## s - the period
## p - 1 or 3, degree of polynomial for estimating the trend function.
## kn- 1, 2, 3 or 4, number of the kernel used as weighting function
         ## kn=1: Uniform Kernel       kn=2: Epanecknikov Kernel
         ## kn=3: Bisquare Kernel      kn=4: Triweight Kernel
## h - the bandwidth
         ## Tip 1: h should be smaller than n/2
         ## Tip 2: Try a bandwidth between 1.5*s and 6*s
 
################### The basic ideas   #######################
#### The model: yi = gi + Si +ei                                 ####
#### The approach used here is LWR (locally weighted regression) ####
#### LWR is a linear smoother. The main tesk is to calculate the ####
#### weighting systems for estimating g and S at each point t.   ####
#### In the following only the weighting systems at the left     ####
#### boundary (t=1 to h) and that at t=h+1 will be computed.     ####
#### The weighting system at a point h+1=<t<=n-h is the same.    ####
#### The weighting system at t > n-hh is symmetric to that at the point j = n-i+1.  #### 


################### constants, vectors and matrices #######################
n<-length(y)           ## number of observations
if(s==1){ye<-matrix(0,1,n)}   ## definition of the matrix of estimates
if(s>1){ye<-matrix(0,2,n)}   ## definition of the matrix of estimates
ht<-2*h+1              ## total bandwith

xg<-matrix(0,p+1,ht)  ## data matrix for the trend
if(s>1){xs<-matrix(0,s-1,ht)}  ## data matrix for the season


for(j in (1:(p+1))){xg[j,1:ht]<-(((1:ht)-h-1)/h)**(j-1)}
	xt<-xg

if(s>1){
  for(j in (1:(trunc((s-1)/2)))){
	xs[2*j-1,1:ht]<-cos(2*j*pi*(1:ht)/s)
	xs[2*j,  1:ht]<-sin(2*j*pi*(1:ht)/s)
	}

  if(s/2==trunc(s/2)){
	xs[s-1,1:ht]<-cos(pi*(1:ht))
	}
   xt<-rbind(xg, xs)
}

xw<-xt                 #weighted data matrix



hr<-c(2*h-(0:h)) #pointwise right bandwith of t

     for(i in (1:(n-h-1))){
	     if(i<=h+1){
                   wk<-(1-(((1:ht)-i)/(hr[i]+0.5))**2)**(kn-1)

                   for(j in (1:(p+s))){xw[j,]<-xt[j,]*wk}



                   yw<-solve(xw%*%t(xt))%*%xw
       
                 if(s==1){
                   xgt<-xg[,i]
                   gw<-xgt%*%yw
                   ye[1,i]<-gw%*%cbind(y[1:ht])
                   ye[1,n-i+1]<-gw%*%cbind(y[n:(n-ht+1)])
                    }

                 if(s>1){
                   xgt<-cbind(t(xg[,i]), t((1:(s-1))*0))
                   xst<-cbind(t((1:(p+1))*0), t(xs[,i]))

                   gw<-xgt%*%yw
                   ye[1,i]<-gw%*%cbind(y[1:ht])
                   sw<-xst%*%yw
                   ye[2,i]<-sw%*%cbind(y[1:ht])

                   ye[1,n-i+1]<-gw%*%cbind(y[n:(n-ht+1)])
                   ye[2,n-i+1]<-sw%*%cbind(y[n:(n-ht+1)])
                    }

              }

	     if(i>h+1 && i<n-h){
                           ye[1,i]<-gw%*%cbind(y[(i-h):(i+h)])
                           if(s>1){ye[2,i]<-sw%*%cbind(y[(i-h):(i+h)])}
                          }
        }

#### final results
     result<-ye

drop(result)
};
####################### End of the function ##########################
