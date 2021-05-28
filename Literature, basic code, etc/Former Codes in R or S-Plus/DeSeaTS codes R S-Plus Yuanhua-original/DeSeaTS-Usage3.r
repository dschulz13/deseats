########## The main program for smoothing the DI monthly data ##########
########## with two local dummy variables at tb.        ##########
########## Programmer: Yuanhua Feng                 ##########
########## Date: July 2006                          ##########

######################################
source("DeSeaTS-SP06-SB.r")

############## The data  ##############
x<-read.table("Trust-DI-month.txt")
y<-x[,3]

s<-12
p<-1   
n<-length(y)           ## number of observations
kn<-2           
h<-trunc(2.5*s)


################## Special procedures for the point with structural breaks
tb<-???    ################ The position where the breaks occur, please find it out
y[tb]<-??? ################# The first correction in the data, 
           ################# please calculate this following the method explained in the meeting 
           ################# Following this program you do not need to change the data file

################################### Calculation with given tb, 
ye<-DeSeaTS.SB(y,s,p, kn, h, tb) #### Very similar to the use of the other program
                                 #### just with one additional parameter tb indicating the time of breaks
 
ge<-ye[1,]
se<-ye[2,]
t0<-1987
Year<-((1:n)-1)/s+t0


#############################################################
#############################################################
        par(mfrow=c(2,1), mai=c(0.8, 0.9, 0.8, 0.9))

matplot(Year, cbind(y, ge), type="l", lty=c(1, 3), col=c(5,6))
title("... ...", cex=0.6)
title(xlab="Observation Year")

matplot(Year, cbind(se, se*0), type="l", lty=c(1, 2), col=c(3,1))
title(" ... .... ", cex=0.6)
title(xlab="Observation Year")



