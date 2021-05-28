####################Blaa blaa blaa
#        pdf(file="Example.pdf", horizontal=F)
        par(mfrow=c(2,1), mai=c(0.6, 0.6, 0.6, 0.8))
### orders of margins -- b, l, t, r.

############ Call the DeSeaTS function
source("DeSeaTS-SP06.ssc")

########### Input the data 
x<-read.table("Trust-Comb-month.txt")

y<-x[,3]
#y<-log(y)
n<-length(y)
######
#y<-y[49:n]
#n<-length(y)

Year<-((1:n)-1)/12+1987 #### To chage the ob. no. to the correct year 


######### Privide the smoothing parameters
s<-12
p<-3
kn<-2
h<-4*s

ye<-DeSeaTS(y, s, p, kn, h)
yeg<-ye[1,]
yes<-ye[2,]

matplot(Year, cbind(y, yeg), type="l",  lty=c(5, 1), col=c(1,3)) 
title("The data and the fitted trend",cex=0.7)

matplot(Year, cbind(yes*0, yes), type="l", lty=c(2, 1), col=c(1,3)) 
title("The fitted seasonality",cex=0.7)


#dev.off()   #####If postscript is used
