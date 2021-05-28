####################Blaa blaa blaa
#        pdf(file="Example.pdf", horizontal=F)
        par(mfrow=c(2,1), mai=c(0.6, 0.6, 0.6, 0.4))
### orders of margins -- b, l, t, r.

############ Call the DeSeaTS function
source("DeSeaTS-SP06.r")

########### Input the data 
#x<-read.table("Trust-OASI-Year.txt")

#Year<-x[,1]
#y<-x[,2]
#y<-log(y)

n<-10000
y<-rnorm(n)
tx<-(1:n)/n

y<-y+sin(100*tx)+10*tx + 5*tx**2

######### Privide the smoothing parameters
s<-1
p<-1
kn<-2
h<-120

ye<-DeSeaTS(y, s, p, kn, h)

#matplot(Year, cbind(y, ye), type="l", lty=c(3, 1), col=c(5,1)) 
matplot(tx, cbind(y, ye), type="l", lty=c(3, 1), col=c(5,1)) 


#dev.off()   #####If postscript is used

