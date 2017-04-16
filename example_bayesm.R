install.packages("mlogit")
library(mlogit)
data("Electricity", package = "mlogit")
Electr <- mlogit.data(Electricity, id="id", choice="choice",
                      varying=3:26, shape="wide", sep="")
Elec.mxl <- mlogit(choice~pf+cl+loc+wk+tod+seas|0, Electr,
                   rpar=c(pf='n', cl='n', loc='n', wk='n', tod='n', seas='n'),
                   R=100, halton=NA, print.level=0, panel=TRUE)
summary(Elec.mxl)
########################################################################################

library(bayesm)
id=levels(as.factor(Electricity$id))
nresp<-length(unique(id))
lgtdata=NULL


for (i in 1:nresp)
{
  respdata=Electricity[Electricity$id==id[i],]
  ty<-NULL
  tdesign<-NULL
  ty=respdata$choice
  nobs=length(ty)
  for (j in 1:nobs) {
    design1<-as.matrix(respdata[j,c(3,7,11,15,19,23)])
    design2<-as.matrix(respdata[j,c(4,8,12,16,20,24)])
    design3<-as.matrix(respdata[j,c(5,9,13,17,21,25)])
    design4<-as.matrix(respdata[j,c(6,10,14,18,22,26)])
    tdesign<-rbind(tdesign,design1,design2,design3,design4)
  }
  lgtdata[[i]]=list(y=ty,X=as.matrix(tdesign))
}
lgtdata

mcmc=list(R=2000,keep=10)
out=rhierMnlRwMixture(Data=list(p=4,lgtdata=lgtdata),
                      Prior=list(ncomp=1),Mcmc=mcmc)


plot(out$loglike,type="l")
trace<-t(apply(out$betadraw,c(2,3),mean))
matplot(trace, type="l")

beta.51_200<-apply(out$betadraw[,,51:200],2,mean)
beta.101_200<-apply(out$betadraw[,,101:200],2,mean)
beta.151_200<-apply(out$betadraw[,,151:200],2,mean)
cbind(beta.51_200,beta.101_200,beta.151_200)

estimate<-apply(out$betadraw[,,101:200],c(1,2),mean)
estimate2<-cbind(matrix(id),estimate)
write.csv(estimate2, file="estimate.csv")
