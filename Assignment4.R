rm(list=ls())

head(dt)

##Question 1
#Part a
library(dplyr)
library(matrixStats)
mw<-colMeans(dt[,6:10])
ma<-colMeans(dt[dt$INF1GRW1==1,][,6:10])
mb<-colMeans(dt[dt$INF2GRW1==1,][,6:10])
mc<-colMeans(dt[dt$INF1GRW2==1,][,6:10])
md<-colMeans(dt[dt$INF2GRW2==1,][,6:10])
sw<-colSds(as.matrix(dt[,6:9]))
sa<-colSds(as.matrix(dt[dt$INF1GRW1==1,][,6:9]))
sb<-colSds(as.matrix(dt[dt$INF2GRW1==1,][,6:9]))
sc<-colSds(as.matrix(dt[dt$INF1GRW2==1,][,6:9]))
sd<-colSds(as.matrix(dt[dt$INF2GRW2==1,][,6:9]))
cbind(sw,sa,sb,sc,sd)
SRw<-data.frame(as.matrix(mw[1:4]-mw[5])/sw)
SRa<-data.frame(as.matrix(ma[1:4]-ma[5])/sa)
SRb<-data.frame(as.matrix(mb[1:4]-mb[5])/sb)
SRc<-data.frame(as.matrix(mc[1:4]-mc[5])/sc)
SRd<-data.frame(as.matrix(md[1:4]-md[5])/sd)
table1<-cbind(mw,ma,mb,mc,md)
colnames(table1)<-c("Whole Sample","INF1GRW1","INF2GRW1","INF1GRW2","INF2GRW2")
table2<-cbind(SRw,SRa,SRb,SRc,SRd)
colnames(table2)<-c("Whole Sample","INF1GRW1","INF2GRW1","INF1GRW2","INF2GRW2")

#Part b
vcvw<-cov((dt[,6:9]))
vcva<-cov((dt[dt$INF1GRW1==1,][,6:9]))
vcvb<-cov((dt[dt$INF2GRW1==1,][,6:9]))
vcvc<-cov((dt[dt$INF1GRW2==1,][,6:9]))
vcvd<-cov((dt[dt$INF2GRW2==1,][,6:9]))

#Part c
library(MASS)
Dw<-(c(1,1,1,1)%*%ginv(vcvw)%*%(mw[1:4]-mw[5]))
Da<-(c(1,1,1,1)%*%ginv(vcva)%*%(ma[1:4]-ma[5]))
Db<-(c(1,1,1,1)%*%ginv(vcvb)%*%(mb[1:4]-mb[5]))
Dc<-(c(1,1,1,1)%*%ginv(vcvc)%*%(mc[1:4]-mc[5]))
Dd<-(c(1,1,1,1)%*%ginv(vcvd)%*%(md[1:4]-md[5]))
wmsrw<-(ginv(vcvw)%*%(mw[1:4]-mw[5]))/as.numeric(Dw)
wmsra<-(ginv(vcva)%*%(ma[1:4]-ma[5]))/as.numeric(Da)
wmsrb<-(ginv(vcvb)%*%(mb[1:4]-mb[5]))/as.numeric(Db)
wmsrc<-(ginv(vcvc)%*%(mc[1:4]-mc[5]))/as.numeric(Dc)
wmsrd<-(ginv(vcvd)%*%(md[1:4]-md[5]))/as.numeric(Dd)

rpw<-as.matrix(dt[,6:9])%*%wmsrw
rpa<-as.matrix(dt[dt$INF1GRW1==1,][,6:9])%*%wmsra
rpb<-as.matrix(dt[dt$INF2GRW1==1,][,6:9])%*%wmsrb
rpc<-as.matrix(dt[dt$INF1GRW2==1,][,6:9])%*%wmsrc
rpd<-as.matrix(dt[dt$INF2GRW2==1,][,6:9])%*%wmsrd
sdpw<-sd(rpw)
sdpa<-sd(rpa)
sdpb<-sd(rpb)
sdpc<-sd(rpc)
sdpd<-sd(rpd)
SRpw<-(mean(rpw)-mw[5])/sdpw
SRpa<-(mean(rpa)-ma[5])/sdpa
SRpb<-(mean(rpb)-mb[5])/sdpb
SRpc<-(mean(rpc)-mc[5])/sdpc
SRpd<-(mean(rpd)-md[5])/sdpd

WMSR<-cbind(wmsrw,wmsra,wmsrb,wmsrc,wmsrd)
SRMSR<-cbind(SRpw,SRpa,SRpb,SRpc,SRpd)


#Part d
Dw<-c(1,1,1,1)%*%ginv(vcvw)%*%c(1,1,1,1)
Da<-c(1,1,1,1)%*%ginv(vcva)%*%c(1,1,1,1)
Db<-c(1,1,1,1)%*%ginv(vcvb)%*%c(1,1,1,1)
Dc<-c(1,1,1,1)%*%ginv(vcvc)%*%c(1,1,1,1)
Dd<-c(1,1,1,1)%*%ginv(vcvd)%*%c(1,1,1,1)
wgmvw<-(ginv(vcvw)%*%c(1,1,1,1))/as.numeric(Dw)
wgmva<-(ginv(vcva)%*%c(1,1,1,1))/as.numeric(Da)
wgmvb<-(ginv(vcvb)%*%c(1,1,1,1))/as.numeric(Db)
wgmvc<-(ginv(vcvc)%*%c(1,1,1,1))/as.numeric(Dc)
wgmvd<-(ginv(vcvd)%*%c(1,1,1,1))/as.numeric(Dd)

rpw<-as.matrix(dt[,6:9])%*%wgmvw
rpa<-as.matrix(dt[dt$INF1GRW1==1,][,6:9])%*%wgmva
rpb<-as.matrix(dt[dt$INF2GRW1==1,][,6:9])%*%wgmvb
rpc<-as.matrix(dt[dt$INF1GRW2==1,][,6:9])%*%wgmvc
rpd<-as.matrix(dt[dt$INF2GRW2==1,][,6:9])%*%wgmvd
sdpw<-sd(rpw)
sdpa<-sd(rpa)
sdpb<-sd(rpb)
sdpc<-sd(rpc)
sdpd<-sd(rpd)
SRpw<-(mean(rpw)-mw[5])/sdpw
SRpa<-(mean(rpa)-ma[5])/sdpa
SRpb<-(mean(rpb)-mb[5])/sdpb
SRpc<-(mean(rpc)-mc[5])/sdpc
SRpd<-(mean(rpd)-md[5])/sdpd

WGMV<-cbind(wgmvw,wgmva,wgmvb,wgmvc,wgmvd)
SRGMV<-cbind(SRpw,SRpa,SRpb,SRpc,SRpd)


#Part e
A<-c(1.3,2.8,6.5,10.5,16.9)
W<-data.frame(matrix( rep( 0), ncol=5,nrow = 25))
S<-data.frame(matrix( rep( 0), ncol=5,nrow = 4))

for (i in 1:5){

  wAw<-(ginv(vcvw)%*%(mw[1:4]-mw[5]))/A[i]
  wAa<-(ginv(vcva)%*%(ma[1:4]-ma[5]))/A[i]
  wAb<-(ginv(vcvb)%*%(mb[1:4]-mb[5]))/A[i]
  wAc<-(ginv(vcvc)%*%(mc[1:4]-mc[5]))/A[i]
  wAd<-(ginv(vcvd)%*%(md[1:4]-md[5]))/A[i]

  rpw<-as.matrix(dt[,6:9])%*%wAw+as.matrix(dt[,10])%*%(1-sum(wAw))
  rpa<-as.matrix(dt[dt$INF1GRW1==1,][,6:9])%*%wAa+as.matrix(dt[dt$INF1GRW1==1,][,10])%*%(1-sum(wAa))
  rpb<-as.matrix(dt[dt$INF2GRW1==1,][,6:9])%*%wAb+as.matrix(dt[dt$INF2GRW1==1,][,10])%*%(1-sum(wAb))
  rpc<-as.matrix(dt[dt$INF1GRW2==1,][,6:9])%*%wAc+as.matrix(dt[dt$INF1GRW2==1,][,10])%*%(1-sum(wAc))
  rpd<-as.matrix(dt[dt$INF2GRW2==1,][,6:9])%*%wAd+as.matrix(dt[dt$INF2GRW2==1,][,10])%*%(1-sum(wAd))
  sdpw<-sd(rpw)
  sdpa<-sd(rpa)
  sdpb<-sd(rpb)
  sdpc<-sd(rpc)
  sdpd<-sd(rpd)
  SRpw<-(mean(rpw)-mw[5])/sdpw
  SRpa<-(mean(rpa)-ma[5])/sdpa
  SRpb<-(mean(rpb)-mb[5])/sdpb
  SRpc<-(mean(rpc)-mc[5])/sdpc
  SRpd<-(mean(rpd)-md[5])/sdpd
  
  if (i==3){
    mmm<-cbind(mean(rpw),mean(rpa),mean(rpb),mean(rpc),mean(rpd))
    rrr<-cbind(mean(rpa),mean(rpb),mean(rpc),mean(rpd))
    sss<-cbind(sdpw,sdpa,sdpb,sdpc,sdpd)
    srsr<-cbind(SRpw,SRpa,SRpb,SRpc,SRpd)
    table<-rbind(mmm,sss,srsr)
  }
  wAw<-rbind(wAw,1-sum(wAw))
  wAa<-rbind(wAa,1-sum(wAa))
  wAb<-rbind(wAb,1-sum(wAb))
  wAc<-rbind(wAc,1-sum(wAc))
  wAd<-rbind(wAd,1-sum(wAd))
  WAR<-cbind(wAw,wAa,wAb,wAc,wAd)
  SAR<-cbind(SRpw,SRpa,SRpb,SRpc,SRpd)
  
  W[(5*i-4):(5*i),]<-WAR
  S[i,]<-SAR
}
W
S
rrr

##Question 2
i<-3
weight6.5<-W[(5*i-4):(5*i-1),2:5]

wstatic<-weight6.5[,1]/4+weight6.5[,2]/4+weight6.5[,3]/4+weight6.5[,4]/4
watilt<-weight6.5[,1]/2+weight6.5[,2]/6+weight6.5[,3]/6+weight6.5[,4]/6
wbtilt<-weight6.5[,1]/6+weight6.5[,2]/2+weight6.5[,3]/6+weight6.5[,4]/6
wctilt<-weight6.5[,1]/6+weight6.5[,2]/6+weight6.5[,3]/2+weight6.5[,4]/6
wdtilt<-weight6.5[,1]/6+weight6.5[,2]/6+weight6.5[,3]/6+weight6.5[,4]/2
WWW<-cbind(wstatic,watilt,wbtilt,wctilt,wdtilt)
S2<-data.frame(matrix( rep( 0), ncol=5,nrow = 4))
i<-4
for (i in 1:5){
  
  rpw<-as.matrix(dt[,6:9])%*%WWW[,i]+as.matrix(dt[,10])%*%(1-sum(WWW[,i]))
  rpa<-as.matrix(dt[dt$INF1GRW1==1,][,6:9])%*%WWW[,i]+as.matrix(dt[dt$INF1GRW1==1,][,10])%*%(1-sum(WWW[,i]))
  rpb<-as.matrix(dt[dt$INF2GRW1==1,][,6:9])%*%WWW[,i]+as.matrix(dt[dt$INF2GRW1==1,][,10])%*%(1-sum(WWW[,i]))
  rpc<-as.matrix(dt[dt$INF1GRW2==1,][,6:9])%*%WWW[,i]+as.matrix(dt[dt$INF1GRW2==1,][,10])%*%(1-sum(WWW[,i]))
  rpd<-as.matrix(dt[dt$INF2GRW2==1,][,6:9])%*%WWW[,i]+as.matrix(dt[dt$INF2GRW2==1,][,10])%*%(1-sum(WWW[,i]))
  sdpw<-sd(rpw)
  sdpa<-sd(rpa)
  sdpb<-sd(rpb)
  sdpc<-sd(rpc)
  sdpd<-sd(rpd)
  SRpw<-(mean(rpw)-mw[5])/sdpw
  SRpa<-(mean(rpa)-ma[5])/sdpa
  SRpb<-(mean(rpb)-mb[5])/sdpb
  SRpc<-(mean(rpc)-mc[5])/sdpc
  SRpd<-(mean(rpd)-md[5])/sdpd
  
  SAR<-cbind(SRpw,SRpa,SRpb,SRpc,SRpd)
  S2[i,]<-SAR
}
S2