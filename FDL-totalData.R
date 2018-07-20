library(zoo)
library(dynlm)
library(tidyverse)
library(broom)
library(dplyr)
setwd("C:/Users/12157/Desktop")
dat=read.csv("file:///C:/Users/12157/Desktop/fdl_norm.csv",header = T)
dat.ts=ts(data = dat, start=c(2013,5,31), end=c(2017,12,31),frequency=12)


space<-c(" "," "," "," ")
calFdl<-function(n){
  for(i in n){
    for(j in 1:12){
      if(j==1){
        dat.dyn <- dynlm(d(dat.ts[,73])~L(dat.ts[,i],0:j))
        gl_1<-glance(dat.dyn)[c("r.squared","AIC","BIC")]
        tab<- rbind(gl_1)
      }
      else{
        dat.dyn <- dynlm(d(dat.ts[,73])~L(dat.ts[,i],0:j))
        gl<-glance(dat.dyn)[c("r.squared","AIC","BIC")]
        tab<- rbind(tab , as.numeric(gl))
      }
    }
    vn<-c(names(dat)[i],"","","")
    bic<-c(min(tab$BIC),"","","")
    Lag<-c(which.min(tab$BIC),"","","")
    fdl <- dynlm(d(dat.ts[,73])~L(dat.ts[,i], 0:which.min(tab$BIC)))
    result<-summary(fdl)
    if(i==1){
      output<-rbind(vn,Lag,bic,coef(result))
    }
    else{
      temp<-rbind(space,vn,Lag,bic,coef(result))
      output<-rbind(output,temp)
    }
  }
  write.csv(output,"build_fd.csv")
}

calFdl(1)


