install.packages("devtools")  # if not already installed
library(devtools)
install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata) 
library(zoo)
library(dynlm)
library(tidyverse)
library(broom)

data("okun", package="PoEdata")
check.ts <- is.ts(okun) # "is structured as time series?"
okun.ts <- ts(okun, start=c(1985,2), end=c(2009,3),frequency=4)
okun.ts[,1]#g
okun.ts[,2]#u
#shows how lags and differences work. 
#Please note how each lag uses up an observation period
okun.ts.tab <- cbind(okun.ts,
                     lag(okun.ts[,2], -1),
                     diff(okun.ts[,2], lag=1),
                     lag(okun.ts[,1], -1),
                     lag(okun.ts[,1], -2),
                     lag(okun.ts[,1], -3))
kable(head(okun.ts.tab), 
      caption="The `okun` dataset with differences and lags",
      col.names=c("g","u","uL1","du","gL1","gL2","gL3"))

#distributed lag model with 3 lags
okunL3.dyn <- dynlm(d(okun.ts[,2])~L(okun.ts[,1], 0:3), data=okun.ts)
summary(okunL3.dyn)

#distributed lag model with 2 lags
okunL2.dyn <- dynlm(d(u)~L(g, 0:2), data=okun.ts)
okunL2.dyn <- dynlm(d(okun.ts[,2])~L(okun.ts[,1], 0:2))
summary(okunL2.dyn) 

glL3 <- glance(okunL3.dyn)[c("r.squared","statistic","AIC","BIC")]
glL2 <- glance(okunL2.dyn)[c("r.squared","statistic","AIC","BIC")]
tabl <- rbind(glL3, as.numeric(glL2))


