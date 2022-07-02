#set working directory
#Author: Mia Li
setwd("D:/Mia/STAT/626/626 project documents")
######################################## Define Variables ############################
growth_nt_cases(t)=new_cases(t)/total_cases(t-1)
growth_nt_deaths=new_deaths(t)/total_deaths(t-1)
perc_deaths_cases=total_deaths/total_cases
perc_new_ct=new_cases/new_tests #daily
perc_total_ct=total_cases/total_tests #accumulated

######################################## Covid19 #####################################
#read data
covid=read.table("covid19-USA-0308.txt",header = T) #03/08/2020 to 06/01/2020

#plot data
ts.plot(covid$growth_nt_cases) #growth rate of Covid19 infection
ts.plot(covid$growth_nt_deaths)#growth rate of deaths related to Covid19
ts.plot(covid$perc_deaths_cases) #daily ratio of deaths related to Covid19
ts.plot(covid$perc_new_ct)#percentage of positive tests
ts.plot(covid$perc_total_ct) #accumulated percentage of Covid19 positive tests

#Data transformation
covid_gc=diff((covid$growth_nt_cases)**(1/3))
covid_cd=diff((covid$growth_nt_deaths)**(1/3))
covid_pd=diff((covid$perc_deaths_cases)**(1/3))
covid_pn=diff((covid$perc_new_ct)**(1/3))
covid_pt=diff((covid$perc_total_ct)**(1/3))

#check for stationarity
ts.plot(covid_gc) #stationary **selected based on data plot
ts.plot(covid_cd) #stationary
ts.plot(covid_pd) #non-stationary
ts.plot(covid_pn) #stationary **selected based on data plot
ts.plot(covid_pt) #non-stationary

######################################## VIX #####################################
#read data
vix=read.table("vix data 2020.txt",header = T) #01/02/2020 - 06/05/2020
#convert date column to date class
vix[,1]=as.Date(vix[,1],format = "%m/%d/%Y",origin = "2019-01-01")
#data imputation
library("imputeTS")
vix_=matrix(unlist(vix), ncol = 5, byrow = F)
vix[,2:5]=as.numeric(vix_[,2:5])
vix_imputed=na_interpolation(vix, option = "spline")

vix=vix_imputed[67:152,] #03082020,06012020

#calculate new quantities
#growth_vix=(vix_close-vix_open)/vix_open*100
growth_vix=((vix$VIX_Close-vix$VIX_Open)/vix$VIX_Open)*100 

#daily growth rate(measure daily volability)
vix_high_low=vix$VIX_High-vix$VIX_Low 

#plot data
ts.plot(growth_vix) #non-stationary
ts.plot(vix_high_low) #non-stationary

#data transformation
ts.plot(diff(growth_vix))
ts.plot(diff(vix_high_low))

  #better results of stationarity
  cuberoot=function(x){
    x=sign(x)*abs(x)^(1/3)
  }
  ts.plot(diff(cuberoot(growth_vix))) #stationary
  ts.plot(diff(cuberoot(vix_high_low))) #stationary
  #Selected transformed variable
  g_vix=diff(cuberoot(growth_vix))
  hl_vix=diff(cuberoot(vix_high_low))
######################################## Nasdaq ###################################
#read data
nq_com=read.table("IXIC-NASDAQ COMPOSITE HistoricalQuotes.txt",header = T)
nq_com=nq_com[1:154,] # 12/31/2019 to 06/01/2020
#convert date column to date class
nq_com[,1]=as.Date(nq_com[,1],format = "%m/%d/%Y",origin = "2019-01-01")

#Sort by date, increasing
nq=nq_com[
  with(nq_com, order(nq_com[,1])),
]

#data imputation
library("imputeTS")
nq_=matrix(unlist(nq), ncol = 5, byrow = F)
nq[,2:5]=as.numeric(nq_[,2:5])
nq=na_interpolation(nq, option = "spline")

nq_ordered=nq[
  with(nq, order(nq[,1])),
]
nq=nq_ordered[69:154,] #03082020,06012020

#calculate new quantities
#growth_nq=(nq_close-nq_open)/nq_open*100
growth_nq=((nq$Close.Last-nq$Open)/nq$Open)*100 

#daily growth rate(measure daily volability)
nq_high_low=nq$High-nq$Low 

#plot data
ts.plot(growth_nq) 
ts.plot(nq_high_low) #non-stationary

#data transformation
ts.plot(diff(growth_nq)) #stationary
ts.plot(diff(nq_high_low)) #stationary

  #better results of stationarity
  ts.plot(diff(cuberoot(growth_nq))) #stationary
  ts.plot(diff(cuberoot(nq_high_low))) #stationary
  #Selected transformed variable
  g_nq=diff(cuberoot(growth_nq))
  hl_nq=diff(cuberoot(nq_high_low))
######################################## sp500 ###################################
#read data
  sp500=read.table("SP500 HistoricalQuotes.txt",header = T)
  sp500=sp500[1:154,c(-3)] # 12/31/2019 to 06/01/2020
  #convert date column to date class
  sp500[,1]=as.Date(sp500[,1],format = "%m/%d/%Y",origin = "2019-01-01")
  
#Sort by date, increasing
  sp500=sp500[
    with(sp500, order(sp500[,1])),
  ]
  
#data imputation
  library("imputeTS")
  sp500_=matrix(unlist(sp500), ncol = 5, byrow = F)
  sp500[,2:5]=as.numeric(sp500_[,2:5])
  sp500=na_interpolation(sp500, option = "spline")
  
  sp500_ordered=sp500[
    with(sp500, order(sp500[,1])),
  ]
  sp500=sp500_ordered[69:154,] #03082020,06012020
  
  
#calculate new quantities
  growth_sp500=((sp500$Close.Last-sp500$Open)/sp500$Open)*100 
  
#daily growth rate(measure daily volability)
  sp500_high_low=sp500$High-sp500$Low 
  
#plot data
  ts.plot(growth_sp500) 
  ts.plot(sp500_high_low) #non-stationary
  
#data transformation
  ts.plot(diff(growth_sp500)) #stationary
  ts.plot(diff(sp500_high_low)) #stationary
  
 #better results of stationarity
  ts.plot(diff(cuberoot(growth_sp500))) #stationary
  ts.plot(diff(cuberoot(sp500_high_low))) #stationary
 #Selected transformed variable
  g_sp500=diff(cuberoot(growth_sp500))
  hl_sp500=diff(cuberoot(sp500_high_low))
  
######################################## airline ###################################
  #read data
  airline=read.table("XAL-NASDAQ Airline.txt",header = T)
  airline=airline[1:92,] # 12/31/2019 to 06/01/2020
  #convert date column to date class
  airline[,1]=as.Date(airline[,1],format = "%m/%d/%Y",origin = "2019-01-01")
  
  #Sort by date, increasing
  airline=airline[
    with(airline, order(airline[,1])),
  ]
  
  #data imputation
  library("imputeTS")
  airline_=matrix(unlist(airline), ncol = 5, byrow = F)
  airline[,2:5]=as.numeric(airline_[,2:5])
  airline=na_interpolation(airline, option = "spline")
  
  airline_ordered=airline[
    with(airline, order(airline[,1])),
  ]
  airline=airline_ordered[7:92,] #03082020,06012020
  
  
  #calculate new quantities
  growth_airline=((airline$Close.Last-airline$Open)/airline$Open)*100 
  
  #daily growth rate(measure daily volability)
  airline_high_low=airline$High-airline$Low 
  
  #plot data
  ts.plot(growth_airline) 
  ts.plot(airline_high_low) #non-stationary
  
  #data transformation
  ts.plot(diff(growth_airline)) #stationary
  ts.plot(diff(airline_high_low)) #stationary
  
  #better results of stationarity
  ts.plot(diff(cuberoot(growth_airline))) #stationary
  ts.plot(diff(cuberoot(airline_high_low))) #stationary
  #Selected transformed variable
  g_airline=diff(cuberoot(growth_airline))
  hl_airline=diff(cuberoot(airline_high_low))

######################################## biotech ###################################
  #read data
  biotech=read.table("NBI-NASDAQ Biotech.txt",header = T)
  #convert date column to date class
  biotech[,1]=as.Date(biotech[,1],format = "%m/%d/%Y",origin = "2019-01-01")
  
  #Sort by date, increasing
  biotech=biotech[
    with(biotech, order(biotech[,1])),
  ]
  
  #data imputation
  library("imputeTS")
  biotech_=matrix(unlist(biotech), ncol = 5, byrow = F)
  biotech[,2:5]=as.numeric(biotech_[,2:5])
  biotech=na_interpolation(biotech, option = "spline")
  
  biotech_ordered=biotech[
    with(biotech, order(biotech[,1])),
  ]
  biotech=biotech_ordered[7:92,] #03082020,06012020
  
  
  #calculate new quantities
  growth_biotech=((biotech$Close.Last-biotech$Open)/biotech$Open)*100 
  
  #daily growth rate(measure daily volability)
  biotech_high_low=biotech$High-biotech$Low 
  
  #plot data
  ts.plot(growth_biotech) 
  ts.plot(biotech_high_low) #non-stationary
  
  #data transformation
  ts.plot(diff(growth_biotech)) #stationary
  ts.plot(diff(biotech_high_low)) #stationary
  
  #better results of stationarity
  ts.plot(diff(cuberoot(growth_biotech))) #stationary
  ts.plot(diff(cuberoot(biotech_high_low))) #stationary
  #Selected transformed variable
  g_biotech=diff(cuberoot(growth_biotech))
  hl_biotech=diff(cuberoot(biotech_high_low))  

######################################## pharmaceutical ###################################
  #read data
  pharm=read.table("DRG-Pharmaceutical.txt",header = T)
  #convert date column to date class
  pharm[,1]=as.Date(pharm[,1],format = "%m/%d/%Y",origin = "2019-01-01")
  
  #Sort by date, increasing
  pharm=pharm[
    with(pharm, order(pharm[,1])),
  ]
  
  #data imputation
  library("imputeTS")
  pharm_=matrix(unlist(pharm), ncol = 5, byrow = F)
  pharm[,2:5]=as.numeric(pharm_[,2:5])
  pharm=na_interpolation(pharm, option = "spline")
  
  pharm_ordered=pharm[
    with(pharm, order(pharm[,1])),
  ]
  pharm=pharm_ordered[7:92,] #03082020,06012020
  
  
  #calculate new quantities
  growth_pharm=((pharm$Close.Last-pharm$Open)/pharm$Open)*100 
  
  #daily growth rate(measure daily volability)
  pharm_high_low=pharm$High-pharm$Low 
  
  #plot data
  ts.plot(growth_pharm) 
  ts.plot(pharm_high_low) #non-stationary
  
  #data transformation
  ts.plot(diff(growth_pharm)) #stationary
  ts.plot(diff(pharm_high_low)) #stationary
  
  #better results of stationarity
  ts.plot(diff(cuberoot(growth_pharm))) #stationary
  ts.plot(diff(cuberoot(pharm_high_low))) #stationary
  #Selected transformed variable
  g_pharm=diff(cuberoot(growth_pharm))
  hl_pharm=diff(cuberoot(pharm_high_low))  
  
######################################## cross-correlation ########################
library("astsa")
par(mfrow=c(2,2))
ccf2(covid_gc,diff(cuberoot(growth_vix)))
ccf2(covid_gc,diff(cuberoot(vix_high_low)))
ccf2(covid_pn,diff(cuberoot(growth_vix)))
ccf2(covid_pn,diff(cuberoot(vix_high_low)))

par(mfrow=c(2,2))
ccf2(covid_gc,diff(cuberoot(growth_nq)))
ccf2(covid_gc,diff(cuberoot(nq_high_low)))
ccf2(covid_pn,diff(cuberoot(growth_nq)))
ccf2(covid_pn,diff(cuberoot(nq_high_low)))

par(mfrow=c(2,2))
ccf2(covid_gc,diff(cuberoot(growth_sp500)))
ccf2(covid_gc,diff(cuberoot(sp500_high_low)))
ccf2(covid_pn,diff(cuberoot(growth_sp500)))
ccf2(covid_pn,diff(cuberoot(sp500_high_low)))

par(mfrow=c(2,2))
ccf2(g_airline,covid_gc)
ccf2(hl_airline,covid_gc)
ccf2(g_airline,covid_pn)
ccf2(hl_airline,covid_pn)

######################################## Model Fitting ##################
library(MTS)
######################################## Define Tests #####################################
  
vix_covid=cbind(g_vix,hl_vix,covid_gc,covid_pn) #significant
nqcom_covid=cbind(g_nq,hl_nq,covid_gc,covid_pn)
sp500_covid=cbind(g_sp500,hl_sp500,covid_gc,covid_pn)
airline_covid=cbind(g_airline,hl_airline,covid_gc,covid_pn)
biotech_covid=cbind(g_biotech,hl_biotech,covid_gc,covid_pn)
pharm_covid=cbind(g_pharm,hl_pharm,covid_gc,covid_pn)

######################################## Testing Zero Cross-Correlations #################################
#H0: rho_i's all equal to zero (no cross correlation)
mq(vix_covid,lag=10) #reject
mq(nqcom_covid,lag=10) #reject
mq(sp500_covid,lag=10) #reject
mq(airline_covid,lag=10) #reject
mq(biotech_covid,lag=10) #fail to reject all
mq(pharm_covid,lag=10) #reject

######################################## Cross Correlation Matrix #########################
ccm(vix_covid,lag=10) #ccf14
ccm(nqcom_covid,lag=10) #ccf14,24,23
ccm(sp500_covid,lag=10) #ccf14
ccm(airline_covid,lag=10) #not significant
ccm(biotech_covid,lag=10) #not significant
ccm(pharm_covid,lag=10) #ccf14,23,24

#base on above results, adjust the models
#ccf14
g_vix_covid_pn=cbind(g_vix,covid_pn)#1

#ccf14,24,23
g_nq_covid_pn=cbind(g_nq,covid_pn)#2
hl_nq_covid_pn=cbind(hl_nq,covid_pn)#3 should be removed
hl_nq_covid_gc=cbind(hl_nq,covid_gc)#5 newly added

#ccf14
g_sp500_covid_pn=cbind(g_sp500,covid_pn)#4

#ccf14,23,24
g_pharm_covid_pn=cbind(g_pharm,covid_pn)#6
hl_pharm_covid_gc=cbind(hl_pharm,covid_gc)#7
hl_pharm_covid_pn=cbind(hl_pharm,covid_pn)#8


######################################## Granger Causality ####################
library(lmtest)

p_values_1=rep(0.001,10)
p_values_2=rep(0.001,10)
p_values_3=rep(0.001,10)
p_values_4=rep(0.001,10)
p_values_5=rep(0.001,10)
p_values_6=rep(0.001,10)
p_values_7=rep(0.001,10)
p_values_8=rep(0.001,10)

?grangertest

g_vix_covid_pn=cbind(g_vix,covid_pn)#1
g_nq_covid_pn=cbind(g_nq,covid_pn)#2
hl_nq_covid_pn=cbind(hl_nq,covid_pn)#3 should be removed
g_sp500_covid_pn=cbind(g_sp500,covid_pn)#4


for(i in 1:10){
  p_values_1[i]=round(grangertest(covid_pn,g_vix,order = i)$`Pr(>F)`[2],3)
}

for(i in 1:10){
  p_values_2[i]=round(grangertest(covid_pn,g_nq,order = i)$`Pr(>F)`[2],3)
}

for(i in 1:10){
  p_values_3[i]=round(grangertest(covid_pn,hl_nq,order = i)$`Pr(>F)`[2],3)
}

for(i in 1:10){
  p_values_4[i]=round(grangertest(covid_pn,g_sp500,order = i)$`Pr(>F)`[2],3)
}

hl_nq_covid_gc=cbind(hl_nq,covid_gc)#5 newly added
g_pharm_covid_pn=cbind(g_pharm,covid_pn)#6
hl_pharm_covid_gc=cbind(hl_pharm,covid_gc)#7
hl_pharm_covid_pn=cbind(hl_pharm,covid_pn)#8

for(i in 1:10){
  p_values_5[i]=round(grangertest(covid_gc,hl_nq,order = i)$`Pr(>F)`[2],3)
}

for(i in 1:10){
  p_values_6[i]=round(grangertest(covid_pn,g_pharm,order = i)$`Pr(>F)`[2],3)
}

for(i in 1:10){
  p_values_7[i]=round(grangertest(covid_gc,hl_pharm,order = i)$`Pr(>F)`[2],3)
}

for(i in 1:10){
  p_values_8[i]=round(grangertest(covid_pn,hl_pharm,order = i)$`Pr(>F)`[2],3)
}

p_values_1
p_values_2
p_values_3
p_values_4
p_values_5
p_values_6
p_values_7
p_values_8
p_values_sign=0.05

#1 graph

plot(p_values_1, axes=T, ylim=c(0,1), , xlim=c(0,15.5),xlab="Lags", ylab="",type="l",col="blue", main="",lty=1)
points(p_values_1,pch=20,col="blue")

mtext(2,text="P-values for Granger Causality Test",line=2)
abline(h=0.05,col="red")

lines(p_values_2, xlab="", ylab="",type="l",col="black", main="",lty=2)
points(p_values_2,pch=20,col="black")

lines(p_values_3, xlab="", ylab="",type="l",col="black", main="",lty=3)
points(p_values_3,pch=20,col="black")

lines(p_values_4, xlab="", ylab="",type="l",col="black", main="",lty=4)
points(p_values_4,pch=20,col="black")
legend(x=10,y=1,legend=c("Growth Rate of VIX-Covid19","Growth Rate of Nasdaq Composite-Covid19 ",
                         "Diff(High,Low) of Nasdaq Composite-Covid19","Growth Rate of SP500-Covid"),lty=c(1,2,3,4))


#2 graph

plot(p_values_5, axes=T, ylim=c(0,1),xlim=c(0,18), xlab="Lags", ylab="",type="l",col="black", main="",lty=1)
points(p_values_5,pch=20,col="black")
mtext(2,text="P-values for Granger Causality Test",line=2)
abline(h=0.05,col="red")

lines(p_values_6, xlab="", ylab="",type="l",col="blue", main="",lty=2)
points(p_values_6,pch=20,col="blue")

lines(p_values_7, xlab="", ylab="",type="l",col="black", main="",lty=3)
points(p_values_7,pch=20,col="black")

lines(p_values_8, xlab="", ylab="",type="l",col="black", main="",lty=4)
points(p_values_8,pch=20,col="black")

legend(x=8.5,y=1,legend=c("Diff(High,Low) of Nasdaq Composite-Covid19_Growth Rate","Growth Rate of Pharmaceutical Index-Covid19_Percentage of Positive",
                         "Diff(High,Low) of Pharmaceutical Index-Covid19_Growth Rate","Diff(High,Low) of Pharmaceutical Index-Covid_Percentage of Positive"),lty=c(1,2,3,4))

######################################## VMA ##########################################
VMAorder(g_vix_covid_pn,lag=20)#1
VMAorder(g_nq_covid_pn,lag=20)#2
VMAorder(hl_nq_covid_pn,lag=)#3
VMAorder(g_sp500_covid_pn,lag=20)#4
VMAorder(hl_nq_covid_gc,lag=20)#5
VMAorder(g_pharm_covid_pn,lag=20)#6
VMAorder(hl_pharm_covid_gc,lag=20)#7 
VMAorder(hl_pharm_covid_pn,lag=20)#8

#1: g_vix_covid_pn: when j>=12, p-values > 0.05, Fail to reject H0.
#Thus, roh_12=roh_13...=roh_m=0. q in VMA(q) is 11.

#2: g_nq_covid_pn: when j>=2, p-values > 0.05, Fail to reject H0.
#Thus, roh_2=roh_3...=roh_m=0. q in VMA(q) is 1.

#3: hl_nq_covid_pn: when j>=1, p-values > 0.05, Fail to reject H0.
#Thus, roh_1=roh_2...=roh_m=0. q in VMA(q) is 0.

#4: g_sp500_covid_pn: when j>=12, p-values > 0.05, Fail to reject H0.
#Thus, roh_12=roh_13...=roh_m=0. q in VMA(q) is 11.

#5: hl_nq_covid_gc: when j>=1, p-values > 0.05, Fail to reject H0.
#Thus, roh_1=roh_2...=roh_m=0. q in VMA(q) is 0.

#6: hl_pharm_covid_pn: when j>=3, p-values > 0.05, Fail to reject H0.
#Thus, roh_3=roh_4...=roh_m=0. q in VMA(q) is 2.

#7: hl_pharm_covid_pn: when j>=1, p-values > 0.05, Fail to reject H0.
#Thus, roh_1=roh_2...=roh_m=0. q in VMA(q) is 0.

#8: hl_pharm_covid_pn: when j>=3, p-values > 0.05, Fail to reject H0.
#Thus, roh_3=roh_4...=roh_m=0. q in VMA(q) is 2.


vma1=VMA(g_vix_covid_pn,q=1) #three of seven significant; aic=  -4.935738 bic=  -4.763316
evma1=VMAe(g_vix_covid_pn,q=1) #two of six significant; aic=  -4.864836 bic=  -5.006012 
vma1_ref=refVMA(vma1) #two of five significant; aic=  -2.69407 bic=  -2.550384
evma1_ref=refVMA(evma1) #two of three significant; aic=  -2.832271 bic=  -2.74606
vma1_ref_=refVMAe(vma1) #**four of five significant; aic=  -4.884339 bic=  -5.001986
evma1_ref_=refVMAe(evma1) #two of three significant; aic=  -4.84496 bic=  -4.915548 

vma2=VMA(g_nq_covid_pn,q=2) #eight of ten significant; aic=  -5.816704 bic=  -5.529333
evma2=VMAe(g_nq_covid_pn,q=3) #thirteen of fourteen significant;aic=  -3.301004 bic=  -3.630416
#vma2_ref=refVMA(vma2) #na produced 
#evma2_ref=refVMA(evma2)  #na produced
vma2_ref_=refVMAe(vma2,thres = 1.96) #**all eight significant; aic=  -5.830638 bic=  -6.018873 
evma2_ref_=refVMAe(evma2) #na produced 

vma3=VMA(hl_nq_covid_pn,q=1)#**three of six significant; aic=  -6.87296 bic=  -6.700538 
evma3=VMAe(hl_nq_covid_pn,q=1)#three of six significant; aic= -6.872635 bic=  -7.013811
vma3_ref=refVMA(vma3) #one of three significant; aic=  -6.017103  bic=  -5.930892 
evma3_ref=refVMA(evma3) #one of three significant; aic=  -5.990113 bic=  -5.903902 
vma3_ref_=refVMAe(vma3) #two of three significant; aic=  -6.937103 bic=  -7.007691  
evma3_ref_=refVMAe(evma3) #two of three significant; aic=  -6.937103 bic=  -7.007691 

vma4=VMA(g_sp500_covid_pn,q=3)#ERROR!all fourteen significant; aic=  -Inf bic=  -Inf(singular: U[2,2] = 0)  
evma4=VMAe(g_sp500_covid_pn,q=1)#three of six significant; aic=  -5.492706 bic=  -5.633882
vma4_ref=refVMA(vma4) #three of fourteen significant; aic=  5.943445 bic=  6.345764  
evma4_ref=refVMA(evma4) #two of three significant;zero; aic=  -2.458627 bic=  -2.372415
vma4_ref_=refVMAe(vma4) #ERROR!all fourteen significant; aic=  -4.490771  bic=  -4.820183  
evma4_ref_=refVMAe(evma4) #all three significant;zero; aic=  -5.546488 bic=  -5.617076 

#vma5=VMA(hl_nq_covid_gc,q=3)#na
vma5=VMA(hl_nq_covid_gc,q=1)#two of six significant; aic= -7.594088 bic=  -7.421666 
evma5=VMAe(hl_nq_covid_gc,q=1)#two of six significant; aic=  -7.594058 bic=  -7.735235
vma5_ref=refVMA(vma5) #one of four significant; aic=  -7.402737 bic=  -7.287788  
evma5_ref=refVMA(evma5) #one of four significant; aic=  -7.403389 bic=  -7.288441
vma5_ref_=refVMAe(vma5) #two of four significant;zero; aic=  -7.636004   bic=  -7.730122  
evma5_ref_=refVMAe(evma5)#two of four significant;zero; same: aic=  -7.636004   bic=  -7.730122  

vma6=VMA(g_pharm_covid_pn,q=2)#eight of ten significant; aic=  -6.185929 bic=  -5.898558
evma6=VMAe(g_pharm_covid_pn,q=2)#eight of ten significant; aic=  -5.598613 bic=  -5.833907
vma6_ref=refVMA(vma6) #three of eight significant; aic=  -4.126596 bic=  -3.896699  
evma6_ref=refVMA(evma6) #na
vma6_ref_=refVMAe(vma6) #***all eight significant; aic=  -5.923602 bic=  -6.111837  
evma6_ref_=refVMAe(evma6)#eight of nine significant; aic=  -5.675948 bic=  -5.887713


vma8=VMA(hl_pharm_covid_pn,q=2)#five of ten significant; aic=  -7.773207 bic=  -7.485836
evma8=VMAe(hl_pharm_covid_pn,q=2)#nine of ten significant; aic=  -7.258665 bic=  -7.493959
vma8_ref=refVMA(vma8) #two of seven significant; aic=  -6.301124 bic=  -6.099965   
evma8_ref=refVMA(evma8) #na
vma8_ref_=refVMAe(vma8) #na  
evma8_ref_=refVMAe(evma8)#**all nine significant; aic=  -7.436866 bic=  -7.648631 

######################################## VAR ############################################
#1
VARorder(g_vix_covid_pn)
#selected order: aic =  12 
#selected order: bic =  1 
#selected order: hq =  1

#2
VARorder(g_nq_covid_pn)
#selected order: aic =  10 
#selected order: bic =  1 
#selected order: hq =  2

#3
VARorder(hl_nq_covid_pn)
#selected order: aic =  2 
#selected order: bic =  2 
#selected order: hq =  2

#4
VARorder(g_sp500_covid_pn)
#selected order: aic =  11 
#selected order: bic =  1 
#selected order: hq =  2

#5
VARorder(hl_nq_covid_gc)
#selected order: aic =  2 
#selected order: bic =  2 
#selected order: hq =  2

#6
VARorder(g_pharm_covid_pn)
#selected order: aic =  2 
#selected order: bic =  2 
#selected order: hq =  2 

#7
VARorder(hl_pharm_covid_gc)
#selected order: aic =  3 
#selected order: bic =  0 
#selected order: hq =  2 

#8
VARorder(hl_pharm_covid_pn)
#selected order: aic =  5 
#selected order: bic =  2 
#selected order: hq =  2 

##Fit VAR models
#at is a sequence of independent and identically distributed random vectors with mean zero and covariance matrix of residuals
var1=VAR(g_vix_covid_pn,p=1)
#det(SSE) =  0.008608435 
#AIC =  -4.660895 
#BIC =  -4.545947 
#HQ  =  -4.61466
var1_ref=refVAR(var1) #constant terms estimation goes to zero


var2=VAR(g_nq_covid_pn,p=1)
#det(SSE) =  0.004069873 
#AIC =  -5.410026 
#BIC =  -5.295078 
#HQ  =  -5.36379
var2_ref=refVAR(var2)
#det(SSE) =  0.004084275 
#AIC =  -5.430023 
#BIC =  -5.343812 
#HQ  =  -5.395346

var3=VAR(hl_nq_covid_pn,p=2)
#det(SSE) =  0.0007802694 
#AIC =  -6.967636 
#BIC =  -6.737739 
#HQ  =  -6.875165
var3_ref=refVAR(var3)
#det(SSE) =  0.0007900763 
#AIC =  -7.049263 
#BIC =  -6.934315 
#HQ  =  -7.003028 

var4=VAR(g_sp500_covid_pn,p=1)
#det(SSE) =  0.004893277 
#AIC =  -5.225775 
#BIC =  -5.110827 
#HQ  =  -5.17954 
var4_ref=refVAR(var4,thres = 1.645) ##Warning, many zeros in the estimation
#det(SSE) =  0.004954314 
#AIC =  -5.260438 
#BIC =  -5.202964 
#HQ  =  -5.23732 

var5=VAR(hl_nq_covid_gc,p=2)
#det(SSE) =  0.0003449612 
#AIC =  -7.783843 
#BIC =  -7.553947 
#HQ  =  -7.691373
var5_ref=refVAR(var5,thres = 1.645)##Warning, many zeros in the estimation
#det(SSE) =  0.0003628519 
#AIC =  -7.850928 
#BIC =  -7.764716 
#HQ  =  -7.816251


var6=VAR(g_pharm_covid_pn,p=2)
#det(SSE) =  0.001651344 
#AIC =  -6.21793 
#BIC =  -5.988034 
#HQ  =  -6.12546
var6_ref=refVAR(var6,thres = 1.645) #**ar(2) is not zero
#det(SSE) =  0.001736301 
#AIC =  -6.238351 
#BIC =  -6.094666 
#HQ  =  -6.180557


var7=VAR(hl_pharm_covid_gc,p=2)
#det(SSE) =  0.0002033974 
#AIC =  -8.312114 
#BIC =  -8.082217 
#HQ  =  -8.219643
var7_ref=refVAR(var7,thres = 1.645)#all zeros

var8=VAR(hl_pharm_covid_pn,p=2)
#det(SSE) =  0.0004102169 
#AIC =  -7.610589 
#BIC =  -7.380693 
#HQ  =  -7.518118 
var8_ref=refVAR(var8,thres = 1.645)#all zeros
#det(SSE) =  0.0004195719 
#AIC =  -7.682158 
#BIC =  -7.56721 
#HQ  =  -7.635922


######################################## VARMA #################################
#VARMA Order Selection based on cross-correlation matrix
#Order of VAR: when q=0(column 1),ith p-value is larger than alpha(0.05), p=i
#Order of VAR: when p=0(row 1),jth p-value is larger than alpha(0.05), q=j 
#Order of VARMA: (ith,jth) p-value is larger than alpha(0.05), p=i and q=j [p!=1, q!=0]
Eccm(g_vix_covid_pn,maxp = 6,maxq = 12)  #1:VAR(2),VMA(11),VARMA(2,2)
Eccm(g_nq_covid_pn,maxp = 6,maxq = 6)    #2:VAR(1),VMA(1),VARMA(1,1)
Eccm(hl_nq_covid_pn,maxp = 3,maxq = 3)   #3:VAR(2),VMA(2),VARMA(1,1)
Eccm(g_sp500_covid_pn,maxp = 3,maxq = 3) #4:VAR(1),VMA(1),VARMA(1,1)
Eccm(hl_nq_covid_gc,maxp = 3,maxq = 3)   #5:VAR(2),VMA(3),VARMA(2,1),VARMA(1,3)
Eccm(g_pharm_covid_pn,maxp = 3,maxq = 3) #6:VAR(2),VMA(3),VARMA(1,2),VARMA(2,1),VARMA(2,2)
Eccm(hl_pharm_covid_gc,maxp = 3,maxq = 3)#7:VAR(2),VMA(3),VARMA(1,2),VARMA(2,1),VARMA(2,2)
Eccm(hl_pharm_covid_pn,maxp = 3,maxq = 3)#8:VAR(1),VMA(1),VARMA(1,1)

#varma1=VARMA(g_vix_covid_pn,p=2,q=2) #na produced
varma1=VARMA(g_vix_covid_pn,p=1,q=1) #two of ten significant, aic=  -4.892256 bic=  -4.604885 
varma2=VARMA(g_nq_covid_pn,p=1,q=1) #na produced
varma3=VARMA(hl_nq_covid_pn,p=1,q=1) #two of ten significant, aic=  -6.829794 bic=  -6.542423 
varma4=VARMA(g_sp500_covid_pn,p=1,q=1) #three of ten significant, aic=  -5.500335 bic=  -5.212965
varma5=VARMA(hl_nq_covid_gc,p=1,q=1) #three of ten significant,zero after ref, aic=  -7.771029 bic=  -7.483658 
varma5=VARMA(hl_nq_covid_gc,p=2,q=1) #four of ten significant, aic=  -7.778827 bic=  -7.376508 
#varma5=VARMA(hl_nq_covid_gc,p=1,q=3) #na
#varma6=VARMA(g_pharm_covid_pn,p=1,q=2)#na
varma6=VARMA(g_pharm_covid_pn,p=2,q=1)#ten of fourteen significant, aic=  -6.282753 bic=  -5.880434
#varma6=VARMA(g_pharm_covid_pn,p=2,q=2)#seven of eighteen significant, aic=  -6.311665 bic=  -5.794398 
varma7=VARMA(hl_pharm_covid_gc,p=1,q=2) #seven of eighteen significant, aic=  -8.628096 bic=  -8.110829
varma7=VARMA(hl_pharm_covid_gc,p=2,q=2) #nine of eighteen significant, aic=  -8.628096 bic=  -8.110829
varma8=VARMA(hl_pharm_covid_pn,p=2,q=2) #na

varma1_ref=refVARMA(varma1,thres = 1.96)  #two of three significant, aic=  -4.940224 bic=  -4.854013
#varma1_ref=refVARMA(varma1,thres = 1.645) #same results as 95%

varma2_ref=refVARMA(varma2,thres = 1.96)  #two of six significant, aic=  -5.853256 bic=  -5.680833
#varma2_ref=refVARMA(varma2,thres = 1.645) #same results as 95%

varma3_ref=refVARMA(varma3,thres = 1.96)  #all two significant, aic=  -6.942976 bic=  -6.885501
#varma3_ref=refVARMA(varma3,thres = 1.645) #same results as 95%

varma4_ref=refVARMA(varma4,thres = 1.96)  #two of three significant, aic=  -5.633812 bic=  -5.547601
#varma4_ref=refVARMA(varma4,thres = 1.645) #same results as 95%

varma5_ref=refVARMA(varma5,thres = 1.96)  #four of five significant, aic=  -7.710597 bic=  -7.566912 
#varma5_ref=refVARMA(varma5,thres = 1.645) #same results as 95%

varma6_ref=refVARMA(varma6,thres = 1.96) #na

varma7_ref=refVARMA(varma7,thres = 1.96) #na

varma8_ref=refVARMA(varma8,thres = 1.96) #na 

######################################## Model Checking ###########################
#Use multivariate Portmanteau Statistics - Residual Cross-Correlations
#If fail to reject H0, therefore, H0:all cross-correlations are zero
#adj The adjustment for degrees of freedom of Ljung-Box statistics. 
#Typically, the number of fitted coefficients of the model. Default is zero.
#Page72, since we use p, the Q2(m) statistics requires m>q to have positive degrees of freedom for asymptotic chi-square distribution
#Page71, 

#k is the number of variables

######################################## Model Checking-VAR #######################
resid_var1_ref=var1_ref$residuals #VAR(p=1),k=2
mq(resid_var1_ref,adj=4) #adj=p*k^2=1*2^2=4
resid_var1=var1$residuals #VAR(p=1),k=2
mq(resid_var1,adj=4) #adj=p*k^2=1*2^2=4


resid_var2_ref=var2_ref$residuals #VAR(p=1),k=2
mq(resid_var2_ref,adj=4) #adj=p*k^2=1*2^2=4
resid_var2=var2$residuals #VAR(p=1),k=2
mq(resid_var2,adj=4) #adj=p*k^2=1*2^2=4

#best result
resid_var3_ref=var3_ref$residuals #VAR(p=2),k=2
mq(resid_var3_ref,adj=8) #adj=p*k^2=2*2^2=8
resid_var3=var3$residuals #VAR(p=2),k=2
mq(resid_var3,adj=8) #adj=p*k^2=2*2^2=8


resid_var4_ref=var4_ref$residuals #VAR(p=1),k=2
mq(resid_var4_ref,adj=4) #adj=p*k^2=1*2^2=4
resid_var4=var4$residuals #VAR(p=1),k=2
mq(resid_var4,adj=4) #adj=p*k^2=1*2^2=4

#good result
resid_var6_ref=var6_ref$residuals#VAR(p=2),k=2
mq(resid_var6_ref,adj=8)#adj=p*k^2=2*2^2=8 
shapiro.test(var6_ref$residuals) #not normally distributed #sample size?
bartlett.test(list(g_pharm_covid_pn[,1],g_pharm_covid_pn[,2])) #not same variance

acf2(var6_ref$residuals[,1],type="correlation",main = "ACF of Residuals, Growth Rate of Pharmaceutical Index")
acf2(var6_ref$residuals[,2],type="correlation",main = "ACF of Residuals, Percentage of Positive Test")
ccf2(var6_ref$residuals[,1],var6_ref$residuals[,2], main = "CCF of Residuals, Y and X")

?acf2
#new test
library(vars)
var6_new=VAR(g_pharm_covid_pn,p=2)
var6_new$varresult
serial.test(var6_new, lags.pt=10, type='PT.asymptotic')

#test causality
causality(var6_new,vcov.=vcovHC(var6_new)) #fail to reject both h0's
#Granger causality H0: g_pharm do not Granger-cause covid_pn
#H0: No instantaneous causality between: g_pharm and covid_pn
#need to define new variable to run the causality test
covid_pn_g_pharm=cbind(covid_pn, g_pharm)
var6_new_=VAR(covid_pn_g_pharm,p=2,type="const") 
var6_new_$varresult
causality(var6_new_,vcov.=vcovHC(var6_new_))

#another way to test causality
library(lmtest)
grangertest(covid_pn,g_pharm,order=2)
grangertest(covid_pn,g_pharm,order=3)
grangertest(covid_pn,g_pharm,order=10)
######################################## Model Checking-VMA #######################
#good result
resid_vma1_ref_=vma1_ref_$residuals #VMA(q=1),k=2
mq(resid_vma1_ref_,adj=4) #adj=q*k^2=1*2^2=4
resid_vma1=vma1$residuals #VMA(q=1),k=2
mq(resid_vma1,adj=4)

#good result
resid_vma2_ref_=vma2_ref_$residuals #VMA(q=2),k=2
mq(resid_vma2_ref_,adj=8) #adj=q*k^2=2*2^2=8
resid_vma2=vma2$residuals #VMA(q=2),k=2
mq(resid_vma2,adj=8) #adj=q*k^2=2*2^2=8

#vma2:g_nq_covid_pn
acf2(vma2_ref_$residuals[,1],main = "ACF of Residuals- Growth Rate of Nasdaq Composite")
acf2(vma2_ref_$residuals[,2],main = "ACF of Residuals- Percentage of Positive Test")
ccf2(vma2_ref_$residuals[,1],vma2_ref_$residuals[,2],main="CCF between Y and X residuals")

#best result
resid_vma3_ref_=vma3_ref_$residuals #VMA(q=1),k=2
mq(resid_vma3_ref_,adj=4) #adj=q*k^2=1*2^2=4
resid_vma3=vma3$residuals #VMA(q=1),k=2
mq(resid_vma3,adj=4) #adj=q*k^2=1*2^2=4


#all #4 models have terrible results
resid_evma4_ref=evma4_ref$residuals #VMA(q=1),k=2
mq(resid_evma4_ref,adj=4) #adj=q*k^2=1*2^2=4


#singular: U[2,2] = 0
#resid_vma4=vma4$residuals #VMA(q=3),k=2
#mq(resid_vma4,adj=12) #adj=q*k^2=3*2^2=12

#not so good
resid_vma6_ref_=vma6_ref_$residuals #VMA(q=2),k=2
mq(resid_vma6_ref_,adj=8) #adj=q*k^2=2*2^2=8

#best results
resid_evma8_ref_=evma8_ref_$residuals#VMA(q=2),k=2
mq(resid_evma8_ref_,adj=8)#adj=q*k^2=2*2^2=8

acf2(evma8_ref_$residuals[,1],main = "ACF of Residuals- Difference between Highest and Lowest Price, Pharmaceutical")
acf2(evma8_ref_$residuals[,2],main = "ACF of Residuals- Percentage of Positive Test")
ccf2(evma8_ref_$residuals[,1],evma8_ref_$residuals[,2],main="CCF between Y and X residuals")


#another way to test causality
library(lmtest)
#VMA2
grangertest(covid_pn,g_nq,order=2) #p-value=0.066

#VMA8 
grangertest(covid_pn,hl_pharm,order=3) #p-value=0.82

######################################## Model Checking-VARMA #######################
#best results
resid_varma1_ref=varma1_ref$residuals #varma1_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma1_ref,adj=8) #adj=k^2*(p+q)=8 #Fail to reject H0
resid_varma1=varma1$residuals #varma1_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma1,adj=8) #adj=k^2*(p+q)=8 #Fail to reject H0

#good results
resid_varma2_ref=varma2_ref$residuals #varma2_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma2_ref,adj=8) #adj=k^2*(p+q)=8 
resid_varma2=varma2$residuals #varma2_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma2,adj=8) #adj=k^2*(p+q)=8 

#good results
resid_varma3_ref=varma3_ref$residuals #varma3_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma3_ref,adj=8) #adj=k^2*(p+q)=8 
resid_varma3=varma3$residuals #varma3_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma3,adj=8) #adj=k^2*(p+q)=8

#best results
resid_varma4_ref=varma4_ref$residuals #varma4_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma4_ref,adj=8) #adj=k^2*(p+q)=8 
resid_varma4=varma4$residuals #varma4_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma4,adj=8) #adj=k^2*(p+q)=8

#not good
resid_varma5_ref=varma5_ref$residuals #varma5_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma5_ref,adj=8) #adj=k^2*(p+q)=8 
#good but zero coefficients
resid_varma5=varma5$residuals #varma9_ref=VARMA(p=1,q=1,k=2)
mq(resid_varma5,adj=8) #adj=k^2*(p+q)=8

#terrible
resid_varma6_ref=varma6_ref$residuals #varma6_ref=VARMA(p=2,q=1,k=2)
mq(resid_varma6_ref,adj=12) #adj=k^2*(p+q)=2^2*3=12 

#both are good
resid_varma8=varma8$residuals #varma6_ref=VARMA(p=2,q=2,k=2)
mq(resid_varma8,adj=16) #adj=k^2*(p+q)=2^2*4=16 
resid_varma8_ref=varma8_ref$residuals #varma6_ref=VARMA(p=2,q=2,k=2)
mq(resid_varma8_ref,adj=16) #adj=k^2*(p+q)=2^2*4=16 
library(astsa)
acf2(varma8_ref$residuals[,1],main = "ACF of Residuals- Difference between Highest and Lowest Price, Pharmaceutical")
acf2(varma8_ref$residuals[,2],main = "ACF of Residuals- Percentage of Positive Test")
ccf2(varma8_ref$residuals[,1],varma8_ref$residuals[,2],main="CCF between Y and X residuals",max.lag = 10)


#another way to test causality
library(lmtest)
#Varma
grangertest(covid_pn,g_nq,order=2) #p-value=0.066


hl_pharm_covid_pn














######################################## Plot results ######################################
library(graphics)
ts.plot(g_pharm,covid_pn)
