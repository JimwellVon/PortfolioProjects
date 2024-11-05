rm(list=ls())
library(dplyr)
library(tidyverse)
library(car)
library(zoo)
library(knitr)
library(broom)
library(lmtest)


# Upload the Census file#
data2021<-read.csv(file = "Census2021.csv")  


#Specified the columns that are going to be used for the analysis#
dataT<- data2021[ ,c("ABOID","AGEGRP","BFNMEMB",
                     "BEDRM","CMA","DPGRSUM","TotInc"
                     ,"MARSTH","Gender","REGIND","HDGREE"
                     ,"SUBSIDY","HHSIZE","REPAIR","LFACT"
                     ,"MODE","DTYPE","HCORENEED_IND","TENUR")]

nrow(data2021)
View(dataT)

#Created a subset of the date#
YVRSub<-subset(dataT, CMA == 933 & DPGRSUM==14 & AGEGRP >=7 
               & AGEGRP <= 15 & TotInc >= 100 & HDGREE <=13)

names(YVRSub)
nrow(YVRSub)
attach(YVRSub)

#Created binary values for the following#

#Marital Status#
YVRSub$Single<-ifelse(MARSTH==1,1,0)
YVRSub$Married<-ifelse(MARSTH==2,1,0)
YVRSub$CommonLaw<-ifelse(MARSTH==3,1,0)
YVRSub$Separated<-ifelse(MARSTH==4,1,0)
YVRSub$Divorced<-ifelse(MARSTH==5,1,0)
YVRSub$Widowed<-ifelse(MARSTH==6,1,0)

#Gender#
YVRSub$Female<-ifelse(Gender==1,1,0)
YVRSub$Male<-ifelse(Gender==2,1,0)

#Aboriginal Status#
YVRSub$FirstNations<-ifelse(ABOID==1,1,0)
YVRSub$Metis<-ifelse(ABOID==2,1,0)
YVRSub$Inuit<-ifelse(ABOID==3,1,0)
YVRSub$MultiAboriginal<-ifelse(ABOID==4,1,0)

#Band Membership indicator#
YVRSub$Nonbandmember<-ifelse(BFNMEMB==0,1,0)
YVRSub$bandmember<-ifelse(BFNMEMB==1,1,0)

#Registered indian indicator#
YVRSub$NonRegIndian<-ifelse(REGIND==0,1,0)
YVRSub$RegIndian<-ifelse(REGIND==1,1,0)


#Education#
YVRSub$Noschooling<-ifelse(YVRSub$HDGREE==1,1,0)
YVRSub$HighSchool<-ifelse(YVRSub$HDGREE==2,1,0)
YVRSub$Certificate<-ifelse(YVRSub$HDGREE==3| YVRSub$HDGREE == 4,1,0)
YVRSub$College<-ifelse(YVRSub$HDGREE==5| YVRSub$HDGREE==6| 
                         YVRSub$HDGREE == 7,1,0)
YVRSub$University<-ifelse(YVRSub$HDGREE==8| YVRSub$HDGREE==9,1,0) 
YVRSub$Masters<-ifelse(YVRSub$HDGREE==10| YVRSub$HDGREE==12,1,0)
YVRSub$PHD<-ifelse(YVRSub$HDGREE==11| YVRSub$HDGREE==13,1,0)


#Subsidized housing indicator#
YVRSub$Subsidized<-ifelse(SUBSIDY==1,1,0)
YVRSub$NonSubsidized<-ifelse(SUBSIDY==0,1,0)


#Age Group#
YVRSub$Age18to19<-ifelse(AGEGRP==7,1,0)
YVRSub$Early20s<-ifelse(AGEGRP==8,1,0)
YVRSub$Late20s<-ifelse(AGEGRP==9,1,0)
YVRSub$Early30s<-ifelse(AGEGRP==10,1,0)
YVRSub$Late30s<-ifelse(AGEGRP==11,1,0)
YVRSub$Early40s<-ifelse(AGEGRP==12,1,0)
YVRSub$Late40s<-ifelse(AGEGRP==13,1,0)
YVRSub$Early50s<-ifelse(AGEGRP==14,1,0)
YVRSub$Late50s<-ifelse(AGEGRP==15,1,0)

#Household size#
YVRSub$HHSIZE1<-ifelse(HHSIZE==1,1,0)
YVRSub$HHSIZE2<-ifelse(HHSIZE==2,1,0)
YVRSub$HHSIZE3<-ifelse(HHSIZE==3,1,0)
YVRSub$HHSIZE4<-ifelse(HHSIZE==4,1,0)
YVRSub$HHSIZE5ormore<-ifelse(HHSIZE==5 |YVRSub$HHSIZE==6| 
                               YVRSub$HHSIZE == 7,1,0)

#House condutuib#
YVRSub$MajorRepair<-ifelse(REPAIR==3,1,0)
YVRSub$MinorRepair<-ifelse(REPAIR==2,1,0)
YVRSub$Regmaintenance<-ifelse(REPAIR==1,1,0)

#Employment#
YVRSub$Employed<-ifelse(LFACT==1| YVRSub$LFACT == 2,1,0)
YVRSub$Unemployed<-ifelse(LFACT>=3| 
                            YVRSub$LFACT <= 10,1,0)

#MOde of transportation#
YVRSub$Bike<-ifelse(MODE==1,1,0)
YVRSub$Car<-ifelse(MODE==2 | YVRSub$MODE==5,1,0)
YVRSub$Motorcycle<-ifelse(MODE==3,1,0)
YVRSub$PublicTransit<-ifelse(MODE==6,1,0)
YVRSub$Walk<-ifelse(MODE==7,1,0)


#Number of beds#
YVRSub$NoBed<-ifelse(BEDRM==0,1,0)
YVRSub$OneBed<-ifelse(BEDRM==1,1,0)
YVRSub$TwoBed<-ifelse(BEDRM==2,1,0)
YVRSub$ThreeBed<-ifelse(BEDRM==3,1,0)
YVRSub$FourBed<-ifelse(BEDRM==4,1,0)
YVRSub$FiveormoreBed<-ifelse(BEDRM==5,1,0)

#Type of house#
YVRSub$SingleDetached<-ifelse(DTYPE==1,1,0)
YVRSub$Apartment<-ifelse(DTYPE==2,1,0)
YVRSub$OtherDwelling<-ifelse(DTYPE==3,1,0)

#Core housing need#
YVRSub$Hnotcoreneed<-ifelse(HCORENEED_IND==1,1,0)
YVRSub$Hcoreneed<-ifelse(HCORENEED_IND==2,1,0)


#Owner or renter indicator#
YVRSub$Owner<-ifelse(TENUR==1,1,0)
YVRSub$RenterorGovtprovision<-ifelse(TENUR==2,1,0)

attach(YVRSub)



#Regression line has been applied# for education

YVREducReg<-lm(HDGREE~Subsidized+Married+CommonLaw+Separated
               +Separated+Divorced+Widowed+Male+RegIndian
               +bandmember+Metis+Inuit+MultiAboriginal+
                 Age18to19+Early20s+Early30s+Late30s+Early40s
               +Late40s+Early50s+Late50s+HHSIZE1+HHSIZE3+HHSIZE4
               +HHSIZE5ormore+MajorRepair+MinorRepair+Employed+
                 NoBed+OneBed+ThreeBed+FourBed+FiveormoreBed+
                Apartment+OtherDwelling+RenterorGovtprovision, data=YVRSub )

#An adjusted version with less variables#
YVREducRegFix<-lm(HDGREE~Subsidized+Male, data=YVRSub )

Sum<-summary(YVREducReg)
Sum

Sumeducfix<-summary(YVREducRegFix)
Sumeducfix

#Run a bptest to check for heteroskedasticity#
cov1<-hccm(YVREducRegFix,type="hc1")
EDUC.HC1<-coeftest(YVREducRegFix, vcov.=cov1)



kable(tidy(EDUC.HC1),caption= "Robust (HC1) standard errors
      in the 'Educ' equation")

EDUC.HC1


#Variance inflation factor has been applied to check for multicollinearity#
bptest(EDUC.HC1)

vif(YVREducReg)

bptest(YVREducReg)



#Regression line has been applied# for Income#


YVRIncReg<-lm(log(TotInc)~Subsidized+Married+CommonLaw+Separated
               +Separated+Divorced+Widowed+Male+RegIndian
               +bandmember+Metis+Inuit+MultiAboriginal+
                 Age18to19+Early20s+Early30s+Late30s+Early40s
               +Late40s+Early50s+Late50s+HHSIZE1+HHSIZE3+HHSIZE4
               +HHSIZE5ormore+MajorRepair+MinorRepair+Employed+
                 NoBed+OneBed+ThreeBed+FourBed+FiveormoreBed+
                 Apartment+OtherDwelling+RenterorGovtprovision, data=YVRSub )




YVRIncRegFix<-lm(log(TotInc)~Subsidized+Male
                 , data=YVRSub )

Sumregincfix<-summary(YVRIncRegFix)
Sumregincfix


Sum2<-summary(YVRIncReg)
Sum2


vif(YVRIncReg)

bptest(YVRIncReg)

#Regression line has been applied# for Employment#

YVREmpReg<-glm(Employed~Subsidized+Married+CommonLaw+Separated
               +Separated+Divorced+Widowed+Male+RegIndian
               +bandmember+Metis+Inuit+MultiAboriginal+
                 Age18to19+Early20s+Early30s+Late30s+Early40s
               +Late40s+Early50s+Late50s+HHSIZE1+HHSIZE3+HHSIZE4
               +HHSIZE5ormore+MajorRepair+MinorRepair+
                 NoBed+OneBed+ThreeBed+FourBed+FiveormoreBed+
                 Apartment+OtherDwelling+RenterorGovtprovision,
                family= binomial(link="probit"),data= YVRSub)

           
sum3<-summary(YVREmpReg)
sum3            

YVREmpRegfix<-glm(Employed~Subsidized+Male,
               family= binomial(link="probit"),data= YVRSub)



sumempfix<-summary(YVREmpRegfix)
sumempfix           



vif(YVREmpReg)


bptest(YVREmpReg)




cov1<-hccm(YVREducReg,type="hc1")
EDUC.HC1<-coeftest(YVREducReg, vcov.=cov1)



kable(tidy(EDUC.HC1),caption= "Robust (HC1) standard errors
      in the 'Educ' equation")


EDUC.HC1

bptest(EDUC.HC1)
