# The same has been done for Edmonton#

rm(list=ls())
data2021<-read.csv(file = "Census2021.csv") 

YEGSub<-subset(data2021,CMA == 835 & DPGRSUM==14 &AGEGRP>=7 & AGEGRP <=15 & TotInc>=100)

 
View(data2021)



dataE<- data2021[ ,c("ABOID","AGEGRP","BFNMEMB",
                     "BEDRM","CMA","DPGRSUM","TotInc"
                     ,"MARSTH","Gender","REGIND","HDGREE"
                     ,"SUBSIDY","HHSIZE","REPAIR","LFACT"
                     ,"MODE","DTYPE","HCORENEED_IND","TENUR")]

nrow(dataE)
View(dataE)



attach(YEGSub)
nrow(YEGSub)


YEGSub$Single<-ifelse(MARSTH==1,1,0)
YEGSub$Married<-ifelse(MARSTH==2,1,0)
YEGSub$CommonLaw<-ifelse(MARSTH==3,1,0)
YEGSub$Separated<-ifelse(MARSTH==4,1,0)
YEGSub$Divorced<-ifelse(MARSTH==5,1,0)
YEGSub$Widowed<-ifelse(MARSTH==6,1,0)

YEGSub$Female<-ifelse(Gender==1,1,0)
YEGSub$Male<-ifelse(Gender==2,1,0)

YEGSub$FirstNations<-ifelse(ABOID==1,1,0)
YEGSub$Metis<-ifelse(ABOID==2,1,0)
YEGSub$Inuit<-ifelse(ABOID==3,1,0)
YEGSub$MultiAboriginal<-ifelse(ABOID==4,1,0)

YEGSub$Nonbandmember<-ifelse(BFNMEMB==0,1,0)
YEGSub$bandmember<-ifelse(BFNMEMB==1,1,0)

YEGSub$NonRegIndian<-ifelse(REGIND==0,1,0)
YEGSub$RegIndian<-ifelse(REGIND==1,1,0)


YEGSub$Noschooling<-ifelse(YEGSub$HDGREE==1,1,0)
YEGSub$HighSchool<-ifelse(YEGSub$HDGREE==2,1,0)
YEGSub$Certificate<-ifelse(YEGSub$HDGREE==3| YEGSub$HDGREE == 4,1,0)
YEGSub$College<-ifelse(YEGSub$HDGREE==5| YEGSub$HDGREE==6| 
                         YEGSub$HDGREE == 7,1,0)
YEGSub$University<-ifelse(YEGSub$HDGREE==8| YEGSub$HDGREE==9,1,0) 
YEGSub$Masters<-ifelse(YEGSub$HDGREE==10| YEGSub$HDGREE==12,1,0)
YEGSub$PHD<-ifelse(YEGSub$HDGREE==11| YEGSub$HDGREE==13,1,0)



YEGSub$Subsidized<-ifelse(SUBSIDY==1,1,0)
YEGSub$NonSubsidized<-ifelse(SUBSIDY==0,1,0)

YEGSub$Age18to19<-ifelse(AGEGRP==7,1,0)
YEGSub$Early20s<-ifelse(AGEGRP==8,1,0)
YEGSub$Late20s<-ifelse(AGEGRP==9,1,0)
YEGSub$Early30s<-ifelse(AGEGRP==10,1,0)
YEGSub$Late30s<-ifelse(AGEGRP==11,1,0)
YEGSub$Early40s<-ifelse(AGEGRP==12,1,0)
YEGSub$Late40s<-ifelse(AGEGRP==13,1,0)
YEGSub$Early50s<-ifelse(AGEGRP==14,1,0)
YEGSub$Late50s<-ifelse(AGEGRP==15,1,0)

YEGSub$HHSIZE1<-ifelse(HHSIZE==1,1,0)
YEGSub$HHSIZE2<-ifelse(HHSIZE==2,1,0)
YEGSub$HHSIZE3<-ifelse(HHSIZE==3,1,0)
YEGSub$HHSIZE4<-ifelse(HHSIZE==4,1,0)
YEGSub$HHSIZE5ormore<-ifelse(HHSIZE==5 |YEGSub$HHSIZE==6| 
                               YEGSub$HHSIZE == 7,1,0)

YEGSub$MajorRepair<-ifelse(REPAIR==3,1,0)
YEGSub$MinorRepair<-ifelse(REPAIR==2,1,0)
YEGSub$Regmaintenance<-ifelse(REPAIR==1,1,0)

YEGSub$Employed<-ifelse(LFACT==1| YEGSub$LFACT == 2,1,0)
YEGSub$Unemployed<-ifelse(LFACT>=3| 
                            YEGSub$LFACT <= 10,1,0)

YEGSub$Bike<-ifelse(MODE==1,1,0)
YEGSub$Car<-ifelse(MODE==2 | YEGSub$MODE==5,1,0)
YEGSub$Motorcycle<-ifelse(MODE==3,1,0)
YEGSub$PublicTransit<-ifelse(MODE==6,1,0)
YEGSub$Walk<-ifelse(MODE==7,1,0)

YEGSub$NoBed<-ifelse(BEDRM==0,1,0)
YEGSub$OneBed<-ifelse(BEDRM==1,1,0)
YEGSub$TwoBed<-ifelse(BEDRM==2,1,0)
YEGSub$ThreeBed<-ifelse(BEDRM==3,1,0)
YEGSub$FourBed<-ifelse(BEDRM==4,1,0)
YEGSub$FiveormoreBed<-ifelse(BEDRM==5,1,0)


YEGSub$SingleDetached<-ifelse(DTYPE==1,1,0)
YEGSub$Apartment<-ifelse(DTYPE==2,1,0)
YEGSub$OtherDwelling<-ifelse(DTYPE==3,1,0)


YEGSub$Hnotcoreneed<-ifelse(HCORENEED_IND==1,1,0)
YEGSub$Hcoreneed<-ifelse(HCORENEED_IND==2,1,0)

YEGSub$Owner<-ifelse(TENUR==1,1,0)
YEGSub$RenterorGovtprovision<-ifelse(TENUR==2,1,0)

attach(YEGSub)





###############################################################



YEGEducReg<-lm(HDGREE~Subsidized+Married+CommonLaw+Separated
               +Separated+Divorced+Widowed+Male+RegIndian
               +bandmember+Metis+Inuit+MultiAboriginal+
                 Age18to19+Early20s+Early30s+Late30s+Early40s
               +Late40s+Early50s+Late50s+HHSIZE1+HHSIZE3+HHSIZE4
               +HHSIZE5ormore+MajorRepair+MinorRepair+Employed+
                 NoBed+OneBed+ThreeBed+FourBed+FiveormoreBed+
                 Apartment+OtherDwelling+RenterorGovtprovision, data=YEGSub )

SumYeg1<-summary(YEGEducReg)
SumYeg1


YEGEducRegFix<-lm(HDGREE~Subsidized+Male, data=YEGSub )


Sumeducfixyeg<-summary(YEGEducRegFix)
Sumeducfixyeg


vif(YEGEducReg)

bptest(YEGEducReg)





YEGIncReg<-lm(log(TotInc)~Subsidized+Married+CommonLaw+Separated
              +Separated+Divorced+Widowed+Male+RegIndian
              +bandmember+Metis+Inuit+MultiAboriginal+
                Age18to19+Early20s+Early30s+Late30s+Early40s
              +Late40s+Early50s+Late50s+HHSIZE1+HHSIZE3+HHSIZE4
              +HHSIZE5ormore+MajorRepair+MinorRepair+Employed+
                NoBed+OneBed+ThreeBed+FourBed+FiveormoreBed+
                Apartment+OtherDwelling+RenterorGovtprovision, data=YEGSub )



Sumyeg2<-summary(YEGIncReg)
Sumyeg2


YEGIncRegFix<-lm(log(TotInc)~Subsidized+Male
                 , data=YEGSub)


SumregincfixYEG<-summary(YEGIncRegFix)
SumregincfixYEG


vif(YEGIncReg)

bptest(YEGIncReg)


YEGEmpReg<-glm(Employed~Subsidized+Married+CommonLaw+Separated
               +Separated+Divorced+Widowed+Male+RegIndian
               +bandmember+Metis+Inuit+MultiAboriginal+
                 Age18to19+Early20s+Early30s+Late30s+Early40s
               +Late40s+Early50s+Late50s+HHSIZE1+HHSIZE3+HHSIZE4
               +HHSIZE5ormore+MajorRepair+MinorRepair+
                 NoBed+OneBed+ThreeBed+FourBed+FiveormoreBed+
                 Apartment+OtherDwelling+RenterorGovtprovision,
               family= binomial(link="probit"),data= YEGSub)


sumyeg3<-summary(YEGEmpReg)
sumyeg3            


YEGEmpRegfix<-glm(Employed~Subsidized+Male,
                  family= binomial(link="probit"),data= YEGSub)


sumempfixYEG<-summary(YEGEmpRegfix)
sumempfixYEG          





vif(YEGEmpReg)

bptest(YEGEmpReg)


