if (!(require(survminer))) install.packages ("survminer")
if (!(require(survival))) install.packages ("survival")
if (!(require(stargazer))) install.packages ("stargazer")
if (!(require(dplyr))) install.packages ("dplyr")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(ggplot2))) install.packages ("ggplot2")
if (!(require(reshape))) install.packages ("reshape")

# load SMM/CPR Calculator function
setwd("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV")

source("SMM.R")


gc()
options(scipen=999)
options(digits=5)

load("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV/FNMA.Rda")
#FNMA <-subset(FNMA,MONTH>="Dez 2010")



#1. CONTINOUS MODEL

#1.1. Survival Curve: Kaplan Meier
fit.null <- survfit(Surv(START,STOP,PREPAID)~1, data=FNMA)
ggsurv.NULL <- ggsurvplot(fit.null, risk.table = TRUE,  palette=c("#86BC25"), censor=FALSE, conf.int = TRUE, xlab="Month", 
                          ggtheme=theme_bw(), legend=c("none"),break.x.by = 10,break.y.by = 0.1)
ggsurv.NULL
ggsave(file = "surv_null.jpeg", print(ggsurv.NULL))
#1.2. CPR
SMM.null<- SMM(fit.null)
test <- FNMA %>%
  group_by(MONTH) %>%
  dplyr::summarize(MEANS.YSPREAD = mean(YSPREAD))
test$MEANS.YSPREAD<- test$MEANS.YSPREAD/100
test = test[-1,]
SMM.null <- cbind(SMM.null,test)
SMM.null <- subset(SMM.null, select=c("MONTH","SMM","MEANS.YSPREAD"))
SMM.null <- melt(SMM.null, id="MONTH")

ggplot(SMM.null, aes(x=MONTH, y=value, group=variable)) +
  geom_line(aes(color=variable),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(labels = scales::percent,breaks = seq(-0.01, 0.04, by = 0.005))+
  theme(legend.position = c(0.8, 0.85)) +
  scale_color_manual(values = c("#86BC25","#0076A8"),
                     labels = c("SMM","Average Yield Spread"),
                     name="")+
  ylab("SMM and Yield Spread")+
  xlab("Month")
  
ggsave(file = "SMM_null.jpeg")







### Cox-PH Models ###
## continous model suggests that all analysed variable are strongly significant. Tough, variables like DTI and CSCORE seem not to have a strong impact tough.
FINAL_cont1<-coxph(Surv(START,STOP,PREPAID)~YSPREAD+LTV, data=FNMA)
summary(FINAL_cont1)
extractAIC(FINAL_cont1)
### adding personal-loan information OCC_STAT and VOL
FINAL_cont3<-coxph(Surv(START,STOP,PREPAID)~YSPREAD+LTV+VOL+OCC_STAT+DTI+CSCORE, data=FNMA)
summary(FINAL_cont3)
extractAIC(FINAL_cont3)
zph_cont3 <- cox.zph(FINAL_cont3)
zph_cont3

zphplot_cont3<- ggcoxzph(zph_cont3,var=c("YSPREAD","LTV","DTI"),  resid=FALSE,se=TRUE, title=FALSE, ggtheme=theme_bw(),xlab="Month")
length(zphplot_cont3)
zphplot_cont3[[1]] <- zphplot_cont3[[1]] + labs(title = 'YIELD (p=0.000)') 
zphplot_cont3[[2]] <- zphplot_cont3[[2]] + labs(title = 'LTV (p=0.000)') 
zphplot_cont3[[3]] <- zphplot_cont3[[3]] + labs(title = 'DTI (p=0.940)') 
zphplot_cont3
ggsave(file = "zph_cont3.jpeg", print(zphplot_cont3))

#### 
FINAL_cont5<-coxph(Surv(START,STOP,PREPAID)~YSPREAD+LTV+VOL+OCC_STAT+DTI+CSCORE+UNEMP+TAX, data=FNMA)
summary(FINAL_cont5)
extractAIC(FINAL_cont5)
zph_cont5 <- cox.zph(FINAL_cont5)
zph_cont5

zphplot_cont5<- ggcoxzph(zph_cont5,var=c("YSPREAD","LTV","DTI","UNEMP"),  resid=FALSE,se=TRUE, title=FALSE, ggtheme=theme_bw(),xlab="Month")
length(zphplot_cont5)
zphplot_cont5[[1]] <- zphplot_cont5[[1]] + labs(title = 'YIELD (p=0.00)') 
zphplot_cont5[[2]] <- zphplot_cont5[[2]] + labs(title = 'LTV (p=0.000)') 
zphplot_cont5[[3]] <- zphplot_cont5[[3]] + labs(title = 'DTI (p=0.597)') 
zphplot_cont5[[4]] <- zphplot_cont5[[4]] + labs(title = 'UNEMP (p=0.274)') 
zphplot_cont5
ggsave(file = "zphplot_cont5.jpeg", print(zphplot_cont5))



stargazer(FINAL_cont1,FINAL_cont3,FINAL_cont5,ci=TRUE,apply.coef=exp,dep.var.caption="",dep.var.labels.include=FALSE)

## time-depedent model ##
FINAL_time<-coxph(Surv(START,STOP,PREPAID)~YSPREAD.CAT+CSCORE+CSCORE:START+LTV.CAT+LTV.CAT:START+OCC_STAT+OCC_STAT:START+DTI+UNEMP+VOL.CAT+VOL.CAT:START+YSPREAD.CAT:START+UNEMP+TAX, data=FNMA)
summary(FINAL_time)
extractAIC(FINAL_time)
zph_time <- cox.zph(FINAL_time)
zph_time
zphplot_time <- ggcoxzph(zph_time,var=c("YSPREAD.CATITM","LTV.CAT<60","LTV.CAT>80","VOL.CAT>350k"),  resid=FALSE,se=TRUE, title=FALSE, ggtheme=theme_bw(),xlab="Month")
length(zphplot_time)
zphplot_time[[1]] <- zphplot_time[[1]] + labs(title = 'ITM YIELD (p=0.6750)') 
zphplot_time[[2]] <- zphplot_time[[2]] + labs(title = 'LTV<60 (p=0.1067)')
zphplot_time[[3]] <- zphplot_time[[3]] + labs(title = 'LTV>80 (p=0.1710)') 
zphplot_time[[4]] <- zphplot_time[[4]] + labs(title = 'VOL>$350k (p=0.0903)') 
zphplot_time
ggsave(file = "zph_time.jpeg", print(zphplot_time))


stargazer(FINAL_time,ci=TRUE,apply.coef=exp,dep.var.caption="",dep.var.labels.include=FALSE,single.row = TRUE)













##### VOL: Kaplan Meier ####
fit.VOL <- survfit(Surv(START,STOP,PREPAID)~strata(VOL.CAT), data=FNMA)
SMM.VOL<- SMM(fit.VOL)
#CPR plot
ggplot(SMM.VOL, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "Credit Volume",
                     breaks = c(1,2),
                     labels = c("<= 350,000$",">350,000$"),
                     values = c("#86BC25","#0076A8"))
ggsave(file = "CPR_vol.jpeg")

fit.tax <- survfit(Surv(START,STOP,PREPAID)~strata(TAX), data=FNMA)
SMM.tax<- SMM(fit.tax)
#CPR plot
ggplot(SMM.tax, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "NY vs LA:",
                     breaks = c(1,2),
                     labels = c("LA","NY"),
                     values = c("#86BC25","#0076A8"))
ggsave(file = "CPR_tax.jpeg")
#survival curves for tax
ggsurv.tax<- ggsurvplot(fit.tax, risk.table = TRUE,  
                        censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                        break.x.by = 10,break.y.by = 0.1)
ggsurv.tax

##


##### YIELD: Kaplan Meier ####
fit.YIELD <- survfit(Surv(START,STOP,PREPAID)~strata(YSPREAD.CAT), data=FNMA)
SMM.YIELD<- SMM(fit.YIELD)
#CPR plot
ggplot(SMM.YIELD, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "Yield Break (at 0.75%):",
                     breaks = c(1,2),
                     labels = c("Out of Money","In the Money"),
                     values = c("#86BC2
                                
                                5","#0076A8"))
ggsave(file = "CPR_YIELD.jpeg")

#Survival Curve plot
ggsurv.VOL<- ggsurvplot(fit.VOL, risk.table = TRUE,  palette=c("#86BC25","#0076A8"), 
                        censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                        legend=c(0.9, 0.8),legend.labs = c("<= 350,000$",">350,000$"),legend.title="Credit Volume",
                        break.x.by = 10,break.y.by = 0.1)
ggsurv.VOL
ggsave(file = "surv_vol.jpeg", print(ggsurv.VOL))

#survival curves for YIELD
ggsurv.YIELD<- ggsurvplot(fit.YIELD, risk.table = TRUE,  palette=c("#86BC25","#0076A8"), 
                          censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                          legend=c(0.9, 0.8),legend.labs = c("OTM","ITM"),legend.title="YIELD",
                          break.x.by = 10,break.y.by = 0.1)
ggsurv.YIELD
ggsave(file = "surv_YIELD.jpeg", print(ggsurv.YIELD))
##### LTV: Kaplan Meier ####
fit.LTV <- survfit(Surv(START,STOP,PREPAID)~strata(LTV.CAT), data=FNMA)
SMM.LTV<- SMM(fit.LTV)
#CPR plot
ggplot(SMM.LTV, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "LTV",
                     breaks = c(1,2,3),
                     labels = c("<60","60-75",">75"),
                     values = c("#86BC25","#0076A8","#BBBCBC"))
ggsave(file = "CPR_LTV.jpeg")
#Survival Curve plot
ggsurv.LTV<- ggsurvplot(fit.LTV, risk.table = TRUE,  palette=c("#86BC25","#0076A8","#BBBCBC"), 
                        censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                        legend=c(0.9, 0.8),legend.labs = c("<60","60-75",">75"),legend.title="LTV",
                        break.x.by = 10,break.y.by = 0.1)
ggsurv.LTV
ggsave(file = "surv_LTV.jpeg", print(ggsurv.LTV))


##### TAX: Kaplan Meier ####


## For deeper analysis, use a discrete model in which the most important factors were categorzied
FINAL_disc<-coxph(Surv(START,STOP,PREPAID)~YSPREAD.CAT+LTV.CAT+VOL.CAT+OCC_STAT+CSCORE+DTI+UNEMP+YEAR, data=FNMA)
summary(FINAL_disc)
extractAIC(FINAL_disc)
zph_disc <- cox.zph(FINAL_disc)
zph_disc
zphplot_disc <- ggcoxzph(zph_disc,var=c("YSPREAD.CATITM","LTV.CAT>75","VOL.CAT>350k"),  resid=FALSE,se=TRUE, title=FALSE, ggtheme=theme_bw(),xlab="Month")
length(zphplot_disc)
zphplot_disc[[1]] <- zphplot_disc[[1]] + labs(title = 'ITM YIELDs: Burnout (p=0.0002)') 
zphplot_disc[[2]] <- zphplot_disc[[2]] + labs(title = 'LTV>75 (p=0.0006)') 
zphplot_disc[[3]] <- zphplot_disc[[3]] + labs(title = 'VOL.CAT>350k (p=0.0000)') 
ggsave(file = "zph_disc.jpeg", print(zphplot_disc))






## adding time-depended factors. All factors were adjusted with time-depended covariates beta(t)X(t).








