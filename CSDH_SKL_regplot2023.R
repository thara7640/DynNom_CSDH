##################################################################
########################################################
###########3 conventional Nomogram #################
## set data
rm(list=ls())
#AA<- read.csv("D:/MyWork/R/Class8.1 NOMOGRAM visualization/NOMO_BOOK/TERT_MGMT_50_nomo_test2.csv")
Df<- read.csv("D:/My work/R/R_code/R1/class8 nomogram_update/CSDH_SKL/github/CSDH_SKL.csv")
#D:\My work\R\R_code\R1\class8 nomogram1\GBM_deepsurv

## set data
getwd()
#setwd("D:/My work/My research/วิจัยBeer_CT_elderly/Result_beer_26_12_2022")
setwd("D:/My work/R/R_code/R1/class8 nomogram_update/CSDH_SKL/github")
## library
library(data.table)
library(rio)
#library(epicalc)
Df <- setDT(import("CSDH_SKL.csv"))


Df
#AA <- setDT(import("CSDH_SKL.csv"))

#install.packages("regplot")
#library(regplot)


## 2 Survival model for pbc data

summary(Df)
str(Df)

library(survival)
library(regplot)
#############################
Df <- transform(Df, Antiplatelet = factor(Antiplatelet , labels=c("No","Yes")))
Df <- transform(Df, NSS_ml_3200 = factor(NSS_ml_3200, labels=c("<3200ml",">=3200ml")))
Df <- transform(Df, Optime_140 = factor(Optime_140, labels=c("<140min",">=140min")))
Df <- transform(Df, GCS_gr = factor(GCS_gr, labels=c("13-15","9-12","3-8")))


#AA
Df
### Backward method ###
Df <- transform(Df, ASA_PLAVIX = factor(ASA_PLAVIX , labels=c("No","Yes")))
Df <- transform(Df, NSS_ml_3200 = factor(NSS_ml_3200, labels=c("<3200ml",">=3200ml")))
Df <- transform(Df, Optime_140 = factor(Optime_140, labels=c("<140min",">=140min")))
Df <- transform(Df, GCS_gr = factor(GCS_gr, labels=c("13-15","9-12","3-8")))


## cox model 
ddist <- datadist(Df)
options(datadist='ddist')

Df$INDEX<- NULL
Df$HERBAL_USE <- NULL
Df$Complication <- NULL
Df$CT5_symptom <- NULL
Df$CT5_Tx<- NULL
Df$OPERATION_gr <- NULL
Df$BURR <- NULL
Df$date_ <- NULL
Df$CSDH <- NULL
Df$CRAN_REOPICH <- NULL
Df$REOP <- NULL
Df$FUSN <- NULL
Df$LAM <- NULL
Df$VSHN <- NULL
Df$DVT <- NULL
Df$PE <- NULL
Df$DEATH <- NULL
Df$SSI_MM <- NULL
Df$BW_kg<- NULL
Df$SSI_IC<- NULL

Df$Ht_cm<- NULL
Df$Wt_kg<- NULL
Df$INFORM_INCOM<- NULL
Df$INFORM_DETAIL<- NULL
Df$DC_KPS<- NULL
Df$ASA_gr<- NULL
Df$Wt_kg<- NULL


ddist <- datadist(Df)
options(datadist='ddist')
#AA$datelastFU <- NULL
#date4thSx
library(rms)

ddist <- datadist(Df)
options(datadist='ddist')
#mod.cox<- cph(Surv(Dtime,Death) ~ ThaBGBS_GBM+Lepto_gr+Multiple_GBM+Surgery_re+Adjuvant_treatment_re,AA, surv=TRUE)
mod.cox<- cph(Surv(FU_time,RECURR) ~  Antiplatelet+NSS_ml_3200+Optime_140+GCS_gr,Df, surv=TRUE)
mod.cox
summary(mod.cox)
require(coin)
## nomogram 

library(survey)
library(rms)
library (SvyNom)
library(survival)

## survival cox->linear 
surv.cox <- Survival(mod.cox)
surv.cox
nom.cox <- nomogram(mod.cox, fun=list(function(x) surv.cox(30, x),
                                      function(x) surv.cox(60, x),
                                      function(x) surv.cox(90, x)),
                    funlabel=c("1-month Sur. Prob.",
                               "2-month Sur. Prob.",
                               "3-month Sur. Prob."),
                    lp=F)

nom.cox


nom.cox <- nomogram(mod.cox, fun=list(function(x) surv.cox(120, x),
                                      function(x) surv.cox(150, x)),
                    funlabel=c("4-month Sur. Prob.",
                               "5-month Sur. Prob."),
                    lp=F)

nom.cox
plot(nom.cox,
     fun.side=list(c(rep(c(1,3),5),1,1,1),
                   c(1,1,1,rep(c(3,1),3))))

plot(nom.cox,
     fun.side=list(c(rep(c(1,1),6),1,1,1),c(1,1,1,1,rep(c(1,1),5))),
     xfrac=.3,
     col.grid = c("red","green"))
########################################
### NOMOGRAM NEW PLOT 
plot(nom.cox, xfrac=.2, 
     total.points.label="Sum of all points", 
     cex.axis = 1.05,
     force.label = TRUE,
     tcl = 0.8,
     lmgp = 0.1,
     vnames="labels",
     col.grid=gray(c(0.85,0.95)))
############################################
plot(nom.cox, xfrac=.2, 
     total.points.label="Sum of all points", 
     cex.axis = 1.05,
     force.label = TRUE,
     tcl = 0.8,
     lmgp = 0.1,
     vnames="labels",
     col.grid = c("red","blue"))

##############################
library(gridGraphics)
library(grid)
plot(nom.cox, xfrac=.2, 
     total.points.label="Sum of all points", 
     cex.axis = 1.05,
     force.label = TRUE,
     tcl = 0.8,
     lmgp = 0.1,
     vnames="labels",
     col.grid=gray(c(0.85,0.95)))

grid.echo()
a <- grid.grab()
a$children$`graphics-plot-1-text-1`$gp$col <- "red"
  a$children$`graphics-plot-1-text-2`$gp$col <- "green"
    a$children$`graphics-plot-1-text-7`$gp$col <- "blue"
      
    grid.draw(a)
    ####################################################################
    #SAVE 
    
    tiff(filename = "Nomogram4yr.tif",width = 10, height = 8, units = "in",res=600,compression = "lzw")
    plot(nom.cox, xfrac=.2, 
         total.points.label="Sum of all points", 
         cex.axis = 1.05,
         force.label = TRUE,
         tcl = 0.8,
         lmgp = 0.1,
         vnames="labels",
         col.grid = c("red","blue"))
    dev.off()

    ###########################################
    ###########################################
    #############################################
    ###############################################################
    ############################ Buliding app ##################
    #########DynNoM#####
    library(DynNom)
    # Binary logistic model
    #fit2 <- glm(Positive_finding_on_CT ~ Age_5yrs+Road_traffic_injury+LOC+Motor_weakness+Scalp_injury+Bleeding_per_nose_ear+Pupillary_reflex+GCS  ,data=AA,family=binomial())
    
    # Cox model 
    #fit2 <- cph(Surv(time, Death) ~Number_of_tumor+Eloquent+Extent_of_resection+Adjuvant_treatment+MGMT , data=AA) 
    mod.cox<- cph(Surv(FU_time,RECURR) ~  Antiplatelet+NSS_ml_3200+Optime_140+GCS_gr,Df, surv=TRUE)
    mod.cox
    summary(mod.cox)
    DynNom(mod.cox, Df)
    DNbuilder(mod.cox, Df)
    
###############################################################################    
    # cox model
    fit2 <- cph(Surv(Dtime,Death) ~  Surgery+CCRT+Combined_biomarkers  ,data=AA)
    
    fit2
    summary(fit2)
    ### 
    DNbuilder(fit2, AA)
    ##############################
    ###
    DynNom(fit2, AA)
###########################################################################
    ###
    
    