#===============================================================================
# Step 1: Import the data-------------------------------------------------------
#===============================================================================
  # 1.1)------------------------------------------------------------------------
  # clears all data and plots from your current Global Environment
rm(list=ls())
cat("\014")  

  # packages
library('ggplot2')
library('tidyverse')
library('fitdistrplus')
library('lme4') 
library('flexplot')
library('zoom')
library('sjPlot')
library('predictmeans')
library('BayesFactor')

  # working directory
setwd("/Users/marcu/OneDrive/Desktop/Uni/Dissertation/code")

  # read-in data
df <- get(load('killersfull(Marcus).Rdata'))
rm(killersfull)
get(load('dates.Rdata'))
df <- cbind(df,dates)
rm(dates)
#===============================================================================
# Step 2: Exploration ----------------------------------------------------------
#===============================================================================
  # 2.1) Imputation -------------------------------------------------------------

df$YearOfDeath[500] <- median(df$YearOfDeath)
df$ConfessedKills[1758] <- median(df$ConfessedKills)

df$Race <-ifelse(df$Race=='White','White',
               ifelse(df$Race=='Black','Black',NA))
df$Race <- as.factor(df$Race)

df$Motive <- as.factor(df$Motive)
#===============================================================================
# step 3: Modelling ------------------------------------------------------------
#===============================================================================
  # 3.1) Linear regression -----------------------------------------------------
  # Y = AgeFirstKill, X = Sex

    # checking assumptions
plot(fitdist(as.vector(na.omit(df$AgeFirstKill)),"norm", 
             discrete=F),breaks=50)

    # model 1 
AgeFirstKill_model_1 <- lm(AgeFirstKill ~ 1, data = df)

    # model 2
AgeFirstKill_model_2 <- lm(AgeFirstKill ~ Sex, 
                              data = df) 

    # summary
summary(AgeFirstKill_model_1)
tab_model(AgeFirstKill_model_1)
summary(AgeFirstKill_model_2)
tab_model(AgeFirstKill_model_2)

    # residual assumption checking
par(mfrow = c(1,1))
par(mar = c(4, 4, 4, 4))
plot(fitdist(AgeFirstKill_model_2$residuals,"norm", 
             discrete=F),breaks=30)

      # Independence assumption checking, cluster=Motive
      # plot
par(mfrow = c(1,1))
par(mar = c(4, 4, 4, 20))
plot(as.numeric(df$Motive)~df$AgeFirstKill,xlab='AgeFirstKill',ylab='Motive',
     pch=19,col= df$Motive,yaxt='n')
axis(4, at = 1:18, labels = paste(1:18,levels(df$Motive)), las = 2)
abline(v = 27.64, lty = 2)
axis(3, at =27.64, labels = expression(paste(widehat(beta)[0], " = 27.64")))


  # 3.2) Variance components model ---------------------------------------------
    # Y=AgeFirstKill    Cluster=Motive

    # model 3
AgeFirstKill_model_3 <- lmer(AgeFirstKill ~ 1 + (1 | Motive),
                               data = df, REML=F) 
summary(AgeFirstKill_model_3)  
tab_model(AgeFirstKill_model_3)

    # checking assumptions
randomeffects <- ranef(AgeFirstKill_model_3)
u <- randomeffects[["Motive"]][["(Intercept)"]]

par(mfrow = c(1,2))
par(mar = c(4, 4, 4, 4))
qqnorm(resid(AgeFirstKill_model_3))
qqline(resid(AgeFirstKill_model_3), col = 2,lwd=2,lty=1)
qqnorm(u)
qqline(u, col = 2,lwd=2,lty=1)

   
    # plot 2
par(mfrow = c(1,1))
par(mar = c(4, 4, 4, 20))
plot(as.numeric(df$Motive)~df$AgeFirstKill,xlab='AgeFirstKill',ylab='Motive',
     pch=19,col= df$Motive,yaxt='n')
axis(4, at = 1:18, labels = paste(1:18,levels(df$Motive)), las = 2)
abline(v = 27.87, lty = 2)
axis(3, at =27.87, labels = expression(paste(widehat(beta)[0], " = 27.87")))
points(27.87 + u, 1:18, pch = "|", cex=2)
     # zoomed in plot
#zm()

  # 3.3) Random Intercepts model------------------------------------------------
rm(list=setdiff(ls(), "df"))
dev.off()

    # model
baseline = lmer(AgeFirstKill~1 +(1|Motive), data=df)
icc(baseline)

fixed_slopes = lmer(AgeFirstKill~Sex +(1|Motive), data=df,REML=F)
icc(fixed_slopes)
summary(fixed_slopes)
tab_model(fixed_slopes)

    # assumption checking
randomeffects <- ranef(fixed_slopes)
u <- randomeffects[["Motive"]][["(Intercept)"]]

par(mfrow = c(1,2))
par(mar = c(4, 4, 4, 4))
qqnorm(resid(fixed_slopes))
qqline(resid(fixed_slopes), col = 2,lwd=2,lty=1)
qqnorm(u)
qqline(u, col = 2,lwd=2,lty=1)

    # visualise
compare.fits(AgeFirstKill~Sex | Motive, data=df, fixed_slopes, baseline,
             clusters=2)

    # model 2 (Sex is too imbalanced for VCM so let's try Race)
baseline = lmer(AgeFirstKill~1 +(1|Motive), data=df)
fixed_slopes = lmer(AgeFirstKill~Race +(1|Motive), data=df,REML=F)
summary(fixed_slopes)
tab_model(fixed_slopes)

# assumption checking
randomeffects <- ranef(fixed_slopes)
u <- randomeffects[["Motive"]][["(Intercept)"]]

par(mfrow = c(1,2))
par(mar = c(4, 4, 4, 4))
qqnorm(resid(fixed_slopes))
qqline(resid(fixed_slopes), col = 2,lwd=2,lty=1)
qqnorm(u)
qqline(u, col = 2,lwd=2,lty=1)

# visualise
compare.fits(AgeFirstKill~Race | Motive, data=df, fixed_slopes, baseline,
             clusters=2)


  # 3.4) Random Slopes model----------------------------------------------------
rm(list=setdiff(ls(), "df"))
dev.off()

    # model
baseline = lmer(AgeFirstKill~1 +(1|Motive), data=df, REML=F)
fixed_slopes = lmer(AgeFirstKill~Race +(1|Motive), data=df,REML=F)
random_slopes = lmer(AgeFirstKill~Race +(Race|Motive), data=df,REML=F)
summary(random_slopes)
icc(baseline)
icc(fixed_slopes)
icc(random_slopes)

model.comparison(baseline,fixed_slopes)
model.comparison(baseline,random_slopes)
model.comparison(fixed_slopes,random_slopes)

    # assumption checking
randomeffects <- ranef(random_slopes)
u0 <- randomeffects[["Motive"]][["(Intercept)"]]
u1 <- randomeffects[["Motive"]][["RaceWhite"]]

par(mfrow = c(2,2))
par(mar = c(4, 4, 4, 4))
qqnorm(resid(random_slopes),main="")
qqline(resid(random_slopes), col = 2,lwd=2,lty=1)
qqnorm(u0,main="")
qqline(u0, col = 2,lwd=2,lty=1)
qqnorm(u1,main="")
qqline(u1, col = 2,lwd=2,lty=1)

# visualise
compare.fits(AgeFirstKill~Race | Motive, data=df, fixed_slopes, random_slopes,
            clusters=5)

#===============================================================================
# step 4: Model Comparison------------------------------------------------------
#===============================================================================
  # 4.1) Models ----------------------------------------------------------------
rm(list=setdiff(ls(), "df"))
dev.off()
    # generalized linear models used
GLM_Base <- lm(AgeFirstKill ~ 1, data = df)
GLM_Sex <- lm(AgeFirstKill ~ Sex, data = df)
GLM_Race <- lm(AgeFirstKill ~ Race, data = df)

    # multilevel models for no inputs
baseline<- lmer(AgeFirstKill ~ 1 + (1 | Motive),data = df, REML=F)

    # multilevel models for Sex clustered on motive
fixed_slopes_Sex <- lmer(AgeFirstKill ~ Sex + (1 | Motive),data = df, REML=F)
random_slopes_Sex <- lmer(AgeFirstKill ~ Sex + (1+Sex | Motive),data = df, REML=F)

    # multilevel models for Race clustered on motive
fixed_slopes_Race<- lmer(AgeFirstKill ~ Race + (1 | Motive),data = df, REML=F)
random_slopes_Race<- lmer(AgeFirstKill ~ Race + (1+Race|Motive),data = df, REML=F)

  # 4.2) Likelihood ratio test -------------------------------------------------
    # function that outputs likelihood ratio test statistics
LRT <- function(A,B){
  Likelihood_A <- logLik(A)
  Likelihood_B <- logLik(B)
  Deviance_Stat <- 2*(Likelihood_B-Likelihood_A)
  return(paste(c("loglik(modelA)=","loglik(modelB)=", "Deviance stat=")
               ,c(Likelihood_A,Likelihood_B,Deviance_Stat)))
}

    # applying the LRT function
LRT(GLM_Base,baseline)
LRT(GLM_Sex, fixed_slopes_Sex)
LRT(GLM_Race,fixed_slopes_Race)
LRT(GLM_Race,random_slopes_Race)
LRT(fixed_slopes_Race,random_slopes_Race)

    # quantile thresholds used
qchisq(p=0.975,df=1)
qchisq(p=0.975,df=2)


  # 4.3) Further Model comparison metrics---------------------------------------
    # outputs AIC ,BIC and ICC where applicable
FurtherMetrics <- function(model){
  if (typeof(model)=="S4"){
    return(paste(c("AIC=","BIC=","ICC="),c(AIC(model),BIC(model),
                                           icc(model)$icc)))
  }
  else{
    return(paste(c("AIC=","BIC="),c(AIC(model),BIC(model))))
  }
}
    # outputs function above for all cases
FurtherMetrics(GLM_Base)
FurtherMetrics(baseline)
FurtherMetrics(GLM_Sex)
FurtherMetrics(fixed_slopes_Sex)
FurtherMetrics(random_slopes_Sex)
FurtherMetrics(GLM_Race)
FurtherMetrics(fixed_slopes_Race)
FurtherMetrics(random_slopes_Race)



