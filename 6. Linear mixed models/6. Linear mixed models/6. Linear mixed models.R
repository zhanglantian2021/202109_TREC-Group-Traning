ls(); rm(list=ls())

pckg <- c('tidyverse','lubridate', 'broom', 'ggpubr', 'glmmTMB', 'performance', 'ciTools', 'sjPlot', 'car', 'R.utils','nlme','bbmle','MuMIn','R.utils','optimx','car','effects', 'lme4', 'data.table', 'lmerTest', 'grid', 'gridExtra', 'gamm4')

for(i in 1:length(pckg)) {
  if (!requireNamespace(pckg[i]))
    install.packages(pckg[i])
}

for(i in 1:length(pckg)){
  library(pckg[i],character.only=T)
}

setwd("F:/TREC Group/TREC Group/Statistics/R_Workshop_202109/6. Linear mixed models")

##Example data RIKZ
RIKZ <- read.delim("RIKZ.txt")
View(RIKZ)
head(RIKZ)

lm0 <-lm(Richness~NAP,data=RIKZ)
summary(lm0)
anova(lm0)
plot(Richness ~ NAP, data=RIKZ)
abline(lm0,col="red")

#################
library(ciTools)
RIKZ<-RIKZ[order(RIKZ$NAP),]
plot(Richness ~ NAP, data=RIKZ)
lm_ci<-add_ci(RIKZ, lm0, alpha = 0.05,names = c("lwr", "upr"))
lines(lm_ci$NAP, lm_ci$pred, col = "red", lwd = 1,lty="solid")
lines(lm_ci$NAP, lm_ci$lwr, col = "red", lwd = 1,lty="dotted")
lines(lm_ci$NAP, lm_ci$upr, col = "red", lwd = 1,lty="dotted")
title("Linear regression model")
text(1.0, 20,"Richness=6.686-2.867*NAP",col="red")
text(1.2, 18, expression(""~R^2~" = 0.324"))

############
par(mfrow = c(2,2))
hist(lm0$residuals, main="Residual distribution")
plot(density(lm0$residuals), main="Residual distribution")
polygon(density(lm0$residuals),col="slategray1")
qqnorm(lm0$residuals, main="Normal Q-Q")
mean(lm0$residuals)
hist(RIKZ$Richness)
dev.off()
####################
hist(RIKZ$Richness)
pm1 <- glm(Richness~NAP, family="poisson", data=RIKZ)
summary(pm1)
anova(pm1)

###########
RIKZ<-RIKZ[order(RIKZ$NAP),]
plot(Richness~NAP,data=RIKZ, pch=19)
ci_pm1<-add_ci(RIKZ, pm1, alpha = 0.05, names = c("lwr", "upr"))
lines(ci_pm1$NAP, ci_pm1$pred, col = "red", lwd = 1,lty="solid")
lines(ci_pm1$NAP, ci_pm1$upr, col = "red", lwd = 1,lty="dotted")
lines(ci_pm1$NAP, ci_pm1$lwr, col = "red", lwd = 1,lty="dotted")
title("glm-poisson regression")
##################
par(mfrow=c(2,2))
###Homogeneity of variances
plot(pm1$residuals ~ pm1$fitted.values, xlab = "Predicted values", ylab = "Residuals", main="predicted v.s residuals")
abline(h=0, col="red")
plot(pm1$fitted.values ~  pm1$model$Richness, xlab="Observed values", ylab="Predicted values", main="observed v.s predicted")
abline(0,1, col="red")
qqnorm(residuals(pm1))###normal distribution of residuals
qqline(residuals(pm1))
hist(pm1$residuals, main="Residual distribution")
abline(v=0, col="red")
dev.off()
####################
ggplot(RIKZ, aes(x=NAP, y=Richness, color=factor(Beach)))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)
####################
library(nlme)
RIKZ$fBeach <- factor(RIKZ$Beach)
lmm_1 <- lme(Richness ~ NAP, random = ~1 | fBeach, method="REML", data=RIKZ)
summary(lmm_1)
RIKZ$fit_InterceptOnly <- predict(lmm_1)

###Plot the 
ggplot(RIKZ, aes(x = NAP, y = Richness, colour = fBeach)) +
  geom_point(size = 3) +
  # Add fixed effect regression line (i.e. NAP)
  geom_abline(aes(intercept = `(Intercept)`, slope = NAP),
              size = 2,
              as.data.frame(t(fixef(lmm_1)))) +
  # Add fitted values (i.e. regression) for each beach
  geom_line(aes(y = fit_InterceptOnly), size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_brewer(palette="Set1")
################
lmm_2 <- lme(Richness ~ NAP,
             random = ~ NAP | fBeach, method="REML", data = RIKZ)
summary(lmm_2)
####
RIKZ$fit_IntSlope <- predict(lmm_2)
ggplot(RIKZ, aes(x = NAP, y = Richness, colour = fBeach)) +
  geom_abline(aes(intercept = `(Intercept)`, slope = NAP),
              size = 2,
              as.data.frame(t(fixef(lmm_2)))) +
  geom_line(aes(y = fit_IntSlope), size = 1) +
  geom_point(size = 3) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_brewer(palette="Set1")
####################
lmm_3 <- lme(Richness ~ 1, random = ~1 | fBeach,
             method="REML", data = RIKZ)
summary(lmm_3)
######Model comparisons
library(bbmle)
AICtab(lmm_1, lmm_2, lmm_3)
##################
lmm_f <- lme(Richness ~ NAP,
            random = ~ NAP | fBeach, method="REML", data = RIKZ)
summary(lmm_f)
anova(lmm_f)

library(MuMIn)
library(sjPlot)
tab_model(lmm_f, show.intercept=T, show.se=T, show.df=T, show.est=TRUE, show.ci=0.95,
          show.aic = T, show.loglik = T, show.obs = T)
r.squaredGLMM(lmm_f)
#############
library(performance)
plot_model(lmm_f, type="est")
plot_model(lmm_f, type="diag")
plot_model(lmm_f, type = "eff", 
           terms = c("NAP"), show.data = TRUE, jitter = 0.2,
           title = "", dot.size = 2, colors="darkgreen", line.size = 2)
###model diagnosis 
par(mfrow=c(2,2))
plot(residuals(lmm_f) ~ predict(lmm_f), xlab = "Predicted values", ylab = "Residuals", main="predicted v.s residuals")
abline(h=0, col="red")
plot(predict(lmm_f) ~  lmm_f$data$Richness, xlab="Observed values", ylab="Predicted values", main="observed v.s predicted")
abline(0,1, col="red")
qqnorm(residuals(lmm_f))###normal distribution of residuals
qqline(residuals(lmm_f))
boxplot(residuals(lmm_f) ~ lmm_f$data$fBeach, main="Beach",ylab="Residuals")
dev.off()
#################

library(MASS)
library(lme4)
gm1 <- glmer(Richness ~ NAP + (NAP|fBeach), data =RIKZ, family = "poisson")
summary(gm1)


new_NAP<-data.frame(NAP=seq(-1.336,2.2550,length=1000))
pred_R3<-exp(predict(gm1,newdata=new_NAP,re.form=~0))
ci_line3<-bootMer(gm1,FUN=function(.) predict(.,newdata=new_NAP,
                                              re.form=~0),nsim=100)
ci_R3<-apply(ci_line3$t,2,function(x) x[order(x)][c(25,975)])
lb_R3<-exp(ci_R3[1,])
ub_R3<-exp(ci_R3[2,])

plot(Richness~NAP,data=RIKZ,col=fBeach, pch=19)
lines(new_NAP$NAP,pred_R3,lwd=1,col="red")
lines(new_NAP$NAP,lb_R3,lty=2,col="red")
lines(new_NAP$NAP,ub_R3,lty=2,col="red")

################
par(mfrow=c(2,2))
plot(residuals(gm1) ~ predict(gm1), xlab = "Predicted values", ylab = "Residuals", main="predicted v.s residuals")
abline(h=0, col="red")
plot(predict(gm1) ~  log(RIKZ$Richness), xlab="Observed values", ylab="Predicted values", main="observed v.s predicted")
abline(0,1, col="red")
qqnorm(residuals(gm1))###normal distribution of residuals
qqline(residuals(gm1))
boxplot(residuals(gm1) ~ RIKZ$fBeach, main="Beach",ylab="Residuals")
dev.off()
##########linear mixed model with bayes
library(brms)
library(performance)
##########The RIKZ data
head(RIKZ)
RIKZ$fBeach <- factor(RIKZ$Beach)
RIKZ$Sample <- factor(RIKZ$Sample)
bm0 <- brm(Richness ~ NAP + (NAP|fBeach), 
           data =RIKZ, family = "poisson")
summary(bm0)
plot(marginal_effects(bm0),points=T)
mcmc_plot(bm0)
mcmc_plot(bm0, type="trace")
pp_check(bm0, nsamples=1000)
plot(conditional_effects(bm0, resolution =100), points=T)
r2_bayes(bm0) 
#####################333
