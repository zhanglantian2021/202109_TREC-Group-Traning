####MCMC example
n=1000
samples=numeric(n)
samples[1]=0.1
for (i in 2:n)
{
  proposal=samples[i-1]+rnorm(1,0,0.1)
  if ((dnorm(0.8,proposal,1)/dnorm(0.8,samples[i-1],1))>runif(1))
  {samples[i]=proposal}else{samples[i]=samples[i-1]}
}
plot(samples,type="b")
#####
library(brms)
library(lme4)
data(sleepstudy)
##The average reaction time per day (in milliseconds) for subjects in a sleep deprivation study.
summary(sleepstudy)
m1=brm(Reaction~Days+(Days|Subject),sleepstudy)
summary(m1)
prior = c(set_prior("cauchy(0,2.5)", class = "Intercept"), ###截距
          set_prior("cauchy(0,2.5)", class = "b", coef="Days"), ###斜率
          set_prior("cauchy(0,2.5)", class = "sd"),##随机效应方差
          set_prior("lkj(2)", class = "cor")) ###随机截距，随机斜率的相关性
m2<-brm(Reaction~Days + (Days|Subject), prior=prior, data = sleepstudy)
get_prior(Reaction~Days + (Days|Subject), data = sleepstudy)
