
#########MCMC
td1=data.frame(c(0.8,0.6,0.9,0.3,0.7,0.4))
names(td1)="y"
samples=200
samples[1]=0.1
for(i in 2:200){
  proposal=samples[i-1]+rnorm(1,0,0.2)
  ##add a minor radom to i-1 parameter
  if(prod(dnorm(td1$y, proposal, 1)) / prod(dnorm(td1$y, samples[i-1], 1)) >runif(1)) 
  
samples[i]<-proposal
else (samples[i]<-samples[i-1])
}


