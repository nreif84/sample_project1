rm(list=ls(all=TRUE))
library(MASS)
library(lme4)
library(bbmle)
setwd("~/Desktop/sample_project1/")
load("may.30.2014.Rdata") 
ls()
head(sub1)

# shouldn't I be specifying what beta is based on the shape of the distribution from the observed data???
#		if so: can set b0=random deviate function (either zero inflated beta, or zero inflated betabiniomial is my guess...

# f.dist1 = fitdistr(x=sub1$Rec.Jan2014,densfun = "negative binomial")

var.plot = seq(0,4,by=0.2)

power.est  = numeric(length(var.plot))
for (j in 1:length(var.plot)){
	
	
nsim  =  50
pval  = numeric(length(50))

	

sim1  =  function(b0=5,bSeedPred=2, bCompet=0 , bSPxC=0, Vsite=1.5, Vplot=1, Verror=2) {
	for (i in 1:nsim){
		   Site 		 =  factor(rep( 1:10, each=4))
		   Plot 		 =  factor(rep(c(1:20),each = 2))
		   Split		 =  factor(rep(1:2,20))
		   SeedPred	 =  rep(c('excluded','open'),each=2,10)
		   Compet	 =  rep(c('removed','intact'),20)	   
#Random Effects
		  site.re	= rnorm(n = Site, mean = 0, sd= Vsite)
		  plot.re	= rnorm(n = Plot, mean = 0, sd = Vplot)
		  tot.re 	= rnorm(10 * 2 * 2, mean = 0 , sd = Verror)
		   
		   Recruitment  =  b0 + bSeedPred*(SeedPred=='excluded') + bCompet*(Compet=="removed")+
		    		   bSPxC*(SeedPred=='excluded'& Compet=='removed') + plot.re[Plot] + site.re[Site] + tot.re
		   mydata  =  data.frame(Site = paste('S',with = Site, sep =''), 
		                         Plot = paste('P',with = Plot, sep =''), 
		                         SeedPred = SeedPred,Compet= Compet,
		                         Recruitment = Recruitment)		   		
		                 
		            	fit1  =  lmer( Recruitment ~ SeedPred + (1|Site) + (1|Site/Plot),data=mydata, REML=FALSE)
		   		     fit2  =  lmer( Recruitment ~ (1|Site) + (1|Site/Plot), data=mydata,REML=FALSE)
				################need to get the specification correct for 1. the LR test (i.e. full vs reduced models) and 
				############### 											  2. the call from anova, and storing correctly in pval/power.es		
			pval[i]  =  anova(fit1,fit2)[2,"Pr(>ChiSq"]
		}
	power.est[j]  =  sum(pval)/nsim
	
	
	
	
}
set.seed(1984)
pb  =  txtProgressBar (max=100)
out1  =  replicate(1, {setTxtProgressBar(pb, getTxtProgressBar(pb)+1);
                       sim1(b0=10, bSeedPred=0, Vsite=1, Verror=1)})
power.run = locator()