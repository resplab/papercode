###Table 2 code:

library(MASS); data(birthwt)

n <- dim(birthwt)[1]
z <- 0.2 #Risk threshold

#Step 1: calculate predicted probabilities.
pi <- 1/(1+exp(-(2-0.05*birthwt$age-0.01*birthwt$lwt)))  #Predicted risks


#Step 2: The bootstrap Monte Carlo simulation
N <- 10000
NBmodel <- NBall <- NBmax <- rep(0,N)
for(i in 1:N)
{
  bsdata <-  birthwt[sample(1:n, n, replace = T),]
  NBall[i] <- mean(bsdata$low-(1-bsdata$low)*z/(1-z))
  NBmodel[i] <- mean((bsdata$pi>z)*(bsdata$low-(1-bsdata$low)*z/(1-z))) #NB of using the model
}


#Step 3: EVPI calculation
EVPI <- mean(pmax(0,NBmodel,NBall))-max(0,mean(NBmodel),mean(NBall))






### Table 3 code
library(MASS);
data(birthwt)
#remotes::install_github("resplab/predtools")
library(predtools)

n <- dim(birthwt)[1]
z <- 0.2 #This is the risk threshold

#Step 1: calculate predicted probabilities. #Our model is logit(pi) = 2 -0.05*age -0.01*lwt
pi <- 1/(1+exp(-(2-0.05*birthwt$age-0.01*birthwt$lwt)))

#Step 2: Estimate the moments*
Y <- birthwt$low;

moments <- calc_NB_moments(Y,pi,z)

#Step 3: Closed-form solution for E{max{0,NB_model,NB_all } }
A <- do.call(mu_max_trunc_bvn, as.list(moments))

# Step 4: EVPI calculation
EVPI <- A - max(0,moments[1],moments[2])

