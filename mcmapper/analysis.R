test_via_sim <- function(mcmapper_output, n_sim=10^6, test_threshold=0.01, sig=0.05)
{
  args <- mcmapper_output$value
  if(mcmapper_output$type=="logitnorm")
  {
    pi <- 1/(1+exp(-rnorm(n_sim,args[1],args[2])))
    Y <- rbinom(n_sim,1,pi)
  }

  if(mcmapper_output$type=="probitnorm")
  {
    pi <- pnorm(rnorm(n_sim,args[1],args[2]))
    Y <- rbinom(n_sim,1,pi)
  }

  if(mcmapper_output$type=="beta")
  {
    pi <- rbeta(n_sim,args[1],args[2])
    Y <- rbinom(n_sim,1,pi)
  }

  require(pROC)

  r <- pROC::roc(Y~pi, ci=T)
  z <- as.vector(r$ci)
  se <- (z[3]-z[1])/2/1.96
  Pc <- pnorm(mcmapper_output$target[2]*(1-test_threshold),z[2],se)+1-pnorm(mcmapper_output$target[2]*(1-test_threshold),z[2],se)
  se <- sd(pi-mcmapper_output$target[1])
  c(m=mean(pi), c=r$auc)
}




m <- 0.086
c <- 0.81

res_logitnorm <- mcmap_logitnorm(c(m,c))
res_probitnorm <- mcmap_probitnorm(c(m,c))
res_beta <- mcmap_beta(c(m,c))

x <- (0:(10000-1))/10000
plot(x, dlogitnorm(x,res_logitnorm[1],res_logitnorm[2]), type='l')
lines(x, dprobitnorm(x,res_probitnorm[1],res_probitnorm[2]), type='l', col='blue')
lines(x, dbeta(x,res_beta[1],res_beta[2]), type='l', col='red')

plot(x, plogitnorm(x,res_logitnorm[1],res_logitnorm[2]), type='l')
lines(x, pprobitnorm(x,res_probitnorm[1],res_probitnorm[2]), type='l', col='blue')
lines(x, pbeta(x,res_beta[1],res_beta[2]), type='l', col='red')



populate_table <- function()
{
  out <- data.frame(m=double(), c=double(), type=character(), parm1=double(), parm2=double())

  ms <- (1:99)/100
  cs <- (51:99)/100

  N <- length(ms)*length(cs)*3

  out[N,1] <- 0

  index <- 1
  for(m in ms)
    for(c in cs)
    {
      cat(c(m,c),"|");

      res <- mcmap(c(m,c), "logitnorm"); if(is.null(res)) {res<-c(NA,NA); message("Bad")}
      out[index,] <- list(m,c,"logitnorm",res[1],res[2])
      index <- index+1

      # res <- mcmap(c(m,c),"probitnorm"); if(is.null(res)) {res<-c(NA,NA); message("Bad")}
      # out[index,] <- list(m,c,"probitnorm",res[1],res[2])
      # index <- index+1
      #
      # res <- mcmap(c(m,c),"beta"); if(is.null(res)) {res<-c(NA,NA); message("Bad")}
      # out[index,] <- list(m,c,"beta",res[1],res[2])
      # index <- index+1
    }

  out
}




