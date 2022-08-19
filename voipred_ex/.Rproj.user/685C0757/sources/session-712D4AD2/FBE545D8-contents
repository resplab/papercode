dca <- function(val_data, zs=(0:99)/100, weights=NULL)
{
  n <- dim(val_data)[1]
  
  NB_model <- NB_all <- rep(0, length(zs))
  
  if(is.null(weights)) weights <- rep(1,n)
    
  for(j in 1:length(zs))
  {
    NB_model[j] <- sum(weights*(val_data$pi>zs[j])*(val_data$Y-(1-val_data$Y)*zs[j]/(1-zs[j])))/n
    NB_all[j] <- sum(weights*(val_data$Y-(1-val_data$Y)*zs[j]/(1-zs[j])))/n
  }
  
  return(data.frame(z=zs, NB_model=NB_model, NB_all=NB_all))
}

bootstrap <- function (n, Bayesian=F, weights=NULL)
{
  if(Bayesian)
  {
    u <- c(0,sort(runif(n-1)),1)
    u <- (u[-1] - u[-length(u)])*n
    if(!is.null(weights)) u <- u*weights*n/sum(u*weights)
  }
  else
  {
    if(is.null(weights)) weights <-rep(1/n,n)
    u <- rmultinom(1,n,weights)
  }
  as.vector(u)
}



