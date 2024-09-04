#2024.06.18

#Comparing different EVSI computation methods

setwd("M:/Projects/2023/Project.EVSIexval/Analysis")
res_path <- "M:/Projects/2023/Project.EVSIexval/output/results/WIP/"
source("include.R")
val_data <- val_data[sample(1:(dim(data_us)[1]),500,F),]

library(evsiexval)

z <- 0.02
future_sample_sizes <- c(500, 1000, 2000, 4000)
n <- nrow(val_data)
n_sim <- 10^6

n <- nrow(val_data)
n_D <- sum(val_data$Y)
n_TP <- sum(val_data$Y*(val_data$pi>=z))
n_FN <- n_D-n_TP
n_TN <- sum((1-val_data$Y)*(val_data$pi<z))
n_FP <- n-n_D-n_TN

evidence <- list(prev=c(n_D, n-n_D),
                 se=c(n_TP, n_FN),
                 sp=c(n_TN, n_FP))



do_sim <- function(method)
{
  if(method=="BBS")
  {
    n <- nrow(val_data)

    NB0 <- NB1 <- NB2 <- NBbest <-0
    NBw <- rep(0, length(future_sample_sizes)) #Max NB after observing future data

    #Main simulation loop
    for(i in 1:n_sim)
    {
      #Bayesian bootstrapping involves sampling from Dirichlet(1,1,...,1).
      #Here we generate weights W by normalizing Gamma random variables.
      W <- rgamma(n, 1, 1)
      W <- W/sum(W)

      #(prev, se, sp) from Bayesian-bootstrapped data taken as draws from their posterior dist.
      prev <- sum(val_data$Y*W)
      se <- sum(val_data$Y*(val_data$pi>=z)*W)/sum(val_data$Y*W)
      sp <- sum((1-val_data$Y)*(val_data$pi<z)*W)/sum((1-val_data$Y)*W)

      #These are draws from 'true' NBs
      NB1t <- prev*se-(1-prev)*(1-sp)*z/(1-z)
      NB2t <- prev-(1-prev)*z/(1-z)
      NBbest <- NBbest + max(0, NB1t, NB2t) #For EVPI computation
      #Need to collect the sum to calculate ENB_current_info
      #As stated in the paper, this converges to NB calculated directly from the sample
      #However, embedding calculations inside the loop prevents getting negative VoI quantities
      #due to MC error
      NB1 <- NB1 + NB1t
      NB2 <- NB2 + NB2t

      for(j in 1:length(future_sample_sizes)) #Loop over requested future sample sizes
      {
        n_star <- future_sample_sizes[j]
        #Second-level resampling creates D*
        W_star <- rmultinom(1, n_star, W)
        #Adding +1 to all weights represents adding the current sample to the future sample
        W_pooled <- W_star+1

        #Imputing missing predictor values should be implemented at this point (not relevant for GUSTO data)

        prev_pooled <- sum(val_data$Y*W_pooled)/sum(W_pooled)
        se_pooled <- sum(val_data$Y*(val_data$pi>=z)*W_pooled)/sum(val_data$Y*W_pooled)
        sp_pooled <- sum((1-val_data$Y)*(val_data$pi<z)*W_pooled)/sum((1-val_data$Y)*W_pooled)

        #NB0 <- 0
        NB1_pooled <- prev_pooled*se_pooled-(1-prev_pooled)*(1-sp_pooled)*z/(1-z)
        NB2_pooled <- prev_pooled-(1-prev_pooled)*z/(1-z)

        #NBw[j] <- NBw[j]+max(0,NB1_pooled,NB2_pooled)
        NBw[j] <- NBw[j]+c(0,NB1t,NB2t)[which.max(c(0,NB1_pooled,NB2_pooled))]
      }
    }

    ENB_current_info <- max(0, NB1/n_sim, NB2/n_sim)

    EVPI <- NBbest/n_sim - ENB_current_info
    EVSI <- NBw/n_sim - ENB_current_info

    return(list(EVPI=EVPI, EVSI=EVSI))
  }


  if(method=="OBS")
  {
    n <- nrow(val_data)

    NB0 <- NB1 <- NB2 <- NBbest <-0
    NBw <- rep(0, length(future_sample_sizes)) #Max NB after observing future data

    #Main simulation loop
    for(i in 1:n_sim)
    {
      #Bayesian bootstrapping involves sampling from Dirichlet(1,1,...,1).
      #Here we generate weights W by normalizing Gamma random variables.
      W <- rmultinom(1, n, rep(1/n,n))/n

      #(prev, se, sp) from Bayesian-bootstrapped data taken as draws from their posterior dist.
      prev <- sum(val_data$Y*W)
      se <- sum(val_data$Y*(val_data$pi>=z)*W)/sum(val_data$Y*W)
      sp <- sum((1-val_data$Y)*(val_data$pi<z)*W)/sum((1-val_data$Y)*W)

      #These are draws from 'true' NBs
      NB1t <- prev*se-(1-prev)*(1-sp)*z/(1-z)
      NB2t <- prev-(1-prev)*z/(1-z)
      NBbest <- NBbest + max(0, NB1t, NB2t) #For EVPI computation
      #Need to collect the sum to calculate ENB_current_info
      #As stated in the paper, this converges to NB calculated directly from the sample
      #However, embedding calculations inside the loop prevents getting negative VoI quantities
      #due to MC error
      NB1 <- NB1 + NB1t
      NB2 <- NB2 + NB2t

      for(j in 1:length(future_sample_sizes)) #Loop over requested future sample sizes
      {
        n_star <- future_sample_sizes[j]
        #Second-level resampling creates D*
        W_star <- rmultinom(1, n_star, W)
        #Adding +1 to all weights represents adding the current sample to the future sample
        W_pooled <- W_star+1

        #Imputing missing predictor values should be implemented at this point (not relevant for GUSTO data)

        prev_pooled <- sum(val_data$Y*W_pooled)/sum(W_pooled)
        se_pooled <- sum(val_data$Y*(val_data$pi>=z)*W_pooled)/sum(val_data$Y*W_pooled)
        sp_pooled <- sum((1-val_data$Y)*(val_data$pi<z)*W_pooled)/sum((1-val_data$Y)*W_pooled)

        #NB0 <- 0
        NB1_pooled <- prev_pooled*se_pooled-(1-prev_pooled)*(1-sp_pooled)*z/(1-z)
        NB2_pooled <- prev_pooled-(1-prev_pooled)*z/(1-z)

        #NBw[j] <- NBw[j]+max(c(0,NB1_pooled,NB2_pooled))
        #Note: a variance reduction technique is to use
        NBw[j] <- NBw[j]+c(0,NB1t,NB2t)[which.max(c(0,NB1_pooled,NB2_pooled))]
        #That is, once the winning strategy after observing D* is determined,
        #we pick the true NB of that strategy instead of noisier NB from D*.
      }
    }

    ENB_current_info <- max(0, NB1/n_sim, NB2/n_sim)

    EVPI <- NBbest/n_sim - ENB_current_info
    EVSI <- NBw/n_sim - ENB_current_info

    return(list(EVPI=EVPI, EVSI=EVSI))
  }

  if(method=="BetaBin")
  {
    res <- EVSI_ag(evidence, z, future_sample_sizes, prior=list(prev=c(0,0), se=c(0,0),sp=c(0,0)), n_sim)
    res$summary <- NULL
    return(res)
  }


  if(method=="General100")
  {
    samples <- cbind(prev=rbeta(100,evidence$prev[1],evidence$prev[2]),
                     se=rbeta(100,evidence$se[1],evidence$se[2]),
                     sp=rbeta(100,evidence$sp[1],evidence$sp[2])
                     )
    res <- EVSI_gf(samples, z, future_sample_sizes, n_sim=n_sim/100)
    return(res)
  }


  if(method=="General1000")
  {
    samples <- cbind(prev=rbeta(1000,evidence$prev[1],evidence$prev[2]),
                     se=rbeta(1000,evidence$se[1],evidence$se[2]),
                     sp=rbeta(1000,evidence$sp[1],evidence$sp[2])
    )
    res <- EVSI_gf(samples, z, future_sample_sizes, n_sim=n_sim/1000)
    return(res)
  }
}



main <- function(n_meta_sim=1)
{
  out <- list()
  for(i in 1:n_meta_sim)
  {
    cat(i)
    for(m in c("BBS","OBS","BetaBin","General100","General1000"))
    {
      cat(m)
      time <- system.time(res <- do_sim(m))
      out <- rbind(out, c(time[3], unlist(res)))
    }
  }

  out
}

res <- main(100)

res2 <- as.double(res)
dim(res2) <- dim(res)
colnames(res2) <- colnames(res)
data <- as.data.frame(res2)
data$method <- rep(c("BBS","OBS","BetaBin","General (M=100)","General (M=1000)"), 10)

library(sqldf)

table <- data.frame(evsi1=character(), evsi2=character(), evsi3=character(), evsi4=character(), evpi=character(), time=character())
table[5,] <- ""

EVPIs <- sqldf("SELECT Method, AVG(EVPI) AS mu, SQRT(VARIANCE(EVPI)) AS sd FROM data GROUP BY method")
y <- EVPIs[match(c("BBS","OBS","BetaBin","General (M=100)","General (M=1000)"),EVPIs$method),]
table[,'evpi'] <- paste0(formatC(y$mu, format="e", digits=3)," (", formatC(y$sd/y$mu, format="e", digits=2), ")")

times <- sqldf("SELECT Method, AVG(elapsed) AS time FROM data GROUP BY method")
y <- times[match(c("BBS","OBS","BetaBin","General (M=100)","General (M=1000)"),times$method),]
table[,'time'] <- paste0(format(round(y$time, 1)))


x_range <- c(0, max(future_sample_sizes))
y_range <- c(0, max(EVPIs$mu+2*EVPIs$sd))
plot(x=future_sample_sizes, xlim=x_range, ylim=y_range, xlab="Future sample size", ylab="EVSI")

for(i in 1:4)
{
  x <- sqldf(paste0("SELECT Method, AVG(EVSI",i,") AS mu, SQRT(VARIANCE(EVSI",i,")) AS sd, AVG(elapsed) AS time FROM data GROUP BY method"))
  y <- x[match(c("BBS","OBS","BetaBin","General (M=100)","General (M=1000)"),x$method),]
  table[, i] <- paste0(formatC(y$mu, format="e", digits=3)," (", formatC(y$sd/y$mu, format="e", digits=2), ")")

  band <- 100
  y_points <- seq(from=future_sample_sizes[i]-band, to=future_sample_sizes[i]+band, length.out=length(y$mu))
  points(y_points, y$mu)

  y$low <- y$mu-y$sd*1.96; y$high <- y$mu+y$sd*1.96
  for(j in 1:nrow(y))
  {
    lines(rep(y_points[j],2), c(y$low[j],y$high[j]))
  }
}

write.table(table, "clipboard")


