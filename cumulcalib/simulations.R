# By Mohsen Sadatsafavi
# Last update: March 16, 2024
# This file contains the code for running the simulation section of the associated paper.

#Required packages.
library(cumulcalib)
library(MASS)
library(generalhoslem)
library(progress)


aux <- list()







#This is the simulation code for investigating the null-behavior of the tests
sim_null_behavior <- function(b0s=c(-2,-1,0), sample_sizes=c(50, 100, 250, 1000), n_sim=10000, seed=1)
{
  set.seed(seed)
  columns <- c("i_sim","sample_size", "b0", "pval.BM", "pval.BB")
  out<-as.data.frame(matrix(NA, nrow =n_sim*length(sample_sizes)*length(b0s),ncol=length(columns)))
  colnames(out) <- columns

  pb <- progress_bar$new(total = n_sim)
  index <- 1
  for(i in 1:n_sim) #Loop over simulations
  {
    pb$tick()
    for(sample_size in sample_sizes)
    {
      for(b0 in b0s) #Loop over beta_0 (intercept)
      {
        logit_pi <- b0+rnorm(sample_size)
        o <- order(logit_pi)
        logit_pi <- logit_pi[o]
        pi <- 1/(1+exp(-logit_pi))
        y=rbinom(sample_size,size=1, prob=pi)
        #tmp1 <- cumulcalib(y, pi, ordered = T, n_sim=10000)
        tmp <- cumulcalib(y, pi, ordered = T)

        out[index,] <- c(i, sample_size, b0, tmp$by_metho$BM$pval, tmp$by_method$BB$pval)
        index <- index+1
      }
    }

  }
  out
}




#Processing the results returned by sim_null_behavior
process_sim_null_behavior <- function(x, type="qq", val=c('BM'="pval.BM",'BB'="pval.BB"))
{
  require("sqldf")


  l1_vals <- unique(x[,'sample_size'])
  l2_vals <- unique(x[,'b0'])

  par(mfrow=c(length(l1_vals),length(l2_vals)))

  my_palette <- c("blue", "#F17720")

  if(type=="qq")
  {
    par(mar=c(2,2,1,1))

    xs <- (0:100)/100

    #cols <- names(x)[which(substring(names(x),1,5)=="pval.")]
    for(i in l1_vals)
      for(j in l2_vals)
        for(k in 1:length(val))
        {
          str <- paste0("SELECT [", val[k], "] FROM  x WHERE sample_size=", i," AND b0=" , j, "")
          this_data <- sqldf(str)[[1]]
          ys <- ecdf(this_data)(xs)
          if(k==1)
          {
            xaxt <- yaxt <- 'n'
            if(i==l1_vals[length(l1_vals)]) xaxt<-'s'
            if(j==l2_vals[1]) yaxt<-'s'
            plot(xs,ys,xlim=c(0,1),ylim=c(0,1),type='l',col=my_palette[k], lwd=2, xaxt=xaxt, yaxt=yaxt)
            lines(c(0,1),c(0,1),col="gray")
            title(paste0("n=",i," | b0=",j),line = -1)
          }
          else
          {
            lines(xs,ys,col=my_palette[k], lwd=2)
          }
          p <- round(mean(this_data<0.05),digits = 3)
          text(0.2,0.7-k/10,paste0(names(val)[k],":",paste0(format(p*100,nsmall=1),"%")))
        }
  }
  else
  {
    par(mar=0*c(1,1,1,1))

    for(i in l1_vals)
      for(j in l2_vals)
      {
        str <- paste0("SELECT [", val, "] FROM  x WHERE sample_size=", i," AND b0=" , j, "")
        this_data <- sqldf(str)[[1]]

        p <- mean(this_data<0.05)
        hist(this_data, main="", axes=F, freq=F)
        title(paste0("n=",i," | b0=",j),line = -1)
        text(0.1,0.1,p)

        index <- index+1
      }
  }
}


# saveRDS(res_null, "M:/Projects/2023/Project.CumulCalib/sealedResults/2024.05.21/res_null.RDS")
# res_null <- readRDS(paste0(path_to_results,"res_null.RDS"))
# setEPS()
# postscript("./res_null.eps")
# process_sim_null_behavior(res_null)
# dev.off()





#The 'linear' miscalibration simulation
#This function returns a data frame each row containing the results from one loop of Monte Carlo simulations
detailed_sim_linear<-function(sample_sizes=c(100,250,1000), X_dist=c(0,1), b0s=c(-0.25,-0.125,0,0.125,0.25),b1s=c(0.5,3/4,1,4/3,2),n_sim=2500, GRuse=FALSE, seed=1)
{
  set.seed(seed)

  #Creating the data frame that this function returns
  columns <- c("i_sim","sample_size", "b0", "b1", "pval.HLT","pval.LRT", "pval.BM", "pval.BB")
  out <- as.data.frame(matrix(NA, nrow =n_sim*length(sample_sizes)*length(b0s)*length(b1s), ncol = length(columns)))
  colnames(out) <- columns

  pb <- progress_bar$new(total=n_sim)
  index<-1
  for(i in 1:n_sim)  #Main simulation loop
  {
    pb$tick()
    for(j in 1:length(sample_sizes))  #loop over requested sampel sizes
    {
      ss<-sample_sizes[j]
      for(k0 in 1:length(b0s)) #Loop over requested intercept (beta_0) values
      {
        b0<-b0s[k0]
        for(k1 in 1:length(b1s))  #Loop over requested slopes (this is implemented but not used in the paper)
        {
          b1<-b1s[k1]
          if(is.null(X_dist)) p<-runif(ss) else  p<-1/(1+exp(-rnorm(ss,X_dist[1],X_dist[2]))) #Creating actual risks
          p <- p[order(p)]
          pi<-1/(1+exp(-(b0+b1*log(p/(1-p)))))  #Creating predicted risks
          logit.pi<-log(pi/(1-pi))

          repeat  #Generate the response variable
          {
            y=rbinom(ss,size = 1,prob=p)
            if(sum(y)>=3 && sum(1-y)>=3)
              break
            else
            {
              message("OUCH - bad sample, too few outcomes, repeat!")
            }
          }

          out[index,1]<-i
          out[index,2]<-ss
          out[index,3]<-b0
          out[index,4]<-b1

          #Likelihood ratio test
          f.0<-glm(y~-1+offset(logit.pi),family="binomial")
          #f.a<-glm(y~offset(logit.pi_star),family="binomial")
          f.ab<-glm(y~logit.pi,family="binomial")
          #message(coefficients(f.ab))
          p.val.ab<-1-pchisq(f.0$deviance-f.ab$deviance,2)
          out[index,"pval.LRT"]<-p.val.ab
          #Hosmer-Lemeshow test
          tmp <- logitgof(y, pi)
          out[index,"pval.HLT"]<-1-pchisq(tmp$statistic,10)
          #BM and BB tests, as explained in the paper
          tmp <- cumulcalib(y, pi, ordered = T)
          out[index,"pval.BM"]<-tmp$by_method$BM$pval
          #out[index,"pval.BM2"]<-tmp$by_method$BM2$pval
          out[index,"pval.BB"]<-tmp$by_method$BB$pval

          index<-index+1
        }
      }
    }
  }

  aux$out<<-out

  return(out)
}




#The 'power' miscalibration simulation
#This function returns a data frame each row containing the results from one loop of Monte Carlo simulations
#X_dist:mean and SD of the distribution of the simple predictor. If NULL, then directly samples pi from standard uniform.
detailed_sim_power<-function(sample_sizes=c(100,250,500), X_dist=c(0,1), b0s=c(-0.25,-0.125,0,0.125,0.25),b1s=c(0.5,3/4,1,4/3,2), b2s=NULL, n_sim=2500, GRuse=FALSE, seed=1)
{
  set.seed(seed)
  columns <- c("i_sim","sample_size", "b0", "b1", "pval.HLT","pval.LRT", "pval.BM", "pval.BB")
  out <- as.data.frame(matrix(NA, nrow =n_sim*length(sample_sizes)*length(b0s)*length(b1s), ncol = length(columns)))
  colnames(out) <- columns

  pb <- progress_bar$new(total=n_sim)
  index<-1
  for(i in 1:n_sim) #Loop over simulations
  {
    pb$tick()
    for(j in 1:length(sample_sizes)) #Loop over sample sizes
    {
      ss<-sample_sizes[j]
      for(k0 in 1:length(b0s)) #Loop over intercepts ('a' in the manuscript)
      {
        b0<-b0s[k0]
        for(k1 in 1:length(b1s)) #Loop over power factor (b in the manuscript)
        {
          b1<-b1s[k1]
          if(is.null(b2s) || b2s<0) b2s<- -1/b1   #If b2s is null, it is set as reciprocal of b1. This accomodates a third parameter but is not used in the paper
          for(k2 in 1:length(b2s))
          {
            b2<-abs(b2s[k2])

            #Generating actual (p) and predicted (pi) risks
            repeat
            {
              if(is.null(X_dist)) p<-runif(ss) else  p<-1/(1+exp(-rnorm(ss,X_dist[1],X_dist[2])))
              o <- order(p)
              p <- p[o]
              x<-log(p/(1-p))
              xx<-b0+sign(x)*b1*(abs(x)^b2)
              pi<-1/(1+exp(-xx))
              if(min(pi)>0 && max(pi)<1)
                break
              else
              {
                message("OUCH - bad sample, extreme probabilities, repeat!")
              }
            }

            logit.pi<-log(pi/(1-pi))
            repeat  #Generating response
            {
              y=rbinom(ss, size=1, prob=p)
              if(sum(y)>=3 && sum(1-y)>=3)
                break
              else
              {
                message("OUCH - bad sample, too few outcomes, repeat!")
              }
            }

            out[index,1]<-i
            out[index,2]<-ss
            out[index,3]<-b0
            out[index,4]<-b1
            out[index,5]<-b2

            aux$i <<- i
            aux$parms <<- c(ss=ss,b0=b0,b1=b1,b2=b2)
            aux$y <<- y
            aux$pi <<- pi

            #The LR test
            aux$stage <<- "f.0"
            f.0<-glm(y~ -1+offset(logit.pi),family="binomial")
            #f.a<-glm(y~offset(logit.pi_star),family="binomial")
            aux$stage <<- "f.ab"
            f.ab<-glm(y~logit.pi,family="binomial")
            #message(coefficients(f.ab))
            p.val.ab<-1-pchisq(f.0$deviance-f.ab$deviance,2)
            out[index,"pval.LRT"]<-p.val.ab

            #The Hosmer-Lemeshow test
            aux$stage <<- "logitgof"
            tmp <- logitgof(y, pi)
            out[index,"pval.HLT"]<-1-pchisq(tmp$statistic,10)

            #The BM and BB tests
            aux$stage <<- "cumulcalib"
            tmp <- cumulcalib(y, pi, ordered = T)
            out[index,"pval.BM"]<-tmp$by_method$BM$pval
            out[index,"pval.BB"]<-tmp$by_method$BB$pval

            index<-index+1
          }
        }
      }
    }
  }

  return(out)
}



#This function generates the barplots presenting the results of both linear and power simulations
process_detailed_sim_results_graph<-function(x, detailed=F, n_col=5, dec_points=3, level1="b0", level2="b1", level3="sample_size", rounding_error=0.001)
{
  require("sqldf")
  require("MASS") #For fractions()
  l1_vals <- unique(x[,level1])
  l2_vals <- unique(x[,level2])

  par(mfrow=c(length(l1_vals),length(l2_vals)))
  par(mar=0*c(1,1,1,1))

  for(i in l1_vals)
    for(j in l2_vals)
    {
      str <- paste0("SELECT [",level3,"], AVG([pval.LRT]<0.05), AVG([pval.HLT]<0.05), AVG([pval.BM]<0.05), AVG([pval.BB]<0.05) FROM x WHERE ABS([",level1,"]-(",i,"))<",rounding_error," AND ABS([", level2,"]-(",j,"))<", rounding_error," GROUP BY [",level3,"]")
      this_data <- sqldf(str)
      my_palette <- c("#FFFFFF","#C0C0C0",  "blue", "#F17720")
      level3_values <- this_data[,1]
      values <- as.vector(rbind(t(this_data)[-1,],0))
      bp<-barplot(values,xaxt='n', yaxt='n', space=0, ylim=c(-0.25,1.6),col=c(my_palette,rgb(1,0,0)))
      text(x=0.4+c(0:14)*1,y=values+0.25,ifelse(values==0,"",round(values,2)),cex=0.9, srt=90)
      text(x=6,y=1.5,paste0(" a=", fractions(i)," | b=", fractions(j)),cex=1.5,col="#404040")
      #if(i==l1_vals[length(l1_vals)])
      text(x=c(1, 7 ,12),y=-0.1,paste(level3_values),cex=1.5)
    }
}


res_lin <- readRDS(paste0(path_to_results,"res_lin.RDS"))
setEPS()
postscript("./res_lin.eps")
process_detailed_sim_results_graph(res_lin)
dev.off()


res_pow <- readRDS(paste0(path_to_results,"res_pow.RDS"))
setEPS()
postscript("./res_pow.eps")
process_detailed_sim_results_graph(res_pow)
dev.off()


#
#
#
# #Draws the PDF Of the null distirbutions of the BM and BB tests. This is no longer reported in the manuscript.
# draw_pdfs <- function(eps=T)
# {
#   if(eps)
#   {
#     setEPS()
#     postscript("./cumulcalib/pdfs.eps")
#   }
#
#   xs <- (1:3500)/1000
#   cdf_a <- cdf_b1 <- cdf_b2 <- cdf_b3 <- cdf_c <- c(NA,lengths(xs))
#   for(i in 1:length(xs))
#   {
#     cdf_a[i] <- cumulcalib:::pMAD_BM(xs[i])
#     cdf_b1[i] <- cumulcalib:::pMAD_BM_c(xs[i],0.5, method=1)
#     cdf_b2[i] <- cumulcalib:::pMAD_BM_c(xs[i],1, method=1)
#     cdf_b3[i] <- cumulcalib:::pMAD_BM_c(xs[i],1.5, method=1)
#     cdf_c[i] <- cumulcalib:::pKolmogorov(xs[i])
#   }
#
#
#   y_a <- cdf_a[-1]-cdf_a[-length(cdf_a)]
#   y_a[length(y_a)] <- 0
#   y_b1 <- cdf_b1[-1]-cdf_b1[-length(cdf_b1)]
#   y_b2 <- cdf_b2[-1]-cdf_b2[-length(cdf_b2)]
#   y_b3 <- cdf_b3[-1]-cdf_b3[-length(cdf_b3)]
#   y_c <- cdf_c[-1]-cdf_c[-length(cdf_c)]
#
#
#
#   plot(xs, y_a, type='l', ylim=c(0,max(c(y_a,y_c))), lwd=2, xlab="Statistic value", ylab="Density", col="blue")
#   # t1 <- cumulcalib:::qMAD_BM(0.95)
#   # lines(c(t1,t1),c(0,1000),lty=3, col="blue")
#   # t1 <- cumulcalib:::qMAD_BM(0.5)
#   # lines(c(t1,t1),c(0,1000),lty=3, col="blue")
#   y_b1[which(y_b1<=0)]<-NA
#   #lines(xs, y_b1, type='l', col='blue', lwd=1)
#   y_b2[which(y_b2<=0)]<-NA
#   #lines(xs, y_b2, type='l', col='blue', lwd=1)
#   y_b3[which(y_b3<=0)]<-NA
#   #lines(xs, y_b3, type='l', col='blue', lwd=1)
#   lines(xs, y_c, type='l', col='#F17720', lwd=2)
#   #t2 <- cumulcalib:::qKolmogorov(0.95)
#   #lines(c(t2,t2),c(0,1000),col="red",lty=3)
#
#   if(eps)
#   {
#     dev.off()
#   }
#
# }
