library(evsiexval)
setwd("M:/Projects/2023/Project.EVSIexval/Analysis")
source("include.R")


settings <- list(
  name="case_study",
  output_dir="M:/Projects/2023/Project.EVSIexval/Output/Results/WIP/",
  val_sample_size=c(500),
  future_sample_sizes=c(500, 1000, 2000, 4000, 8000, 16000),
  n_sim=10^6,  #Internal for VoI calculations
  n_DCA_bs=10000, #Number of bootstraps for DCA 95%CI
  zs=c(0.01,0.02)
)

#out <- readRDS(paste0(settings$output_dir, "case_study.RDS"))
out <- list(settings=settings)
out$model_coeffs <- coefficients(model)


model_equation <- function(model, proper_coeff_names=NULL, ...) { # from https://stats.stackexchange.com/questions/63600/how-to-translate-the-results-from-lm-to-an-equation
  format_args <- list(...)

  model_coeff <- model$coefficients
  if(is.null(proper_coeff_names))
  {
    coeff_names <- names(model_coeff)
  }
  else
  {
    coeff_names <- proper_coeff_names[names(model_coeff)]
  }

  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(#model$family$link,"(",strsplit(as.character(model$call$formula), "~")[[2]],")" # 'y'
                     "logit(P(Y=1))=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 "",
                                 coeff_names[-1],
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}


proper_coeff_names <- c("(Intercept)"="",
                              "age"="[age]",
                              "milocOther"="[AMI location other (vs. inferior)]",
                              "milocAnterior"="[AMI location anterior (v. inferior)]",
                              "pmiyes"="[previous AMI]",
                              "kill"="[AMI severity (Killip score)]",
                              "pmin(sysbp, 100)"="[min(blood pressure, 100)]",
                              "pulse"="[pulse]")


out$model_equation <- model_equation(model, proper_coeff_names, digits=3)

sample_size <- max(settings$val_sample_size)

if(!is.infinite(sample_size))
{
  val_data <- val_data[sample(1:(dim(data_us)[1]),sample_size,F),]
}

i<-1
for(sample_size in settings$val_sample_size)
{
  out$model_auc[i] <- pROC::auc(val_data$Y[1:sample_size],val_data$pi[1:sample_size])*1
  i <- i+1
}


EVSI(model, val_data[1:500,] , z=0.02, NULL, n_sim=settings$n_sim, prior=list(prev=c(1,1), se=c(1,1), sp=c(1,1)))$EVPI




###########################DCA
zs <- (0:10)/100
n <- dim(val_data)[1]
NBh_model <- NBh_all <- rep(0,length(zs))
for(i in 1:length(zs))
{
  NBh_model[i] <- mean((val_data$pi>zs[i])*(val_data$Y - (1-val_data$Y)*zs[i]/(1-zs[i])))
  NBh_all[i] <- mean((val_data$Y - (1-val_data$Y)*zs[i]/(1-zs[i])))
}

# And now uncertainty around it
bs_NBh_model <- bs_NBh_all <- matrix(0, nrow=settings$n_DCA_bs, ncol=length(zs))

for(i_sim in 1:settings$n_DCA_bs)
{
  w <- voipred:::bootstrap(n)

  for(i in 1:length(zs))
  {
    bs_NBh_model[i_sim,i] <- sum(w*(val_data$pi>zs[i])*(val_data$Y - (1-val_data$Y)*zs[i]/(1-zs[i])))/n
    bs_NBh_all[i_sim,i] <- sum(w*(val_data$Y - (1-val_data$Y)*zs[i]/(1-zs[i])))/n
  }
}

NBh_all <- colMeans(bs_NBh_all)
NBh_model <- colMeans(bs_NBh_model)


pdf(paste0(settings$output_dir,"DCA.pdf"), width=6, height=5)
  max_y <- max(0,NBh_all,NBh_model)
  min_y <- 0*min(0,NBh_all,NBh_model)
  par(mar=c(4, 4, 3, 0), xpd=TRUE)
  plot(zs[1:10], NBh_all[1:10], col='darkgray', type='l', lty=2, lwd=2, xlim=c(0,0.1), ylim=c(0,max_y), xlab="Risk threshold (z)", ylab="Net benefit")
  lines(zs, zs*0, col='darkgray', type='l', lwd=2, lt=3)
  lines(zs, NBh_model, col='blue4', type='l', lwd=2)
  # ci_model <- apply(bs_NBh_model,MARGIN = 2,FUN = quantile, c(0.025,0.975))
  # lines(zs,ci_model[1,], type='l', col='gray', lt=2)
  # lines(zs,ci_model[2,], type='l', col='gray', lt=2)
  # ci_all <- apply(bs_NBh_all,MARGIN = 2,FUN = quantile, c(0.025,0.975))
  # lines(zs,ci_all[1,], type='l', col='gray', lt=2)
  # lines(zs,ci_all[2,], type='l', col='gray', lt=2)
  #legend(0.0,1.2*max_y, legend=c("Treat none", "Use the model", "Treat all"), lty=c(1,1,1),  col=c("lightgray","blue4","darkgray"), lwd=c(2,2,2), horiz=T, bty="n")
  #legend(0.0,0.022, legend=c("Default strategies*","Use the model","95% confidence interval"), lty=c(1,1,2),  col=c("gray","blue4","gray"), lwd=c(2,2,1))
dev.off()


pdf(paste0(settings$output_dir,"dNB.pdf"), width=6, height=5)
  par(mar=c(4, 4, 3, 0), xpd=TRUE)
  ci <- apply(bs_NBh_model - pmax(bs_NBh_all,0),MARGIN = 2,FUN = quantile, c(0.025,0.975))
  max_y <- max(ci[2,])
  plot(zs, NBh_model - pmax(NBh_all,0), col='blue4', type='l', lwd=2, ylim=c(min_y,max_y), xlab="Risk threshold (z)", ylab="Net benefit")
  lines(zs,ci[1,], type='l', col='gray', lt=2)
  lines(zs,ci[2,], type='l', col='gray', lt=2)
  #legend(0,1.5*max_y, legend=c("Incremental NB of the model","95% confidence interval"), lty=c(1,1),  col=c("blue4","gray"), lwd=c(2,2), bty="n", horiz=T)
dev.off()



###EVPI
zs <- c(0:10)/100
EVPIs <- data.frame(val_size=integer(), z=double(), EVPI=double())
i<-1
for(sample_size in settings$val_sample_size)
{
  tmp <- c()
  dt <- val_data[1:sample_size,]
  for(z in zs)
  {
    evidence <- list(prev=c(sum(val_data$Y), nrow(val_data)-sum(val_data$Y)),
                     se=c(sum(val_data$Y*(val_data$pi>=z)),sum(val_data$Y)-sum(val_data$Y*(val_data$pi>=z))),
                     sp=c(sum((1-val_data$Y)*(val_data$pi<z)),sum(1-val_data$Y)-sum((1-val_data$Y)*(val_data$pi<z)))
                     )

    EVPI <- EVSI_ag(evidence, z, NULL, n_sim=settings$n_sim, prior=list(prev=c(1,1), se=c(1,1), sp=c(1,1)))$EVPI
    EVPIs[i,] <- c(sample_size, z, EVPI)
    i <- i+1
    cat(i)
  }
}

#EVPI curve
pdf(paste0(settings$output_dir,"EVPI.pdf"), width=7, height=5)
df <- EVPIs[which(EVPIs$val_size==settings$val_sample_size),]
plot(zs, df$EVPI, type='l', xlab='Risk threshold (z)', ylab='EVPI', col='blue', lwd=2)
dev.off()


#Scaled EVPI curve
pdf(paste0(settings$output_dir,"scaled_EVPI.pdf"), width=8, height=5)
  par(mar=c(4, 4, 0, 3), xpd=TRUE)
  df <- EVPIs[which(EVPIs$val_size==settings$val_sample_size),]
  scale <- 1
  max_y <- df$EVPI/scale*1.1
  plot(zs, df$EVPI, type='l', xlab='Risk threshold (z)', ylab='EVPI', col='blue4', lwd=2)
  #title(paste(df$val_size, z))
  y2 <- pretty(c(0,800000*df$EVPI))
  axis(4, at=y2/800000, labels=y2)
  mtext("Population EVPI", side=4, line=2)
dev.off()




### EVSI
EVSIs <- NULL
for(sample_size in settings$val_sample_size)
{
  for(z in settings$zs)
  {
    tmp <- val_data[1:sample_size,]
    evidence <- list(prev=c(sum(tmp$Y), nrow(tmp)-sum(tmp$Y)),
                     se=c(sum(tmp$Y*(tmp$pi>=z)),sum(tmp$Y)-sum(tmp$Y*(tmp$pi>=z))),
                     sp=c(sum((1-tmp$Y)*(tmp$pi<z)),sum(1-tmp$Y)-sum((1-tmp$Y)*(tmp$pi<z)))
    )
    tmp <- EVSI_ag(evidence, z, settings$future_sample_sizes, n_sim=settings$n_sim)
    EVSIs <- rbind(EVSIs, c(val_size=sample_size, z=z, EVPI=tmp$EVPI, EVSI=tmp$EVSI))
  }
}
EVSIs <- as.data.frame(EVSIs)
out$EVSIs <- EVSIs



my_colors <- c('blue','red')
evsi_cols <- which(substr(colnames(EVSIs),1,4)=="EVSI")
for(z in settings$zs)
{
  i <- 1
  for(sample_size in settings$val_sample_size)
  {
    #pdf(paste0(settings$output_dir,"EVSI_",sample_size,"_",z,".pdf"), width=7, height=5)
    df <- sqldf(paste("SELECT * FROM EVSIs WHERE z=",z," AND val_size=",sample_size))
    max_y <- max(df$EVPI)
    par(las=2)
    plot(c(0,settings$future_sample_sizes), c(0,df[1,evsi_cols]), type='l', ylim=c(0,max_y), xlab="Future sample size", ylab="EVSI", col=my_colors[i], xaxt="n")
    axis(1, at=settings$future_sample_sizes, labels=settings$future_sample_sizes)
    #title(paste(df$val_size, z))
    lines(c(0,max(settings$future_sample_sizes)), c(df[1,'EVPI'],df[1,'EVPI']), col='gray')
    #dev.off()
    i <- i+1
  }
}





#Scaled EVSI (V2 - combined)
pdf(paste0(settings$output_dir,"scaled_EVSI.pdf"), width=8, height=5)
  z <- 0.02
  sample_size <- 500
  df <- sqldf(paste("SELECT * FROM EVSIs WHERE z=",z," AND val_size=",sample_size))
  evsi_cols <- which(substr(colnames(EVSIs),1,4)=="EVSI")

  scale <- 1 #df$EVPI
  max_y <- df$EVPI/scale*1.1
  par(mar=c(4, 4, 3, 3), xpd=TRUE)
  plot(c(0,settings$future_sample_sizes)[1:5], (c(0,unlist(df[1,evsi_cols]))/scale)[1:5],
     type='l', ylim=c(0,max_y),
     xlab="Future sample size (N)",
     ylab="EVSI", col='red3', lwd=2, xaxt="n")
  axis(1, at=c(0,settings$future_sample_sizes), las=2)
  #title(paste(df$val_size, z))
  lines(c(0,max(settings$future_sample_sizes[1:4])), c(df$EVPI/scale,df$EVPI/scale), col='blue4')
  y2 <- pretty(c(0,800000*df$EVPI))
  axis(4, at=y2/800000, labels=y2)
  mtext("Population EVSI", side=4, line=2)

  #legend(0, 1.25*max_y, legend=c("Threshold(z)", "0.01","0.02"), lwd=c(2,2), lty=c(1,2), col=c('white','blue','red'), bty="n", horiz=T)

  z <- 0.01
  sample_size <- 500
  df <- sqldf(paste("SELECT * FROM EVSIs WHERE z=",z," AND val_size=",sample_size))

  #pdf(paste0(settings$output_dir,"scaled_EVSI_",sample_size,"_",z,".pdf"))

  options(scipen=999) #Prevent Y axis from going scientific
  scale <- 1 #df$EVPI
  #max_y <- df$EVPI/scale*1.1
  #par(mar=c(4, 4, 0, 3), xpd=TRUE)
  lines(c(0,settings$future_sample_sizes)[1:5], (c(0,unlist(df[1,evsi_cols]))/scale)[1:5],
     type='l', ylim=c(0,max_y),
     xlab="Future sample size",
     ylab="EVSI", col='red3', lwd=2, lty=2)
  #title(paste(df$val_size, z))
  lines(c(0,max(settings$future_sample_sizes[1:4])), c(df$EVPI/scale,df$EVPI/scale), col='blue4', lty=2)
dev.off()







#Stuff that we use in the text and their processing is put here to save inline code
entities <- list()

entities$n_full_dev <- nrow(dev_data)
entities$n_full_val <- nrow(data_us)

entities$prev_full_dev <- mean(dev_data$day30)
entities$prev_full_val <- mean(data_us$day30)
entities$prev_val <- mean(val_data$day30)


EVPI_02 <- as.numeric(out$EVSIs %>% filter(z==0.02 & val_size==min(settings$val_sample_size)) %>% select(EVPI))
entities$EVPI_02 <- EVPI_02
EVSI_02 <- as.numeric(out$EVSIs %>% filter(z==0.02 & val_size==min(settings$val_sample_size)) %>% select(EVSI2))
entities$EVSI_02 <- EVSI_02

EVPI_01 <- as.numeric(out$EVSIs %>% filter(z==0.01 & val_size==min(settings$val_sample_size)) %>% select(EVPI))
entities$EVPI_01 <- EVPI_01
EVSI_01 <- as.numeric(out$EVSIs %>% filter(z==0.01 & val_size==min(settings$val_sample_size)) %>% select(EVSI2))
entities$EVSI_01 <- EVSI_01

out$entities <- entities
out$val_data <- val_data

saveRDS(out,paste0(settings$output_dir, "case_study.RDS"))
print(paste("Results saved at", settings$output_dir))







