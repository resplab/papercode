#QRISK validation code
# By Mohsen Sadatsafavi
#Last revision:2023.06.16

setwd("R:/qdata/R")

library(haven)
library(dplyr)
library(survival)
library(cmprsk)
library(mice)
library(sqldf)

#Required for competing risk approach
library(riskRegression)
library(prodlim)

hc_numbers <- read.csv("hc_numbers.csv")

competing_risk = F #If T, CHD should have 2 levels with CHD=2 being all-cause mortality
max_fu <- 20

predictor_mapping <- list(
  patid="patid",
  gender="gender",
  age="patid_age",
  atrial_fibrillation="atrial_fib",
  atypical_antipsy="antipsycho",
  regular_steroid_tablets="steroid",
  erectile_disfunction="erect_dys",
  migraine="migraine",
  rheumatoid_arthritis="rheu_art",
  chronic_kidney_disease="ckd",
  severe_mental_illness="sev_mental_ill",
  systemic_lupus_erythematosis="sle",
  blood_pressure_treatment="trted_hyper",
  diabetes1="diabe_type1",
  diabetes2="diabe_type2",
  weight="weight",
  height="height", #processed
  ethiniciy="ethnicity", #processed
  heart_attack_relative="fh_cvd",
  cholesterol_HDL_ratio="serum_hdl_ratio",
  systolic_blood_pressure="sbp",
  std_systolic_blood_pressure="sbp_stddev",
  smoke="smoke",
  townsend="imd2015_5")



bootstrap <- function (n, Bayesian=F, weights=NULL)
{
  if(Bayesian)
  {
    u <- c(0,sort(stats::runif(n-1)),1)
    u <- (u[-1] - u[-length(u)])*n
    if(!is.null(weights)) u <- u*weights*n/sum(u*weights)
  }
  else
  {
    if(is.null(weights)) weights <-rep(1/n,n)
    u <- stats::rmultinom(1,n,weights)
  }
  as.vector(u)
}

read_sas_data <- function(save=T)
{
  final_table <<- read_sas("R:\\Qdata\\Final_table.sas7bdat")

  if(save)  saveRDS(final_table,"final_table.RDS")

  final_table
}



prepare_data <- function(data)
{
  data$gender <- data$gender-1

  data$fu <- data$FU_days/365

  long_fus <- which(data$fu>max_fu)
  if(length(long_fus)>0)
  {
    data[long_fus,'fu'] <- max_fu
    data[long_fus,'CHD'] <- 0
  }


  if(competing_risk==F) data[which(data$CHD==2),"CHD"] <- 0

  data$ethnicity <- unlist(sapply(data$ethnicity, function(x) switch(
    as.character(x),
    "white_or_not_stated"= 1,
    "Indian"= 2,
    "Pakistani"= 3,
    "Bangladeshi"=4,
    "Other_Asian"=5,
    "Black_Carib"=6,
    "Black_African"=7,
    "Chinese"=8,
    "Other_ethn_grp"=9,
    "Other"=9,
    NA
  )))

  data$smoke <- unlist(sapply(data$smoke_status, function(x) switch(
    as.character(x),
    "non_smoker"= 1,
    "ex_smoker"= 2,
    "light_smoker"= 3,
    "moderate_smoker"=4,
    "heavy_smoker"=5,
    " " = NA,
    NA
  )))

  data$height <- data$heights*100 #QRISK want this in cm

  print(apply(data,2,function(x){mean(is.na(x))}))

  data
}





impute_data <- function(data=final_data, m=5, maxit=10, load=F, save=T)
{
  if(load)
  {
    imputed_data<<-readRDS("imputed_data.RDS")
    return()
  }

  vars <-  unlist(predictor_mapping)[-1]
  data <- data[,vars]
  imputed_data<<-mice(data,m=m,maxit=maxit)

  if(save) saveRDS(imputed_data,"imputed_data.RDS")

  imputed_data
}


library(QRISK3)


my_qrisk <- function(data,predictors=predictor_mapping)
{

  valid_levels <- list(gender=c(0,1),
                       atrial_fibrillation=c(0,1),
                       atypical_antipsy=c(0,1),
                       regular_steroid_tablets=c(0,1),
                       erectile_disfunction=c(0,1),
                       migraine=c(0,1),
                       rheumatoid_arthritis=c(0,1),
                       chronic_kidney_disease=c(0,1),
                       severe_mental_illness=c(0,1),
                       systemic_lupus_erythematosis=c(0,1),
                       blood_pressure_treatment=c(0,1),
                       diabetes1=c(0,1),
                       diabetes2=c(0,1),
                       ethiniciy=(1:9),
                       smoke=(1:5)
  )


  for (item in names(valid_levels))
  {
    print(paste0("Checking variable ",item, "(",predictors[[item]],")"))
    data[,predictors[[item]]] <- as.numeric(data[,predictors[[item]]])
    if(sum(is.na(data[,predictors[[item]]]))>0) stop(paste0("Missing values detected in ",item))
    if(sum(!unique(data[,predictors[[item]]]) %in% valid_levels[[item]])>0)
    {
      stop(paste(item,"is not coded in its valid levels which are", valid_levels[item],". \n Observed levels are", paste(sort(unique(data[,predictors[[item]]])),collapse=",")))
    }
  }

  args <- list(data=data)
  args <- c(args, predictors)
  do.call(QRISK3_2017, args)

}




#Deprecated - replaced by cumulative incidence counterpart
# km_cal_plot <- function(data, pred_var="probs", formula=Surv(fu,CHD)~1, n_bin=10)
# {
#   breaks <- quantile(data$probs, probs=(0:n_bin)/n_bin)
#   breaks[1] <-0; breaks[length(breaks)] <-1
#   data$bin <- as.numeric(cut(data[[pred_var]], breaks=breaks))
#
#   cal_data <- km_prob_by_group(data=data, formula=formula, group_var="bin")
#
#   max_x <- max(cal_data$pred_risk,cal_data$obs_risk)
#   max_y <- max_x
#
#   plot(cal_data$pred_risk, cal_data$obs_risk, xlim=c(0,max_x), ylim=c(0,max_y), xlab="Predicted risk", ylab="Observed risk", col="red")
#   for(i in 1:(dim(cal_data)[1]))
#   {
#     lines(x=rep(cal_data$pred_risk[i],2), y=c(cal_data$obs_risk_l[i],cal_data$obs_risk_u[i]), col="blue")
#   }
#   lines(c(0,max(max_x,max_y)),c(0,max(max_x,max_y)), col="grey")
#
#   cal_data
# }



ci_gen_cal_data <- function(data, pred_var, ftime_var="fu", fstatus_var="CHD", n_bin=10)
{
  breaks <- quantile(data[[pred_var]], probs=(0:n_bin)/n_bin)
  breaks[1] <-0; breaks[length(breaks)] <-1
  data$bin <- as.numeric(cut(data[[pred_var]], breaks=breaks))

  cal_data <- ci_prob_by_group(data=data, pred_var=pred_var, ftime_var=ftime_var, fstatus_var=fstatus_var,  group_var="bin")

  cal_data
}



#Deprecated - replaced by cumulative incidence counterpart
# km_prob_by_group <- function(data, formula=Surv(fu,CHD)~1, group_var=NULL)
# {
#   #Proper survival analysis method
#   #Mean calibration
#
#   out <- data.frame(group=integer(), pred_risk=double(), obs_risk=double(), obs_risk_l=double(), obs_risk_u=double())
#
#   if(is.null(group_var))
#   {
#     data$temp<-0
#     group_var <- "temp"
#   }
#
#   levels <- sort(unique(data[[group_var]]))
#   index <- 1
#   for(level in levels)
#   {
#     dt <- data[which(data[[group_var]]==level),]
#     km <- survfit(formula=formula, data=dt)
#     lt <- max(which(km$time<=10))
#     obs_risk <- 1-km$surv[lt]
#     obs_risk_l <- 1-km$upper[lt]
#     obs_risk_u <- 1-km$lower[lt]
#
#     pred_risk <- mean(dt$probs)
#
#     out[index,] <- c(level, pred_risk, obs_risk, obs_risk_l, obs_risk_u)
#     index <- index+1
#   }
#
#   out
# }



#Cumulative incidence
ci_prob_by_group <- function(data, pred_var, ftime_var="fu", fstatus_var="CHD", group_var=NULL)
{
  out <- data.frame(group=integer(), pred_risk=double(), pred_risk_se=double(), obs_risk=double(), obs_risk_se=double())

  if(is.null(group_var))
  {
    data$temp<-0
    group_var <- "temp"
  }

  ci <- cuminc(ftime=data[[ftime_var]], fstatus = data[[fstatus_var]], group=data[[group_var]])

  nms <- names(ci)
  index <- 1
  for(nm in nms)
  {
    if(substr(nm,nchar(nm),nchar(nm))=="1") #Only interested in CHD
    {
      gp <- as.integer(strsplit(nm,split=" ")[[1]][1])
      lt <- max(which(ci[[nm]]$time<=10))
      obs_risk <- ci[[nm]]$est[lt]
      se <- sqrt(ci[[nm]]$var[lt])
      out[index,] <- c(gp, mean(data[which(data[[group_var]]==gp),][[pred_var]]), sd(data[which(data[[group_var]]==gp),][[pred_var]])/sqrt(sum(data[[group_var]]==gp)), obs_risk, se)
      index <- index+1
    }
  }

  out
}



incidence <- function(count_data, groups=c(), n_sim=10^4) #if n_sim>0 does CI calculations
{
  out <- list()

  count_data$fakegrp <- 1
  grp_phrase <- ifelse(length(groups)>0, paste(groups, collapse=","),"fakegrp")

  z <- sqldf(paste("SELECT ", grp_phrase,", SUM(n_event)/SUM(py) AS inc,
                   sum(ref_n_event)/sum(ref_py*1.00) AS ref_inc,
                   sum(ref_n_event*py/ref_py) AS e_n,
                   SUM(n_event) AS o_n
                   FROM count_data GROUP BY", grp_phrase))

  z$ir <- z$inc/z$ref_inc
  z$sir <- z$o_n/z$e_n
  out$incidence_stats <- z


  if(n_sim>0)
  {
    n_group <- dim(count_data)[1]
    count_data$igrp <- 1:n_group

    e_counts <- rep(count_data$n_event, n_sim)
    e_ref_counts <- rep(count_data$ref_n_event, n_sim)

    n_event <- rpois(n_group*n_sim, lambda=e_counts)
    ref_n_event <- rpois(n_group*n_sim, lambda=e_ref_counts)

    xx <- count_data[rep(seq_len(nrow(count_data)), n_sim),]
    xx$isim <- rep(1:n_sim, each=nrow(count_data))
    xx$n_event <- n_event
    xx$ref_n_event <- ref_n_event

    z <- sqldf(paste("SELECT isim,",grp_phrase,", SUM(n_event)/SUM(py) AS inc,
                     sum(ref_n_event)/sum(ref_py*1.00) AS ref_inc,
                     sum(ref_n_event*py/ref_py) AS e_n,
                     SUM(n_event) AS o_n FROM xx
                     GROUP BY isim,", grp_phrase,"ORDER BY isim"))

    z$ir <- z$inc/z$ref_inc
    z$sir <- z$o_n/z$e_n

    out$incidence_stats <- cbind(out$incidence_stats, z %>% group_by_at(groups) %>% summarise(sir_l=quantile(sir,0.025),sir_h=quantile(sir,0.75)))
  }

  out
}




DCA <- function(data, pred_var, formula=Surv(fu,CHD)~1, zs=(0:99)/100, weights=NULL)
{
  n <- dim(data)[1]
  if(is.null(weights)) weights <- rep(1,n)

  nb_model <- nb_all <- tps <- fps <- rep(NA,length(zs))

  km <- survfit(formula=formula, data=data, weights=weights)
  lt <- max(which(km$time<=10))
  p <- 1-km$surv[lt]#Equivalent to prevalence

  data$w <- weights

  for(i in 1:length(zs))
  {
    cat('.')
    z <- zs[i]
    dt <- data[which(data[[pred_var]]>=z),]
    if(dim(dt)[1]>0)
    {
      km <- survfit(formula=formula, data=dt, weights=dt$w)
      lt <- max(which(km$time<=10))
      tps[i] <- (1-km$surv[lt])*dim(dt)[1]/n
      fps[i] <- km$surv[lt]*dim(dt)[1]/n
      nb_model[i] <- tps[i]-fps[i]*z/(1-z)
    }
    else
    {
      nb_model[i] <- 0
    }
    nb_all[i] <- p-(1-p)*z/(1-z)
  }

  cbind(zs,nb_model,nb_all,0)
}



single_imputed_run <- function(data, pred_var)
{
  out <- list()

  #Mean calibration
  res <- ci_prob_by_group(data=data, pred_var=pred_var, group_var=NULL)
  out$all$pred_risk <- res$pred_risk
  out$all$pred_risk_se <- res$pred_risk_se
  out$all$obs_risk <- res$obs_risk
  out$all$obs_risk_se <- res$obs_risk_se
  res <- ci_gen_cal_data(data=data, pred_var=pred_var)
  out$all$cal_data <- res

  #By gender
  dt <- data[which(data$gender==0),]
  res <- ci_prob_by_group(dt, pred_var=pred_var, group_var=NULL)
  out$g0$pred_risk <- res$pred_risk
  out$g0$pred_risk_se <- res$pred_risk_se
  out$g0$obs_risk <- res$obs_risk
  out$g0$obs_risk_se <- res$obs_risk_se
  res <- ci_gen_cal_data(data=dt, pred_var=pred_var)
  out$g0$cal_data <- res

  dt <- data[which(data$gender==1),]
  res <- ci_prob_by_group(dt, pred_var=pred_var, group_var=NULL)
  out$g1$pred_risk <- res$pred_risk
  out$g1$pred_risk_se <- res$pred_risk_se
  out$g1$obs_risk <- res$obs_risk
  out$g1$obs_risk_se <- res$obs_risk_se
  res <- ci_gen_cal_data(data=dt, pred_var=pred_var)
  out$g1$cal_data <- res

  #By age
  dt <- data[which(data$patid_age<=65),]
  res <- ci_prob_by_group(dt, pred_var=pred_var, group_var=NULL)
  out$al$pred_risk <- res$pred_risk
  out$al$pred_risk_se <- res$pred_risk_se
  out$al$obs_risk <- res$obs_risk
  out$al$obs_risk_se <- res$obs_risk_se
  res <- ci_gen_cal_data(data=dt, pred_var=pred_var)
  out$al$cal_data <- res

  dt <- data[which(data$patid_age>65),]
  res <- ci_prob_by_group(dt, pred_var=pred_var, group_var=NULL)
  out$ah$pred_risk <- res$pred_risk
  out$ah$pred_risk_se <- res$pred_risk_se
  out$ah$obs_risk <- res$obs_risk
  out$ah$obs_risk_se <- res$obs_risk_se
  res <- ci_gen_cal_data(data=dt, pred_var=pred_var)
  out$ah$cal_data <- res

  out
}



#MI pooling. Expects the input to be same structure as single_imputed_run
#https://bookdown.org/mwheymans/bookmi/rubins-rules.html#pooling-effect-estimates
pool_results <- function(ls_res)
{
  out <- list()
  m <- length(ls_res)

  components <- c('all','g0','g1','al','ah')

  for(item in components)
  {
    #Predicted risk (easy as it is a  matter of averaging)
    thetas <- NULL
    v_w <- 0
    for(i in 1:m)
    {
      thetas <- cbind(thetas,ls_res[[i]][[item]]$pred_risk)
      v_w <- v_w+ls_res[[i]][[item]]$pred_risk_se^2/m
    }
    theta_hat <- mean(thetas)
    out[[item]]$pred_risk <- theta_hat

    v_b <- sum((thetas-theta_hat)^2)/(m-1)
    v_total <- v_w+v_b+v_b/m
    se <- sqrt(v_total)
    out[[item]]$pred_risk_se <- se


    #Observed risk
    thetas <- NULL
    v_w <- 0
    for(i in 1:m)
    {
      thetas <- c(thetas,ls_res[[i]][[item]]$obs_risk)
      v_w <- v_w+ls_res[[i]][[item]]$obs_risk_se^2/m
    }
    theta_hat <- mean(thetas)
    v_b <- sum((thetas-theta_hat)^2)/(m-1)

    v_total <- v_w+v_b+v_b/m
    se <- sqrt(v_total)

    out[[item]]$observed_risk <- theta_hat
    out[[item]]$observed_risk_se <- se

    #cal_data
    thetas_pred <- thetas_obs<- NULL
    v_w <- 0 #For observed
    for(i in 1:m)
    {
      thetas_pred <- cbind(thetas_pred,ls_res[[i]][[item]]$cal_data[,'pred_risk'])
      thetas_obs <- cbind(thetas_obs,ls_res[[i]][[item]]$cal_data[,'obs_risk'])
      v_w <- v_w+ls_res[[i]][[item]]$cal_data[,'obs_risk_se']^2/m
    }
    thetas_pred_hat <- rowMeans(thetas_pred)
    thetas_obs_hat <- rowMeans(thetas_obs)
    v_b <- rowSums((thetas_obs-thetas_obs_hat)^2)/(m-1)

    v_total <- v_w+v_b+v_b/m
    se <- sqrt(v_total)

    out[[item]]$cal_data <- as.data.frame(cbind(group=ls_res[[1]][[item]]$cal_data[,'group'], pred_risk=thetas_pred_hat, obs_risk=thetas_obs_hat, obs_risk_se=se))
  }

  out
}



plot_cal <-function(cal_data)
{
  max_x <- max(cal_data$pred_risk,cal_data$obs_risk)
  max_y <- max_x

  plot(cal_data$pred_risk, cal_data$obs_risk, pch=16, xlim=c(0,max_x), ylim=c(0,max_y), xlab="Predicted risk", ylab="Observed risk", col="red")
  for(i in 1:(dim(cal_data)[1]))
  {
    lines(x=rep(cal_data$pred_risk[i],2), lwd=2, y=c(cal_data$obs_risk[i]-1.96*cal_data$obs_risk_se[i],cal_data$obs_risk[i]+1.96*cal_data$obs_risk_se[i]), col="blue")
  }
  lines(c(0,max(max_x,max_y)),c(0,max(max_x,max_y)), col="grey")

}


#c('recal_hr', 'recal_lin', 'recal_linsa', 'dca', 'counterpred')

run <- function(save_file=NULL, analyses=c())
{
  out <- list()

  data_raw <- read_sas_data(save=T)
  data_prepared <- prepare_data(data_raw)

  #Calculate incidence etc
  data_prepared$age_group <- floor(data_prepared$patid_age/5)*5

  x <- sqldf("SELECT gender, age_group, SUM(fu) AS py, SUM(CHD) AS n_event FROM data_prepared GROUP BY gender, age_group ORDER BY gender, age_group")
  #x <- data_prepared  %>% group_by(gender, age_group) %>% summarise(py=sum(fu), n_event=sum(CHD)) %>% arrange(gender, age_group)

  y <- hc_numbers %>% arrange(gender, age_group)

  x$ref_py <- y$py
  x$ref_n_event <- y$n_event

  out$full_table <- x

  out$incidence_stats <- incidence(x, groups=c())
  out$incidence_stats_by_gender <- incidence(x, groups=c('gender'))
  x$age_cat <- (x$age_group>65)
  out$incidence_stats_by_age <- incidence(x, groups=c('age_cat'))
  out$incidence_stats_by_gender_age <- incidence(x, groups=c('gender','age_cat'))


  #Multiple imputation

  data_mice <- impute_data(data_prepared)

  data_imputed_all <- NULL
  for(i in 1:5)
  {
    tmp <- mice::complete(data_mice, action=i)
    tmp$copy <- i
    #Bring back the variables left behind
    tmp <- cbind(patid=data_prepared$patid, CHD=data_prepared$CHD, fu=data_prepared$fu, tmp)
    #Do the predictions!
    res <- my_qrisk(tmp)
    tmp$probs <- res$QRISK3_2017/100
    #Remove bad data
    bads <- unique(c(which(tmp$fu<0.01), which(is.na(tmp$probs)), which(tmp$probs==1)))
    out$bads <- tmp$patid[bads]
    if(length(bads)>0) tmp <- tmp[-bads,]

    data_imputed_all <- rbind(data_imputed_all, tmp)
  }
  rm(tmp)
  data_imputed_all$lt <- log(-log(1-data_imputed_all$probs))


  res <- list()
  for(i in 1:5)
  {
    res[[i]] <- single_imputed_run(data=data_imputed_all[which(data_imputed_all$copy==i),], pred_var="probs")
  }
  res_pooled <- pool_results(res)

  out$nocal$res_pooled <- res_pooled




  if("recal_hr" %in% analyses)
  {
    ### Recalibration: intercept only (hazard ratio)
    #TODO: Competing risk is not properly implemented here
    if(competing_risk)
    {

    }else
    {
      res <- ci_prob_by_group(data_imputed_all, pred_var="probs", group_var=NULL)
      hr <- log(1-res$obs_risk)/log(1-res$pred_risk)
      out$recal_hr$hr <- hr
    }

    data_imputed_all$lt_recal_hr <- data_imputed_all$lt + log(hr)
    data_imputed_all$probs_recal_hr <- 1-exp(-exp(data_imputed_all$lt_recal_hr))

    plot(data_imputed_all$probs, data_imputed_all$probs_recal_hr)
    lines(c(0,1),c(0,1))

    res <- list()
    for(i in 1:5)
    {
      res[[i]] <- single_imputed_run(data=data_imputed_all[which(data_imputed_all$copy==i),], pred_var="probs_recal_hr")
    }
    res_pooled_recal_hr <- pool_results(res)
    out$recal_hr$res_pooled <- res_pooled_recal_hr
  }



  ### Recalibration: probability ratio (investigational - not part of final analysis [we get some p>1)])
  #TODO: Competing risk is not properly implemented here
  # if(competing_risk)
  # {
  #
  # }else
  # {
  #   res <- ci_prob_by_group(data_imputed_all, pred_var="probs", group_var=NULL)
  #   rr <- res$obs_risk/res$pred_risk
  # }
  #
  # data_imputed_all$lt_recal_rr <- data_imputed_all$lt + log(rr)
  # data_imputed_all$probs_recal_rr <- pmin(data_imputed_all$probs*rr,0.999)
  #
  # plot(data_imputed_all$probs, data_imputed_all$probs_recal_rr)
  # lines(c(0,1),c(0,1))
  #
  #
  # res <- list()
  # for(i in 1:5)
  # {
  #   res[[i]] <- single_imputed_run(data=data_imputed_all[which(data_imputed_all$copy==i),], pred_var="probs_recal_rr")
  # }
  # res_pooled_recal_rr <- pool_results(res)
  #
  # for(item in names(res_pooled_recal_rr))
  # {
  #   plot_cal(res_pooled_recal_rr[[item]]$cal_data)
  #   title(item)
  # }


  if("recal_lin" %in% analyses)
  {
    ### Recalibration: linear
    data_imputed_all$lt <- log(-log(1-data_imputed_all$probs))

    #TODO: Competing risk is not properly implemented here
    if(competing_risk)
    {
      reg_calib <- FGR(Hist(fu,CHD)~lt, data=data_imputed_all[which(data_imputed_all$fu<=max_fu),], cause = 1)
      b1<-unname(reg_calib$crrFit$coef)
      lt <- which.max(reg_calib$crrFit$uftime[which(reg_calib$crrFit$uftime<=10)])
      b0 <- sum(reg_calib$crrFit$bfitj[1:lt])
    }else
    {
      reg_calib <- coxph(Surv(fu,CHD)~lt, data=data_imputed_all[which(data_imputed_all$fu<=max_fu),])
      b1<-unname(coefficients(reg_calib))
      dt0 <- data_imputed_all[1,]
      dt0$lt <- 0
      dt0$fu <-10
      b0 <- predict(reg_calib, newdata=dt0, type="expected")
      out$recal_lin$bs <- c(b0=0, b1=b1)
    }

    data_imputed_all$lt_recal_lin <- log(b0) + data_imputed_all$lt*b1
    data_imputed_all$probs_recal_lin <- 1-exp(-exp(data_imputed_all$lt_recal_lin))

    plot(data_imputed_all$probs, data_imputed_all$probs_recal_lin)
    lines(c(0,1),c(0,1))

    res <- list()
    for(i in 1:5)
    {
      res[[i]] <- single_imputed_run(data=data_imputed_all[which(data_imputed_all$copy==i),], pred_var="probs_recal_lin")
    }
    res_pooled_recal_lin <- pool_results(res)
    out$recal_lin$res_pooled <- res_pooled_recal_lin
  }



  if("recal_linsa" %in% analyses)
  {
    ### Recalibration: linear + sex and age
    reg_calib <- coxph(Surv(fu,CHD)~lt+gender+patid_age, data=data_imputed_all)
    b1s<-(coefficients(reg_calib))

    bhs <- basehaz(reg_calib, centered = F)
    b0 <- unname(bhs$hazard[which.min(abs(bhs$time-10))])

    data_imputed_all$lt_recal_linsa <- log(b0) + data_imputed_all$lt*b1s['lt'] + data_imputed_all$gender*b1s['gender'] + data_imputed_all$patid_age*b1s['patid_age']
    data_imputed_all$probs_recal_linsa <- 1-exp(-exp(data_imputed_all$lt_recal_linsa))

    plot(data_imputed_all$probs, data_imputed_all$probs_recal_linsa)
    lines(c(0,1),c(0,1))

    out$recal_linsa$bs <- c(b0=b0, b1s)

    res <- list()
    for(i in 1:5)
    {
      res[[i]] <- single_imputed_run(data=data_imputed_all[which(data_imputed_all$copy==i),], pred_var="probs_recal_linsa")
    }
    res_pooled_recal_linsa <- pool_results(res)
    out$recal_linsa$res_pooled <- res_pooled_recal_linsa
  }



  ### Reestimation (without complexities like interactions etc)
  # data_recal_full_restimation <- cbind(data.frame(patid=data_prepared$patid, CHD=data_prepared$CHD, fu=data_raw$FU_days/365), this_data)
  # data_recal_full_restimation <- data_recal_full_restimation[-bads,]
  #
  # fm <- paste("Surv(fu,CHD)~",paste(predictor_mapping[-1],collapse="+"))
  # reg <- coxph(as.formula(fm), data=data_recal_full_restimation)
  #
  # ndt <- data_recal_full_restimation
  # ndt$fu <- 10
  # preds <- predict(reg,newdata = ndt, type="expected")
  # data_recal_full_restimation$probs <- 1-exp(-preds)
  #
  # plot(data_final$probs, data_recal_full_restimation$probs)
  # lines(c(0,1),c(0,1))
  #
  # res <- ci_prob_by_group(data_recal_full_restimation)
  # cat(rel_bias <- res$obs_risk/res$pred_risk)
  # ci_gen_cal_data(data=data_recal_full_restimation)





  if("dca" %in% analyses)
  {
    ##Net benefit
    dca <- DCA(data_imputed_all, pred_var="probs", zs=c(0:50)/100)
    out$nocal$dca <- dca
    dca_recal_hr <- DCA(data_imputed_all, pred_var="probs_recal_hr", zs=c(0:50)/100)
    out$recal_hr$dca <- dca_recal_hr
    dca_recal_lin <- DCA(data_imputed_all, pred_var="probs_recal_lin", zs=c(0:50)/100)
    out$recal_lin$dca <- dca_recal_lin
    dca_recal_linsa <- DCA(data_imputed_all, pred_var="probs_recal_linsa", zs=c(0:50)/100)
    out$recal_linsa$dca <- dca_recal_linsa
  }


  if("counterpred" %in% analyses)
  {
    out$counterpred <- counterfactual_pred(data_imputed_all)
  }


  if(!is.null(save_file)) saveRDS(out,save_file)
  message("Output was saved into", getwd(), "/", save_file)

  out
}



#res is the output object (list) from run()
display_results <- function(res)
{
  print(paste("Bad IDs:",paste(res$bads,collapse=",")))

  print(paste("Pred risk:", res$nocal$res_pooled$all$observed_risk,"; SE:",res$nocal$res_pooled$all$pred_risk_se))
  print(paste("Observed risk:", res$nocal$res_pooled$all$observed_risk,"; SE:",res$nocal$res_pooled$all$observed_risk_se))

  print("#################################")

  print("Original model")
  for(item in names(res$nocal$res_pooled))
  {
    print(paste("Pred risk:", res$nocal$res_pooled[[item]]$pred_risk, "SE:", res$nocal$res_pooled[[item]]$pred_risk_se))
    print(paste("Observed risk:", res$nocal$res_pooled[[item]]$observed_risk,"; SE:",res$nocal$res_pooled[[item]]$observed_risk_se))
    plot_cal(res$nocal$res_pooled[[item]]$cal_data)
    title(item)
  }

  if(length(res$recal_hr)>0)
  {
    print("Simple reclibration (HR)")
    for(item in names(res$recal_hr$res_pooled))
    {
      print(paste("Pred risk:", res$nocal$res_pooled[[item]]$pred_risk, "SE:", res$nocal$res_pooled[[item]]$pred_risk_se))
      print(paste("Observed risk:", res$recal_hr$res_pooled[[item]]$observed_risk,"; SE:",res$recal_hr$res_pooled[[item]]$observed_risk_se))
      plot_cal(res$recal_hr$res_pooled[[item]]$cal_data)
      title(item)
    }
  }

  if(length(res$recal_lin)>0)
  {
    print("Linear reclibration")
    for(item in names(res$recal_lin$res_pooled))
    {
      print(paste("Pred risk:", res$nocal$res_pooled[[item]]$pred_risk, "SE:", res$nocal$res_pooled[[item]]$pred_risk_se))
      print(paste("Observed risk:", res$recal_lin$res_pooled[[item]]$observed_risk,"; SE:",res$recal_lin$res_pooled[[item]]$observed_risk_se))
      plot_cal(res$recal_lin$res_pooled[[item]]$cal_data)
      title(item)
    }
  }

  if(length(res$recal_linsa)>0)
  {
    print("Linear reclibration + sex & age effects")
    print(paste("Pred risk:", res$nocal$res_pooled$all$pred_risk))
    for(item in names(res$recal_linsa$res_pooled))
    {
      print(paste("Pred risk:", res$nocal$res_pooled[[item]]$pred_risk, "SE:", res$nocal$res_pooled[[item]]$pred_risk_se))
      print(paste("Observed risk:", res$recal_linsa$res_pooled[[item]]$observed_risk,"; SE:",res$recal_linsa$res_pooled[[item]]$observed_risk_se))
      plot_cal(res$recal_linsa$res_pooled[[item]]$cal_data)
      title(item)
    }
  }

  if(length(res$dca)>0)
  {
    lwd <- 2
    max_nb <- 0.3
    plot(res$nocal$dca[,1],res$nocal$dca[,2], type='l', xlim=c(0,max_nb), lwd=lwd, xlab="Risk threshold", ylab="Net benefit")
    lines(c(0,max_nb),c(0,0), type='l', xlim=c(0,max_nb), col='gray', lwd=1)
    lines(res$nocal$dca[,1],res$nocal$dca[,3], type='l', xlim=c(0,max_nb), col='gray', lwd=1)
    lines(res$recal_hr$dca[,1],res$recal_hr$dca[,2], type='l', xlim=c(0,max_nb), col='green', lwd=lwd)
    lines(res$recal_lin$dca[,1],res$recal_lin$dca[,2], type='l', xlim=c(0,max_nb), col='blue', lwd=lwd)
    lines(res$recal_linsa$dca[,1],res$recal_linsa$dca[,2], type='l', xlim=c(0,max_nb), col='red', lwd=lwd)
    legend(0, 0.1, legend=c("Original", "Simple", "Log-linear", "Log-linear+SA", "No model"),
           col=c("Black", "Green", "blue", "Red", "Gray"), lty=rep(1,4), cex=0.8)
  }
}



dca_monte_carlo <- function(data, n_sim=100, zs=c(0:30)/100)
{
  out <- NULL;
  n <- dim(data)[1]
  for(i in 1:n_sim)
  {
    tmp <- DCA(data, pred_var="probs", zs=zs, weights = bootstrap(n))
    out <- rbind(out, tmp); cat(i,'\n')
  }

  out
}




## 2023.05.04 counterfactual preds

counterfactual_pred <- function(data)
{
  valid_levels <- list(gender=c(0,1),
                       atrial_fibrillation=c(0,1),
                       atypical_antipsy=c(0,1),
                       regular_steroid_tablets=c(0,1),
                       erectile_disfunction=c(0,1),
                       migraine=c(0,1),
                       rheumatoid_arthritis=c(0,1),
                       chronic_kidney_disease=c(0,1),
                       severe_mental_illness=c(0,1),
                       systemic_lupus_erythematosis=c(0,1),
                       blood_pressure_treatment=c(0,1),
                       diabetes1=c(0,1),
                       diabetes2=c(0,1),
                       ethiniciy=(1:9),
                       smoke=(1:5),
                       townsend=(1:5),
                       age=unique(data$patid_age)
  )





  out <- list()
  real_out <- data.frame(variable=character(),low=double(),high=double())
  for(i in 1:length(valid_levels))
  {
    x <- data
    var_name <- names(valid_levels[i])
    out[[var_name]][["levels"]] <- valid_levels[[i]]
    for(j in 1:length(valid_levels[[i]]))
    {
      var_level <- valid_levels[[i]][j]
      out[[var_name]][["freqs"]] <- c(out[[var_name]][["freqs"]],  sum(data[,predictor_mapping[[var_name]]]==var_level))
      cat(paste0('Name:',var_name,"|level:",var_level))
      x[,predictor_mapping[[var_name]]] <- var_level
      y <- my_qrisk(x)
      out[[var_name]][["preds"]] <- c(out[[var_name]][["preds"]],  mean(y$QRISK3_2017))
    }

    tmp <- cbind(out[[var_name]]$levels,out[[var_name]]$freqs,out[[var_name]]$preds)
    o <- order(tmp[,3])
    tmp <- as.data.frame(tmp[o,])
    cs <- cumsum(tmp[,2])/sum(tmp[,2])
    cutoff <- which.min(abs(cs-0.5))
    if(cs[cutoff]>0.5) cutoff <-cutoff-1
    lows <- tmp[1:cutoff,]
    highs <- tmp[-(1:cutoff),]
    low <- sum(lows[,2]*lows[,3])/sum(lows[,2])
    high <- sum(highs[,2]*highs[,3])/sum(highs[,2])

    out[[var_name]][["contrast"]] <- c(low, high)
    real_out <- rbind(real_out,c(var_name,low,high))
  }

  real_out
}


