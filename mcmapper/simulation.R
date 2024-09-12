library(tidyverse)
library(mcmapper)
library(pROC)

set.seed(2024)

prevs <- seq(from = 0.01, to = 0.50, by = 0.01)
c_stats <- seq(from = 0.51, to = 0.99, by = 0.01)

se_mc <- function(n, m, c)
{
  se_m <- sqrt(m*(1-m)/n)
  se_c <- sqrt(c*(1-c)*((1+(n/2-1)*((1-c)/(2-c)))+((n/2-1)*c)/(1+c))/(n^2*m*(1-m)))

  c(se_m=se_m, se_c=se_c)
}



# Define the function to solve for n
solve_for_n <- function(m, c, se_m, se_c) {
  n_m <- m*(1-m)/se_m^2

  # Calculate the coefficients of the quadratic equation
  A <- se_c^2 * m * (1 - m)
  B <- -0.5 * c * (1 - c) * ((1 - c) / (2 - c) + c / (1 + c))
  C <- -c * (1 - c) * (1 - (1 - c) / (2 - c) - c / (1 + c))

  # Check if the discriminant is non-negative
  discriminant <- B^2 - 4 * A * C
  if (discriminant < 0) {
    return("No real solution")
  }

  # Use the quadratic formula to solve for n
  n1 <- (-B + sqrt(discriminant)) / (2 * A)
  n2 <- (-B - sqrt(discriminant)) / (2 * A)

  # Return the solutions
  n_c <-max(n1, n2)

  c(n_m=n_m, n_c=n_c)
}


param_grid <- expand.grid(prev = prevs, c_stat = c_stats,type=c("beta","logitnorm",'probitnorm'))

if(file.exists("results/algos_sol.rds")){
  calculate_sol <- F
} else{
  calculate_sol <- T
}

if(calculate_sol){

    tmp_sol <- apply(param_grid, 1, function(tmp_row) {
      c(tmp_row,as.numeric(mcmap(as.numeric(tmp_row[1:2]),tmp_row[3])$value))
    }) %>%
      t() %>%
      as.data.frame() %>%
      mutate(across(c(1:2,4:5),as.numeric))

    # sanity-check: unique values
    tmp_sol %>%
      group_by(type) %>%
      distinct(V4,V5) %>%
      tally() -> sanity_check

    sanity_check

  write_rds(tmp_sol,
            "results/algos_sol.rds")
}

df_algo_sol <- read_rds("results/algos_sol.rds") %>%
  rename(arg1=V4,
         arg2=V5)

sim_dir <- "sim_results"
dir.create(sim_dir)

n_outer <- 1

se <- function(x) {sqrt(var(x)/length(x))}

choose_ralgo <- function(type){
  ifelse(type=="logitnorm", rlogitnorm,
         ifelse(type=="probitnorm", rprobitnorm,
                ifelse(type=="beta",rbeta,NA)
         )
  )
}

alpha_coverage <- 0.90
alpha <- (1-alpha_coverage)/2
qcrit <- abs(qnorm(alpha))

# calculate_sample <- function( prev,c_stat,CV = 0.01){
#   target_se <- CV * c_stat
#
#   riley_N <- function(N){
#     numer <- c_stat* (1-c_stat) * (1+(N/2-1)*((1-c_stat)/(2-c_stat))+ (N/2-1)*c_stat / (1+c_stat) )
#     denom <- N^2 * prev * (1-prev)
#     # print(sqrt(numer/denom))
#     target_se - sqrt(numer/denom)
#   }
#
#     rootSolve::uniroot.all(riley_N,c(1,10000000),trace=0)
#
#   }

n_eff_size <- 100

# parallelization on mac
library(doParallel)
library(foreach)
registerDoParallel(cl <- makeCluster(7))

foreach(i=1:nrow(df_algo_sol),.combine = "+") %dopar% {

  library(mcmapper)
  set.seed(i)

  tmp_pi_check <- tmp_cstat_check <- c()
  tmp_row <- df_algo_sol[i,]
  tmp_true <- c(tmp_row$prev,tmp_row$c_stat)
  ralgo <- choose_ralgo(tmp_row$type)
  inner_result <- data.frame(pi=numeric(),
                             pi_se=numeric(),
                             pi_coverage=logical(),
                             c_stat=numeric(),
                             c_stat_lower=numeric(),
                             c_stat_upper=numeric(),
                             c_stat_coverage=logical())

  n_inner <- ceiling(as.numeric(1/tmp_row$prev*n_eff_size))

  for(j in 1:n_outer){

    tmp_pi <- ralgo(n_inner,tmp_row$arg1,tmp_row$arg2)
    tmp_pi_mean <- mean(tmp_pi)
    tmp_pi_se <- se(tmp_pi)
    tmp_pi_CI <- c(tmp_pi_mean-qcrit*tmp_pi_se,tmp_pi_mean + qcrit*tmp_pi_se)
    tmp_Y <- rbinom(n_inner,1,tmp_pi)

    if(length(unique(tmp_Y))!=2){
      next
    }

    tmp_roc <- pROC::roc(tmp_Y~tmp_pi, ci=T,quiet=T,ci.method = c("delong"),conf.level=alpha_coverage)
    tmp_cstat <- as.vector(tmp_roc$ci)
    tmp_cstat_mean <- tmp_cstat[2]
    tmp_cstat_CI <- c(tmp_cstat[1],tmp_cstat[3])

    tmp_pi_check <- as.numeric((tmp_true[1]<=tmp_pi_CI[2]) & (tmp_true[1]>=tmp_pi_CI[1]))
    tmp_cstat_check <- as.numeric((tmp_true[2]<=tmp_cstat_CI[2]) & (tmp_true[2]>=tmp_cstat_CI[1]))

    inner_result[j,] <- c(tmp_pi_mean,tmp_pi_se,tmp_pi_check,
                          tmp_cstat_mean,tmp_cstat[1],tmp_cstat[3],tmp_cstat_check)
  }

  res <- c(unlist(tmp_row),coverage_prob_arg1=mean(inner_result$pi_coverage,na.rm=T),coverage_prob_arg2=mean(inner_result$c_stat_coverage,na.rm=T))
  readr::write_rds(res,paste0(sim_dir,"/",i,"_summary.rds"))
  readr::write_rds(inner_result,paste0(sim_dir,"/",i,"_detailed.rds"))
  return(0)
}

stop(cl)

# process results ---------------------------------------------------------

sim_files <- list.files(sim_dir)[str_detect(list.files(sim_dir),"summary")]

sim_results <- lapply(sim_files,function(tmp_sim){
  read_rds(paste0(sim_dir,"/",tmp_sim))
}) %>%
  do.call(rbind,.) %>%
  as.data.frame() %>%
  mutate(across(c(prev:c_stat,arg1:coverage_prob_arg2),as.numeric)) %>%
  mutate(coverage_prob_arg1=round(coverage_prob_arg1,2),
         coverage_prob_arg2 = round(coverage_prob_arg2,2)) %>%
  arrange(type) %>%
  filter(coverage_prob_arg1!=0) %>%
  mutate(type = case_when(type=="beta" ~ "Beta",
                          type == "logitnorm" ~ "Logit-Normal",
                          type == "probitnorm" ~ "Probit-Normal",
                          TRUE ~ NA)) %>%
  pivot_longer(cols=6:7,names_to="parameter",values_to="Coverage probability") %>%
  mutate(parameter = ifelse(parameter=="coverage_prob_arg1",
                            "Coverage for prevalence",
                            "Coverage for c-statistic"),
         parameter = factor(parameter,levels=c("Coverage for prevalence","Coverage for c-statistic")))

write_rds(sim_results,"results/simulation_results_once.rds")

# detailed info -----------------------------------------------------------
# # compute median

sim_files <- list.files(sim_dir)[str_detect(list.files(sim_dir),"detailed")]

sim_results_detailed <- lapply(sim_files,function(tmp_sim){
  tmp_index <-read_rds(paste0(sim_dir,"/", parse_number(tmp_sim),"_summary.rds"))
  read_rds(paste0(sim_dir,"/",tmp_sim)) %>%
    summarise(med_pi = median(pi),
           med_c = median(c_stat)) -> tmp_vals
  tmp_index$med_pi <- tmp_vals$med_pi
  tmp_index$med_c <- tmp_vals$med_c
  tmp_index
}) %>%
  do.call(rbind,.) %>%
  as.data.frame() %>%
  mutate(across(c(prev:c_stat,arg1:med_c),as.numeric)) %>%
  mutate(coverage_prob_arg1=round(coverage_prob_arg1,2),
         coverage_prob_arg2 = round(coverage_prob_arg2,2),
         med_pi = round(med_pi,5),
         med_c = round(med_c,5)) %>%
  arrange(type) %>%
  mutate(diff_pi = prev - med_pi,
         diff_c = c_stat - med_c) %>%
  filter(coverage_prob_arg1!=0) %>%
  mutate(type = case_when(type=="beta" ~ "Beta",
                          type == "logitnorm" ~ "Logit-Normal",
                          type == "probitnorm" ~ "Probit-Normal",
                          TRUE ~ NA)) %>%
  pivot_longer(cols=10:11,names_to="parameter",values_to="difference") %>%
  mutate(parameter = ifelse(parameter=="diff_pi",
                            "Difference in prevalence",
                            "Difference in c-statistic"),
         parameter = factor(parameter,levels=c("Difference in prevalence","Difference in c-statistic")))

write_rds(sim_results_detailed,"results/simulation_results_detailed_once.rds")


sim_dir <- "sim_results"
sim_results <- read_rds("results/simulation_results.rds")

ggplot(data=sim_results,aes(x=prev,y=c_stat,fill=`Coverage probability`))+
  geom_tile() +
  facet_grid(parameter~type)+
  theme_classic() +
  xlab("Prevalence") +
  ylab("C-statistic") +
  # scale_fill_gradient2(low="orange",mid="white",high='red',limits=c(0.8,1),
  #                      midpoint=0.9,breaks=c(0.8,0.9,1))+
  # "BrBG"
scale_fill_gradient(limits=c(0.8,1))+
  theme(legend.title = element_text("Coverage probability"),
        text = element_text(size=15)) -> fig_sim

ggsave("figures/fig_sim.jpeg",plot=fig_sim,device = "jpg")

sim_results_detailed <- read_rds("results/simulation_results_detailed.rds")

ggplot(data=sim_results_detailed,aes(x=prev,y=c_stat,fill=difference))+
  geom_tile() +
  facet_grid(parameter~type)+
  theme_classic() +
  xlab("Prevalence") +
  ylab("C-statistic") +
  # scale_fill_gradient2(low="orange",mid="white",high='red',
  #                      limits=c(-0.05,0.05),
  #                      midpoint=0,breaks=c(-0.05,0,0.05))+
  # "BrBG"
  # scale_fill_gradient(limits=c(-0.02,0.02))+
  theme(legend.title = element_text("Difference"),
        text = element_text(size=15)) -> fig_sim

ggsave("figures/fig_sim_median.jpeg",plot=fig_sim,device = "jpg")
