library(tidyverse)
library(mcmapper)
library(pROC)
library(RSSthemes)
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

  max(c(n_m=n_m, n_c=n_c))
}


param_grid <- expand.grid(prev = prevs, c_stat = c_stats,type=c("beta","logitnorm",'probitnorm'))

if(file.exists("results/algos_sol.rds")){
  calculate_sol <- F
} else{
  calculate_sol <- T
}

dir.create("mcmapper/results")

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
            "mcmapper/results/algos_sol.rds")
}

df_algo_sol <- read_rds("mcmapper/results/algos_sol.rds") %>%
  rename(arg1=V4,
         arg2=V5)

sim_dir <- "mcmapper/sim_results"
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

# parallelization on mac
library(doParallel)
library(foreach)
registerDoParallel(cl <- makeCluster(7))
SE <- 0.001

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

  n_inner <- ceiling(solve_for_n(tmp_row$prev,tmp_row$c_stat,SE,SE))

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

  res <- c(unlist(tmp_row),
           pi_diff = tmp_row$prev - tmp_pi_mean,
           cstat_diff = tmp_row$c_stat - tmp_cstat_mean,
           pi_est = tmp_pi_mean,
           pi_est_se = tmp_pi_se,
           cstat_mean = tmp_cstat_mean,
           cstat_lower = tmp_cstat[1],
           cstat_upper = tmp_cstat[3])
  readr::write_rds(res,paste0(sim_dir,"/",i,".rds"))
  return(0)
}

stop(cl)

# process results ---------------------------------------------------------

sim_files <- list.files(sim_dir)

sim_results_detailed <- lapply(sim_files,function(tmp_sim){
  read_rds(paste0(sim_dir,"/",tmp_sim))
}) %>%
  do.call(rbind,.) %>%
  as.data.frame() %>%
  mutate(across(c(prev:c_stat,arg1:cstat_upper),as.numeric)) %>%
  arrange(type) %>%
  mutate(type = case_when(type=="beta" ~ "Beta",
                          type == "logitnorm" ~ "Logit-Normal",
                          type == "probitnorm" ~ "Probit-Normal",
                          TRUE ~ NA)) %>%
  pivot_longer(cols=6:7,names_to="parameter",values_to="difference") %>%
  mutate(parameter = ifelse(parameter=="pi_diff",
                            "Difference in prevalence",
                            "Difference in c-statistic"),
         parameter = factor(parameter,levels=c("Difference in prevalence","Difference in c-statistic")))

write_rds(sim_results_detailed,"mcmapper/results/simulation_results.rds")

sim_results_detailed <- read_rds("mcmapper/results/simulation_results.rds")

to_math <- as_labeller(c(`mmm`="m-hat(m)",`ccc` = "c-hat(c)"),label_parsed)
to_math <- as_labeller(c(`mmm`="1",`ccc` = "2"))

ggplot(data=sim_results_detailed %>%
         mutate(rel_diff = ifelse(parameter=="Difference in prevalence",
                                  difference,
                                  difference),
                parameter = ifelse(parameter == "Difference in prevalence",
                                   "mmm",
                                   "ccc"),
                parameter = factor(parameter,
                                   levels=c("mmm","ccc"),
                                   labels=c("mmm","ccc"))),
       aes(x=prev,y=c_stat,fill=rel_diff))+
  geom_tile() +
  facet_grid(parameter~type,labeller = labeller(parameter = to_math)) +
  theme_classic() +
  xlab("m") +
  ylab("c") +
  scale_fill_gradient2(name = "") +
  theme_significance(base_size = 25) +
  theme(legend.key.height = unit(0.5, "cm"),
        legend.key.width =  unit(2, "cm")) -> fig_sim

dir.create("mcmapper/figures")
ggsave("mcmapper/figures/fig_sim.jpeg",plot=fig_sim,device = "jpg")
