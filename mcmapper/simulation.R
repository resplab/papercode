# Simulation analysis
# Make sure the working directly is the same as this file's location

# Load required packages
library(tidyverse)
library(mcmapper)
library(pROC)
library(RSSthemes)
library(doParallel)
library(foreach)

# Set a seed for reproducibility
set.seed(2024)

# Register 7 cores for parallelization
# If you have less than 7 cores, use a smaller number
registerDoParallel(cl <- makeCluster(7))

# Define the function to solve for the number of samples n
# required to obtain the desired level of standard error, se_m and se_c,
# for given prevalence m and c-statistic c, respectively
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

  # Return the the max of n1 and n2
  n_c <-max(n1, n2)
  max(c(n_m=n_m, n_c=n_c))
}

# Define a grid of prevalence and c-statistic for simulation analysis
# by three two-parameter distributions: beta, logit-norm, and probit-norm
prevs <- seq(from = 0.01, to = 0.50, by = 0.01)
c_stats <- seq(from = 0.51, to = 0.99, by = 0.01)
param_grid <- expand.grid(prev = prevs,
                          c_stat = c_stats,
                          type=c("beta","logitnorm",'probitnorm'))

# Create directories to save results and figures
dir.create("./results")
dir.create("./figures")

# If the solution already exists, do not calculate the again.
# This is provided.
if(file.exists("./results/algos_sol.rds")){
  calculate_sol <- F
} else{
  calculate_sol <- T
}

# If you want to re-calculate the solution, set calculate_sol to FALSE
if(calculate_sol){

  # For each row of the grid, apply mcmap function to obtain the solution
  tmp_sol <- apply(param_grid, 1, function(tmp_row) {
    c(tmp_row,as.numeric(mcmap(as.numeric(tmp_row[1:2]),tmp_row[3])$value))
  }) %>%
    t() %>%
    as.data.frame() %>%
    mutate(across(c(1:2,4:5),as.numeric))

  # sanity-check: the solutions should be unique
  sanity_check <- tmp_sol %>%
    group_by(type) %>%
    distinct(V4,V5) %>%
    tally()

  # Save the solution
  write_rds(tmp_sol,
            "./results/algos_sol.rds")
}

# Load the solution
df_algo_sol <- read_rds("./results/algos_sol.rds") %>%
  rename(arg1=V4,
         arg2=V5)

# Define function to calculate the standard error
se <- function(x) {sqrt(var(x)/length(x))}

# Define  function to choose the sampling algorithm
# given the type of distribution
choose_ralgo <- function(type){
  ifelse(type=="logitnorm", rlogitnorm,
         ifelse(type=="probitnorm", rprobitnorm,
                ifelse(type=="beta",rbeta,NA)
         )
  )
}

# Define significance level alpha
alpha_coverage <- 0.90
alpha <- (1-alpha_coverage)/2
qcrit <- abs(qnorm(alpha))

# Define the desired threshold of standard error in both
# prevalence m and c-statistic c in the simulation analysis
SE <- 0.001

# Create a folder to store intermediate simulation results
sim_dir <- "./sim_results"
dir.create(sim_dir)

# Perform simulation by parallelization
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


# Process the results
sim_files <- list.files(sim_dir)

sim_results <- lapply(sim_files,function(tmp_sim){
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

# Save the processed simulation results
write_rds(sim_results,"./results/simulation_results.rds")

# Load the processed simulation results
sim_results <- read_rds("./results/simulation_results.rds")

# Helper functions for relabeling the figure
# to_math <- as_labeller(c(`mmm`="m-hat(m)",`ccc` = "c-hat(c)"),label_parsed)
to_math <- as_labeller(c(`mmm`="Difference in m",`ccc` = "Difference in c"))

# Generate the figure used in the manuscript
ggplot(data=sim_results %>%
         mutate(rel_diff = ifelse(parameter=="Difference in prevalence",
                                  difference,
                                  difference),
                parameter = ifelse(parameter == "Difference in prevalence",
                                   "mmm",
                                   "ccc"),
                parameter = factor(parameter,
                                   levels=c("ccc","mmm"),
                                   labels=c("ccc","mmm"))),
       aes(x=prev,y=c_stat,fill=rel_diff))+
  geom_tile() +
  facet_grid(parameter~type,labeller = labeller(parameter = to_math)) +
  theme_classic() +
  xlab("Expected m") +
  ylab("Expected c") +
  scale_fill_gradient2(name = "") +
  theme_significance(base_size = 25) +
  theme(legend.key.height = unit(0.5, "cm"),
        legend.key.width =  unit(2, "cm")) -> fig_sim

# Save the figure
ggsave("./figures/fig_sim.jpeg",plot=fig_sim,device = "jpg")
