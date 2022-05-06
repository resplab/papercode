
########
############ Author: Abdollah Safari <a.saffari72@gmail.com>
########


## Load AllTrials_Data dataset with ACCEPT 1 predictions by using accept package

### required functions
## Adjusting on exacerbation history (bivariate)
ExacHistAdj <- function(n_sample = 100,
                        RE_var_mat = matrix(c(0.6854, 0.08782, 0.08782, 2.2491), ncol = 2),
                        ParamEst,
                        FileName = "Estimates",
                        SheetName = "All predictors",
                        PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE",
                        PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE",
                        Hist_obsExac = "Hist_obsExac",
                        Hist_obsExac_Sev = "Hist_obsExac_Sev",
                        Data,
                        ParamEst) {     ## must include c("PRED_RATE_PLAC_RE", "Hist_obsExac")
  
  RE_seq_1 <- seq(from = -2 * RE_var_mat[1, 1], to = 2 * RE_var_mat[1, 1], length.out = n_sample)
  RE_seq_2 <- seq(from = -2 * RE_var_mat[2, 2], to = 2 * RE_var_mat[2, 2], length.out = n_sample)
  RE_W_mat <- outer(X = RE_seq_1, Y = RE_seq_2, FUN = Vectorize(function(x, y) BiNorm_pdf(c(x, y), sigma = RE_var_mat)))
  Lambda <- (matrix(Data[ , PRED_RATE_PLAC_RE] ^ (1 / 0.9706), ncol = 1) %*% matrix(exp(RE_seq_1), nrow = 1)) ^ 0.9706
  ProbSev <- matrix(Data[ , PRED_SPROB_ALL_RE] / (1 - Data[ , PRED_SPROB_ALL_RE]), ncol = 1) %*% matrix(exp(RE_seq_2), nrow = 1)
  ProbSev <- ProbSev / (1 + ProbSev)
  
  Lambda_Sev <- lapply(c(1 : nrow(Data)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(ProbSev[x, ], nrow = 1))
  Lambda_non_Sev <- lapply(c(1 : nrow(Data)), function(x) matrix(Lambda[x, ], ncol = 1) %*% matrix(1 - ProbSev[x, ], nrow = 1))
  
  Posterior_Sev_W <-
    lapply(c(1 : nrow(Data)), function(x) {
      t(apply(Lambda_Sev[[x]], 1, function(y) dpois(x = Data[x , Hist_obsExac_Sev], lambda = y))) * RE_W_mat
    })
  Posterior_non_Sev_W <-
    lapply(c(1 : nrow(Data)), function(x) {
      t(apply(Lambda_non_Sev[[x]], 1, function(y) dpois(x = Data[x , Hist_obsExac] - Data[x , Hist_obsExac_Sev], lambda = y))) * RE_W_mat
    })
  
  
  Rate_Sev_Adj <- sapply(c(1 : nrow(Data)), function(x) weighted.mean(x = Lambda_Sev[[x]], w = Posterior_Sev_W[[x]]))
  Rate_non_Sev_Adj <- sapply(c(1 : nrow(Data)), function(x) weighted.mean(x = Lambda_non_Sev[[x]], w = Posterior_non_Sev_W[[x]]))
  
  Rate_Sev_SD_Adj <- sqrt(Rate_Sev_Adj + Rate_Sev_Adj ^ 2 * (exp(0.9706 * RE_var_mat[1, 1]) - 1))
  Rate_non_Sev_SD_Adj <- sqrt(Rate_non_Sev_Adj + Rate_non_Sev_Adj ^ 2 * (exp(0.9706 * RE_var_mat[1, 1]) - 1))
  
  Rate_Adj <- Rate_Sev_Adj + Rate_non_Sev_Adj
  Rate_SD_Adj <- sqrt(Rate_Adj + Rate_Adj ^ 2 * (exp(0.9706 * RE_var_mat[1, 1]) - 1))
  
  return(list(ID = Data$ID,
              Rate_Adj = Rate_Adj, Rate_SD_Adj = Rate_SD_Adj,
              Rate_Sev_Adj = Rate_Sev_Adj, Rate_Sev_SD_Adj = Rate_Sev_SD_Adj,
              Rate_non_Sev_Adj = Rate_non_Sev_Adj, Rate_non_Sev_SD_Adj = Rate_non_Sev_SD_Adj))
}


## Spline fit for rates and their SD
splineFit <- function(data = eclipse2yr_Preds,
                      Preds = "PRED_RATE_Fnl",
                      Preds_SE = "PRED_SE_RATE_Fnl",
                      nTile = 100,
                      rate_DF = 4,
                      rateSD_DF = 4,
                      Obs = "obsExac",
                      YIS = "YIS") {
  data %>%
    mutate(decile = ntile(!!sym(Preds), nTile)) -> data_tiled
  
  data_tiled %>%
    group_by(decile) %>%
    summarise(Obs_Rate = mean(!!sym(Obs) / !!sym(YIS), na.rm = T),
              Obs_Rate_SD = sd(!!sym(Obs) / !!sym(YIS), na.rm = T),
              Obs_No = n(),
              PredRate_Mean = mean(!!sym(Preds), na.rm = T),
              PredRateSE_Mean = mean(!!sym(Preds_SE), na.rm = T)) -> data_tiled_summarized
  
  data_tiled_summarized$Obs_Rate_SE <-
    data_tiled_summarized$Obs_Rate_SD / sqrt(data_tiled_summarized$Obs_No)
  
  # Spline: Rate
  SplineFit_Rate_Sp <- lm(Obs_Rate ~ ns(PredRate_Mean, df = rate_DF, intercept = T),
                          weights = 1 / data_tiled_summarized$Obs_Rate_SE[data_tiled_summarized$Obs_Rate_SE>0],
                          data = data_tiled_summarized[data_tiled_summarized$Obs_Rate_SE>0 , ])
  Rate_nsObj <- ns(data_tiled_summarized[data_tiled_summarized$Obs_Rate_SE>0 , ]$PredRate_Mean, df = rate_DF, intercept = T)
  
  # Spline: Rate SD
  SplineFit_RateSE_Sp <- lm(Obs_Rate_SD ~ ns(PredRateSE_Mean, df = rateSD_DF, intercept = T),
                            data = data_tiled_summarized)
  Rate_SD_nsObj <- ns(data_tiled_summarized$PredRateSE_Mean, df = rate_DF, intercept = T)
  
  return(list(data = data_tiled_summarized,
              RateSpline = SplineFit_Rate_Sp,
              Rate_nsObj = Rate_nsObj,
              SDRateSpline = SplineFit_RateSE_Sp,
              Rate_SD_nsObj = Rate_SD_nsObj))
}

## Impute LAMA for TORCH
# All data
AllData_LAMA <- AllTrials_Data[AllTrials_Data$trial %in% c("OPTIMAL", "MACRO", "STATCOPE", "ECLIPSE") , ]
LAMA_Model <- glm(LAMA ~ indicated_statin+ LABA + ICS +
                    sgrq10 + gender + age10 + nowsmk + oxygen + fev1pp100 + bmi10,
                  data = AllData_LAMA, family = binomial(link = "logit"))
TORCH_temp <- AllTrials_Data[AllTrials_Data$trial == "TORCH", ]

library(Amelia)
set.seed(1)
SGRQ_Imputs <- amelia(TORCH_temp[ , c("ID", "indicated_statin", "LABA", "ICS", "sgrq10", "gender", "age10", "nowsmk",
                                      "oxygen", "fev1pp100", "bmi10", "MRC_SCORE")],
                      m = 10, idvars = "ID", noms = c("indicated_statin", "LABA", "ICS", "gender", "nowsmk", "oxygen"))


## TORCH predictions for reduced model with no SGRQ & Meds
PredObj_temp <- accept::accept(Data = TORCH_temp, KeepSGRQ = FALSE, KeepMeds = FALSE)
TORCH_temp$PRED_RATE_ALL_RE_NOSGRQ_MEDS <- PredObj_temp$PRED_RATE_ALL_F
TORCH_temp$PRED_SPROB_ALL_RE_NOSGRQ_MEDS <- PredObj_temp$PRED_SPROB_ALL_F

set.seed(1)
NRepl <- 10
TORCH_temp$LAMA_sim <- 0
TORCH_temp$sgrq10_sim <- 0
TORCH_temp$PRED_RATE_ALL_RE <- 0
TORCH_temp$PRED_SPROB_ALL_RE <- 0
TORCH_temp$PRED_RATE_ALL_RE_NOMEDS <- 0
TORCH_temp$PRED_SPROB_ALL_RE_NOMEDS <- 0
TORCH_temp$PRED_RATE_ALL_RE_NOSGRQ <- 0
TORCH_temp$PRED_SPROB_ALL_RE_NOSGRQ <- 0
for (i in 1 : NRepl) {
  TORCH_temp$sgrq10 <- SGRQ_Imputs$imputations[[i]]$sgrq10
  TORCH_temp$LAMA_Pred <- predict(LAMA_Model, type = "response", newdata = TORCH_temp)
  TORCH_temp$LAMA <- rbinom(n = nrow(TORCH_temp), size = 1, prob = TORCH_temp$LAMA_Pred)
  
  TORCH_temp$LAMA_sim <- TORCH_temp$LAMA_sim + TORCH_temp$LAMA_Pred
  TORCH_temp$sgrq10_sim <- TORCH_temp$sgrq10_sim + TORCH_temp$sgrq10

  ## All predictors
  PredObj_temp <- accept::accept(Data = TORCH_temp, KeepSGRQ = TRUE, KeepMeds =TRUE)
  TORCH_temp$PRED_RATE_ALL_RE <- TORCH_temp$PRED_RATE_ALL_RE + PredObj_temp$PRED_RATE_ALL_F
  TORCH_temp$PRED_SPROB_ALL_RE <- TORCH_temp$PRED_SPROB_ALL_RE + PredObj_temp$PRED_SPROB_ALL_F
  rm(PredObj_temp)
  
  ## No Meds
  PredObj_temp <- accept::accept(Data = TORCH_temp, KeepSGRQ = FALSE, KeepMeds = TRUE)
  TORCH_temp$PRED_RATE_ALL_RE_NOMEDS <- TORCH_temp$PRED_RATE_ALL_RE_NOMEDS + PredObj_temp$PRED_RATE_ALL_F
  TORCH_temp$PRED_SPROB_ALL_RE_NOMEDS <- TORCH_temp$PRED_SPROB_ALL_RE_NOMEDS + PredObj_temp$PRED_SPROB_ALL_F
  rm(PredObj_temp)
  
  ## No SGRQ
  PredObj_temp <- accept::accept(Data = TORCH_temp, KeepSGRQ = TRUE, KeepMeds = FALSE)
  TORCH_temp$PRED_RATE_ALL_RE_NOSGRQ <- TORCH_temp$PRED_RATE_ALL_RE_NOSGRQ + PredObj_temp$PRED_RATE_ALL_F
  TORCH_temp$PRED_SPROB_ALL_RE_NOSGRQ <- TORCH_temp$PRED_SPROB_ALL_RE_NOSGRQ + PredObj_temp$PRED_SPROB_ALL_F
  rm(PredObj_temp)
}
TORCH_temp$LAMA <- TORCH_temp$LAMA_sim / NRepl
TORCH_temp$sgrq10 <- TORCH_temp$sgrq10_sim / NRepl
TORCH_temp$PRED_RATE_ALL_RE <- TORCH_temp$PRED_RATE_ALL_RE / NRepl
TORCH_temp$PRED_SPROB_ALL_RE <- TORCH_temp$PRED_SPROB_ALL_RE / NRepl
TORCH_temp$PRED_RATE_ALL_RE_NOMEDS <- TORCH_temp$PRED_RATE_ALL_RE_NOMEDS / NRepl
TORCH_temp$PRED_SPROB_ALL_RE_NOMEDS <- TORCH_temp$PRED_SPROB_ALL_RE_NOMEDS / NRepl
TORCH_temp$PRED_RATE_ALL_RE_NOSGRQ <- TORCH_temp$PRED_RATE_ALL_RE_NOSGRQ / NRepl
TORCH_temp$PRED_SPROB_ALL_RE_NOSGRQ <- TORCH_temp$PRED_SPROB_ALL_RE_NOSGRQ / NRepl


AllTrials_Data[AllTrials_Data$trial == "TORCH" , c("LAMA", "sgrq10", "PRED_RATE_ALL_RE", "PRED_SPROB_ALL_RE",
                                                   "PRED_RATE_ALL_RE_NOMEDS", "PRED_SPROB_ALL_RE_NOMEDS",
                                                   "PRED_RATE_ALL_RE_NOSGRQ", "PRED_SPROB_ALL_RE_NOSGRQ",
                                                   "PRED_RATE_ALL_RE_NOSGRQ_MEDS", "PRED_SPROB_ALL_RE_NOSGRQ_MEDS")] <-
  TORCH_temp[order(match(TORCH_temp$ID, AllTrials_Data$ID[AllTrials_Data$trial == "TORCH"])) ,
             c("LAMA", "sgrq10", "PRED_RATE_ALL_RE", "PRED_SPROB_ALL_RE",
               "PRED_RATE_ALL_RE_NOMEDS", "PRED_SPROB_ALL_RE_NOMEDS",
               "PRED_RATE_ALL_RE_NOSGRQ", "PRED_SPROB_ALL_RE_NOSGRQ",
               "PRED_RATE_ALL_RE_NOSGRQ_MEDS", "PRED_SPROB_ALL_RE_NOSGRQ_MEDS")]


# ## Adjust for history of exacerbations
ECLIPSE_Preds_HistAdj <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "ECLIPSE" , ], n_sample = 100)
ECLIPSE_Preds_HistAdj_NOMEDS <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "ECLIPSE" , ], n_sample = 100,
                                                 SheetName = "No Meds",
                                                 PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE_NOMEDS",
                                                 PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE_NOMEDS")
ECLIPSE_Preds_HistAdj_NOSGRQ <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "ECLIPSE" , ], n_sample = 100,
                                                 SheetName = "No SGRQ",
                                                 PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE_NOSGRQ",
                                                 PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE_NOSGRQ")
ECLIPSE_Preds_HistAdj_NOSGRQ_MEDS <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "ECLIPSE" , ], n_sample = 100,
                                                 SheetName = "No Meds & SGRQ",
                                                 PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE_NOSGRQ_MEDS",
                                                 PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE_NOSGRQ_MEDS")


TORCH_Preds_HistAdj <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "TORCH" , ], n_sample = 100)
TORCH_Preds_HistAdj_NOMEDS <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "TORCH" , ], n_sample = 100,
                                                 SheetName = "No Meds",
                                                 PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE_NOMEDS",
                                                 PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE_NOMEDS")
TORCH_Preds_HistAdj_NOSGRQ <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "TORCH" , ], n_sample = 100,
                                                 SheetName = "No SGRQ",
                                                 PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE_NOSGRQ",
                                                 PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE_NOSGRQ")
TORCH_Preds_HistAdj_NOSGRQ_MEDS <- ExacHistAdj(Data = AllTrials_Data[AllTrials_Data$trial == "TORCH" , ], n_sample = 100,
                                                      SheetName = "No Meds & SGRQ",
                                                      PRED_RATE_PLAC_RE = "PRED_RATE_ALL_RE_NOSGRQ_MEDS",
                                                      PRED_SPROB_ALL_RE = "PRED_SPROB_ALL_RE_NOSGRQ_MEDS")

AllTrials_Data$PRED_RATE_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Adj
AllTrials_Data$PRED_SE_RATE_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_SD_Adj
AllTrials_Data$PRED_SRATE_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_Adj
AllTrials_Data$PRED_SE_SRATE_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_SD_Adj
AllTrials_Data$PRED_NSRATE_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_Adj
AllTrials_Data$PRED_SE_NSRATE_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_SD_Adj
AllTrials_Data$PRED_RATE_NOMEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Adj_NOMEDS
AllTrials_Data$PRED_SE_RATE_NOMEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_SD_Adj_NOMEDS
AllTrials_Data$PRED_SRATE_NOMEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_Adj_NOMEDS
AllTrials_Data$PRED_SE_SRATE_NOMEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_SD_Adj_NOMEDS
AllTrials_Data$PRED_NSRATE_NOMEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_Adj_NOMEDS
AllTrials_Data$PRED_SE_NSRATE_NOMEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_SD_Adj_NOMEDS
AllTrials_Data$PRED_RATE_NOSGRQ_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Adj_NOSGRQ
AllTrials_Data$PRED_SE_RATE_NOSGRQ_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_SD_Adj_NOSGRQ
AllTrials_Data$PRED_SRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_Adj_NOSGRQ
AllTrials_Data$PRED_SE_SRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_SD_Adj_NOSGRQ
AllTrials_Data$PRED_NSRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_Adj_NOSGRQ
AllTrials_Data$PRED_SE_NSRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_SD_Adj_NOSGRQ
AllTrials_Data$PRED_RATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SE_RATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_SD_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SE_SRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_Sev_SD_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_NSRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SE_NSRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "ECLIPSE"] <- ECLIPSE_Preds_HistAdj$Rate_non_Sev_SD_Adj_NOSGRQ_MEDS

AllTrials_Data$PRED_RATE_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Adj
AllTrials_Data$PRED_SE_RATE_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_SD_Adj
AllTrials_Data$PRED_SRATE_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_Adj
AllTrials_Data$PRED_SE_SRATE_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_SD_Adj
AllTrials_Data$PRED_NSRATE_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_Adj
AllTrials_Data$PRED_SE_NSRATE_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_SD_Adj
AllTrials_Data$PRED_RATE_NOMEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Adj_NOMEDS
AllTrials_Data$PRED_SE_RATE_NOMEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_SD_Adj_NOMEDS
AllTrials_Data$PRED_SRATE_NOMEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_Adj_NOMEDS
AllTrials_Data$PRED_SE_SRATE_NOMEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_SD_Adj_NOMEDS
AllTrials_Data$PRED_NSRATE_NOMEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_Adj_NOMEDS
AllTrials_Data$PRED_SE_NSRATE_NOMEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_SD_Adj_NOMEDS
AllTrials_Data$PRED_RATE_NOSGRQ_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Adj_NOSGRQ
AllTrials_Data$PRED_SE_RATE_NOSGRQ_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_SD_Adj_NOSGRQ
AllTrials_Data$PRED_SRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_Adj_NOSGRQ
AllTrials_Data$PRED_SE_SRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_SD_Adj_NOSGRQ
AllTrials_Data$PRED_NSRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_Adj_NOSGRQ
AllTrials_Data$PRED_SE_NSRATE_NOSGRQ_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_SD_Adj_NOSGRQ
AllTrials_Data$PRED_RATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SE_RATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_SD_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SE_SRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_Sev_SD_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_NSRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_Adj_NOSGRQ_MEDS
AllTrials_Data$PRED_SE_NSRATE_NOSGRQ_MEDS_Fnl[AllTrials_Data$trial == "TORCH"] <- TORCH_Preds_HistAdj$Rate_non_Sev_SD_Adj_NOSGRQ_MEDS


###############  Spline fits: all exacerbations
eclipse2yr_Rate_Sp <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_RATE_Fnl",
                                      Preds_SE = "PRED_SE_RATE_Fnl", nTile = 100,
                                      Obs = "obsExac", YIS = "YIS")
eclipse2yr_Rate_Sp_NOMEDS <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_RATE_NOMEDS_Fnl",
                                             Preds_SE = "PRED_SE_RATE_NOMEDS_Fnl", nTile = 100,
                                             Obs = "obsExac", YIS = "YIS")
eclipse2yr_Rate_Sp_NOSGRQ <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_RATE_NOSGRQ_Fnl",
                                             Preds_SE = "PRED_SE_RATE_NOSGRQ_Fnl", nTile = 100,
                                             Obs = "obsExac", YIS = "YIS")
eclipse2yr_Rate_Sp_NOSGRQ_MEDS <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_RATE_NOSGRQ_MEDS_Fnl",
                                                  Preds_SE = "PRED_SE_RATE_NOSGRQ_MEDS_Fnl", nTile = 100,
                                                  Obs = "obsExac", YIS = "YIS")

# Calibration function - Spline df = 4
ACCEPT_Adj_Rate_Sp <- function(x) predict(eclipse2yr_Rate_Sp$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_RateSE_Sp <- function(x) predict(eclipse2yr_Rate_Sp$SDRateSpline, newdata = list(PredRateSE_Mean = x))
ACCEPT_Adj_Rate_Sp_NOMEDS <- function(x) predict(eclipse2yr_Rate_Sp_NOMEDS$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_RateSE_Sp_NOMEDS <- function(x) predict(eclipse2yr_Rate_Sp_NOMEDS$SDRateSpline, newdata = list(PredRateSE_Mean = x))
ACCEPT_Adj_Rate_Sp_NOSGRQ <- function(x) predict(eclipse2yr_Rate_Sp_NOSGRQ$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_RateSE_Sp_NOSGRQ <- function(x) predict(eclipse2yr_Rate_Sp_NOSGRQ$SDRateSpline, newdata = list(PredRateSE_Mean = x))
ACCEPT_Adj_Rate_Sp_NOSGRQ_MEDS <- function(x) predict(eclipse2yr_Rate_Sp_NOSGRQ_MEDS$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_RateSE_Sp_NOSGRQ_MEDS <- function(x) predict(eclipse2yr_Rate_Sp_NOSGRQ_MEDS$SDRateSpline, newdata = list(PredRateSE_Mean = x))

# Adjusted Estimated rates of 2nd year based on the 1st year
FourTrials_Preds$PredRate_Sp <- ACCEPT_Adj_Rate_Sp(FourTrials_Preds$PRED_RATE_Fnl)
FourTrials_Preds$PredRate_Sp <- ifelse(FourTrials_Preds$PredRate_Sp > 0, FourTrials_Preds$PredRate_Sp, 0)
FourTrials_Preds$PredRateSE_Sp <- ACCEPT_Adj_RateSE_Sp(FourTrials_Preds$PRED_SE_RATE_Fnl)
FourTrials_Preds$PredRateSE_Sp <- ifelse(FourTrials_Preds$PredRateSE_Sp > 0, FourTrials_Preds$PredRateSE_Sp, 0)
FourTrials_Preds$PredRate_Sp_NOMEDS <- ifelse(ACCEPT_Adj_Rate_Sp_NOMEDS(FourTrials_Preds$PRED_RATE_NOMEDS_Fnl) > 0,
                                              ACCEPT_Adj_Rate_Sp_NOMEDS(FourTrials_Preds$PRED_RATE_NOMEDS_Fnl), 0)
FourTrials_Preds$PredRateSE_Sp_NOMEDS <- ifelse(ACCEPT_Adj_RateSE_Sp_NOMEDS(FourTrials_Preds$PRED_SE_RATE_NOMEDS_Fnl) > 0,
                                                ACCEPT_Adj_RateSE_Sp_NOMEDS(FourTrials_Preds$PRED_SE_RATE_NOMEDS_Fnl), 0)
FourTrials_Preds$PredRate_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_Rate_Sp_NOSGRQ(FourTrials_Preds$PRED_RATE_NOSGRQ_Fnl) > 0,
                                              ACCEPT_Adj_Rate_Sp_NOSGRQ(FourTrials_Preds$PRED_RATE_NOSGRQ_Fnl), 0)
FourTrials_Preds$PredRateSE_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_RateSE_Sp_NOSGRQ(FourTrials_Preds$PRED_SE_RATE_NOSGRQ_Fnl) > 0,
                                                ACCEPT_Adj_RateSE_Sp_NOSGRQ(FourTrials_Preds$PRED_SE_RATE_NOSGRQ_Fnl), 0)
FourTrials_Preds$PredRate_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_Rate_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_RATE_NOSGRQ_MEDS_Fnl) > 0,
                                                   ACCEPT_Adj_Rate_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_RATE_NOSGRQ_MEDS_Fnl), 0)
FourTrials_Preds$PredRateSE_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_RateSE_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_SE_RATE_NOSGRQ_MEDS_Fnl) > 0,
                                                     ACCEPT_Adj_RateSE_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_SE_RATE_NOSGRQ_MEDS_Fnl), 0)




###############  Spline fits: severe exacerbations
eclipse2yr_SRate_Sp <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_SRATE_Fnl",
                                       Preds_SE = "PRED_SE_SRATE_Fnl", nTile = 100,
                                       Obs = "obsExac_Sev", YIS = "YIS")
eclipse2yr_SRate_Sp_NOMEDS <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_SRATE_NOMEDS_Fnl",
                                              Preds_SE = "PRED_SE_SRATE_NOMEDS_Fnl", nTile = 100,
                                              Obs = "obsExac_Sev", YIS = "YIS")
eclipse2yr_SRate_Sp_NOSGRQ <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_SRATE_NOSGRQ_Fnl",
                                              Preds_SE = "PRED_SE_SRATE_NOSGRQ_Fnl", nTile = 100,
                                              Obs = "obsExac_Sev", YIS = "YIS")
eclipse2yr_SRate_Sp_NOSGRQ_MEDS <- splineFit(data = eclipse2yr_Preds, Preds = "PRED_SRATE_NOSGRQ_MEDS_Fnl",
                                                   Preds_SE = "PRED_SE_SRATE_NOSGRQ_MEDS_Fnl", nTile = 100,
                                                   Obs = "obsExac_Sev", YIS = "YIS")

# Calibration function - Spline df = 3
ACCEPT_Adj_SRate_Sp <- function(x) predict(eclipse2yr_SRate_Sp$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_SRateSE_Sp <- function(x) predict(eclipse2yr_SRate_Sp$SDRateSpline, newdata = list(PredRateSE_Mean = x))
ACCEPT_Adj_SRate_Sp_NOMEDS <- function(x) predict(eclipse2yr_SRate_Sp_NOMEDS$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_SRateSE_Sp_NOMEDS <- function(x) predict(eclipse2yr_SRate_Sp_NOMEDS$SDRateSpline, newdata = list(PredRateSE_Mean = x))
ACCEPT_Adj_SRate_Sp_NOSGRQ <- function(x) predict(eclipse2yr_SRate_Sp_NOSGRQ$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_SRateSE_Sp_NOSGRQ <- function(x) predict(eclipse2yr_SRate_Sp_NOSGRQ$SDRateSpline, newdata = list(PredRateSE_Mean = x))
ACCEPT_Adj_SRate_Sp_NOSGRQ_MEDS <- function(x) predict(eclipse2yr_SRate_Sp_NOSGRQ_MEDS$RateSpline, newdata = list(PredRate_Mean = x))
ACCEPT_Adj_SRateSE_Sp_NOSGRQ_MEDS <- function(x) predict(eclipse2yr_SRate_Sp_NOSGRQ_MEDS$SDRateSpline, newdata = list(PredRateSE_Mean = x))

# Adjusted Estimated rates of 2nd year based on the 1st year
FourTrials_Preds$PredSRate_Sp <- ACCEPT_Adj_SRate_Sp(FourTrials_Preds$PRED_SRATE_Fnl)
FourTrials_Preds$PredSRate_Sp <- ifelse(FourTrials_Preds$PredSRate_Sp > 0, FourTrials_Preds$PredSRate_Sp, 0)
FourTrials_Preds$PredSRateSE_Sp <- ACCEPT_Adj_SRateSE_Sp(FourTrials_Preds$PRED_SE_SRATE_Fnl)
FourTrials_Preds$PredSRateSE_Sp <- ifelse(FourTrials_Preds$PredSRateSE_Sp > 0, FourTrials_Preds$PredSRateSE_Sp, 0)
FourTrials_Preds$PredSRate_Sp_NOMEDS <- ifelse(ACCEPT_Adj_SRate_Sp_NOMEDS(FourTrials_Preds$PRED_SRATE_NOMEDS_Fnl) > 0,
                                               ACCEPT_Adj_SRate_Sp_NOMEDS(FourTrials_Preds$PRED_SRATE_NOMEDS_Fnl), 0)
FourTrials_Preds$PredSRateSE_Sp_NOMEDS <- ifelse(ACCEPT_Adj_SRateSE_Sp_NOMEDS(FourTrials_Preds$PRED_SE_SRATE_NOMEDS_Fnl) > 0,
                                                 ACCEPT_Adj_SRateSE_Sp_NOMEDS(FourTrials_Preds$PRED_SE_SRATE_NOMEDS_Fnl), 0)
FourTrials_Preds$PredSRate_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_SRate_Sp_NOSGRQ(FourTrials_Preds$PRED_SRATE_NOSGRQ_Fnl) > 0,
                                               ACCEPT_Adj_SRate_Sp_NOSGRQ(FourTrials_Preds$PRED_SRATE_NOSGRQ_Fnl), 0)
FourTrials_Preds$PredSRateSE_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_SRateSE_Sp_NOSGRQ(FourTrials_Preds$PRED_SE_SRATE_NOSGRQ_Fnl) > 0,
                                                 ACCEPT_Adj_SRateSE_Sp_NOSGRQ(FourTrials_Preds$PRED_SE_SRATE_NOSGRQ_Fnl), 0)
FourTrials_Preds$PredSRate_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_SRate_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_SRATE_NOSGRQ_MEDS_Fnl) > 0,
                                                    ACCEPT_Adj_SRate_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_SRATE_NOSGRQ_MEDS_Fnl), 0)
FourTrials_Preds$PredSRateSE_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_SRateSE_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_SE_SRATE_NOSGRQ_MEDS_Fnl) > 0,
                                                      ACCEPT_Adj_SRateSE_Sp_NOSGRQ_MEDS(FourTrials_Preds$PRED_SE_SRATE_NOSGRQ_MEDS_Fnl), 0)


###############  External: TORCH
## all exaceerbations
torch2yr_Preds$PredRate_Sp <- ACCEPT_Adj_Rate_Sp(torch2yr_Preds$PRED_RATE_Fnl)
torch2yr_Preds$PredRate_Sp <- ifelse(torch2yr_Preds$PredRate_Sp > 0, torch2yr_Preds$PredRate_Sp, 0)
torch2yr_Preds$PredRateSE_Sp <- ACCEPT_Adj_RateSE_Sp(torch2yr_Preds$PRED_SE_RATE_Fnl)
torch2yr_Preds$PredRateSE_Sp <- ifelse(torch2yr_Preds$PredRateSE_Sp > 0, torch2yr_Preds$PredRateSE_Sp, 0)
torch2yr_Preds$PredRate_Sp_NOMEDS <- ifelse(ACCEPT_Adj_Rate_Sp_NOMEDS(torch2yr_Preds$PRED_RATE_NOMEDS_Fnl) > 0,
                                            ACCEPT_Adj_Rate_Sp_NOMEDS(torch2yr_Preds$PRED_RATE_NOMEDS_Fnl), 0)
torch2yr_Preds$PredRateSE_Sp_NOMEDS <- ifelse(ACCEPT_Adj_RateSE_Sp_NOMEDS(torch2yr_Preds$PRED_SE_RATE_NOMEDS_Fnl) > 0,
                                              ACCEPT_Adj_RateSE_Sp_NOMEDS(torch2yr_Preds$PRED_SE_RATE_NOMEDS_Fnl), 0)
torch2yr_Preds$PredRate_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_Rate_Sp_NOSGRQ(torch2yr_Preds$PRED_RATE_NOSGRQ_Fnl) > 0,
                                            ACCEPT_Adj_Rate_Sp_NOSGRQ(torch2yr_Preds$PRED_RATE_NOSGRQ_Fnl), 0)
torch2yr_Preds$PredRateSE_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_RateSE_Sp_NOSGRQ(torch2yr_Preds$PRED_SE_RATE_NOSGRQ_Fnl) > 0,
                                              ACCEPT_Adj_RateSE_Sp_NOSGRQ(torch2yr_Preds$PRED_SE_RATE_NOSGRQ_Fnl), 0)
torch2yr_Preds$PredRate_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_Rate_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_RATE_NOSGRQ_MEDS_Fnl) > 0,
                                                 ACCEPT_Adj_Rate_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_RATE_NOSGRQ_MEDS_Fnl), 0)
torch2yr_Preds$PredRateSE_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_RateSE_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_SE_RATE_NOSGRQ_MEDS_Fnl) > 0,
                                                   ACCEPT_Adj_RateSE_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_SE_RATE_NOSGRQ_MEDS_Fnl), 0)

## severe exacerbations
torch2yr_Preds$PredSRate_Sp <- ACCEPT_Adj_SRate_Sp(torch2yr_Preds$PRED_SRATE_Fnl)
torch2yr_Preds$PredSRate_Sp <- ifelse(torch2yr_Preds$PredSRate_Sp > 0 , torch2yr_Preds$PredSRate_Sp, 0)
torch2yr_Preds$PredSRateSE_Sp <- ACCEPT_Adj_SRateSE_Sp(torch2yr_Preds$PRED_SE_SRATE_Fnl)
torch2yr_Preds$PredSRateSE_Sp <- ifelse(torch2yr_Preds$PredSRateSE_Sp > 0 , torch2yr_Preds$PredSRateSE_Sp, 0)
torch2yr_Preds$PredSRate_Sp_NOMEDS <- ifelse(ACCEPT_Adj_SRate_Sp_NOMEDS(torch2yr_Preds$PRED_SRATE_NOMEDS_Fnl) > 0,
                                             ACCEPT_Adj_SRate_Sp_NOMEDS(torch2yr_Preds$PRED_SRATE_NOMEDS_Fnl), 0)
torch2yr_Preds$PredSRateSE_Sp_NOMEDS <- ifelse(ACCEPT_Adj_SRateSE_Sp_NOMEDS(torch2yr_Preds$PRED_SE_SRATE_NOMEDS_Fnl) > 0,
                                               ACCEPT_Adj_SRateSE_Sp_NOMEDS(torch2yr_Preds$PRED_SE_SRATE_NOMEDS_Fnl), 0)
torch2yr_Preds$PredSRate_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_SRate_Sp_NOSGRQ(torch2yr_Preds$PRED_SRATE_NOSGRQ_Fnl) > 0 ,
                                             ACCEPT_Adj_SRate_Sp_NOSGRQ(torch2yr_Preds$PRED_SRATE_NOSGRQ_Fnl), 0)
torch2yr_Preds$PredSRateSE_Sp_NOSGRQ <- ifelse(ACCEPT_Adj_SRateSE_Sp_NOSGRQ(torch2yr_Preds$PRED_SE_SRATE_NOSGRQ_Fnl) > 0 ,
                                               ACCEPT_Adj_SRateSE_Sp_NOSGRQ(torch2yr_Preds$PRED_SE_SRATE_NOSGRQ_Fnl), 0)
torch2yr_Preds$PredSRate_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_SRate_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_SRATE_NOSGRQ_MEDS_Fnl) > 0 ,
                                                  ACCEPT_Adj_SRate_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_SRATE_NOSGRQ_MEDS_Fnl), 0)
torch2yr_Preds$PredSRateSE_Sp_NOSGRQ_MEDS <- ifelse(ACCEPT_Adj_SRateSE_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_SE_SRATE_NOSGRQ_MEDS_Fnl) > 0,
                                                    ACCEPT_Adj_SRateSE_Sp_NOSGRQ_MEDS(torch2yr_Preds$PRED_SE_SRATE_NOSGRQ_MEDS_Fnl), 0)

