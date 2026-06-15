library(mvtnorm)
library(qrng)
library(randtoolbox)
library(stringr)


# Append a timestamped progress line to a shared log file. Safe to call from
# parallel (PSOCK) workers — each line carries the worker PID so interleaved
# output stays attributable. Watch live with, e.g.:
#   PowerShell:  Get-Content <file> -Wait -Tail 30
# A NULL file is a no-op (logging disabled).
log_progress <- function(msg, file = NULL) {
  if (is.null(file) || !nzchar(file)) return(invisible(NULL))
  line <- sprintf("[%s pid=%d] %s\n",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  Sys.getpid(), msg)
  # Best-effort: if the file is momentarily locked (e.g. opened in a viewer
  # that takes an exclusive lock), silently skip this line rather than letting
  # the write error propagate into — and abort — the worker.
  try(cat(line, file = file, append = TRUE), silent = TRUE)
  invisible(NULL)
}

# Atomically write an R object to `path`: write to a PID-tagged temp file, then
# rename. A crash mid-write therefore never leaves a truncated file that a
# resume would mistake for a completed row.
save_row_atomic <- function(obj, path) {
  tmp <- paste0(path, ".tmp_", Sys.getpid())
  saveRDS(obj, tmp)
  if (!file.rename(tmp, path)) {       # rename can fail across volumes
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

# Master per-row seed base. settings$design_seed is the single master seed for
# the whole study: changing it regenerates the design AND every row's data,
# glmnet folds, and MCMC draws. The trailing *10 reserves sub-seed slots so the
# per-row uses don't collide across adjacent rows:
#   data generation : base + 0   (set inside generate_from_design)
#   glmnet CV folds  : base + 1
#   ridge MCMC       : base + 2
#   lasso MCMC       : base + 3
# scenario < 100 and iteration < 10000 keep the components non-overlapping;
# keep design_seed a modest integer (< ~200) so base stays within R's 32-bit
# integer seed range.
row_seed_base <- function(settings, scenario, iteration) {
  (settings$design_seed * 1000000 + scenario * 10000 + iteration) * 10
}

logit <- function(x) {
  log(x / (1 - x))
}
expit <- function(x) {
  1 / (1 + exp(-x))
}
#
# gen_population <- function(size, grid = FALSE) {
#   pop <- list(
#     size = size,
#     parms = rmvnorm(1, settings$parms$mu, settings$parms$cov)[1, ],
#     casemix = list(
#       mu = rmvnorm(1, settings$casemix$mu, settings$casemix$cov)[1, ],
#       v = diag(settings$casemix$v)
#     )
#   )
#
#   pop$pop <- gen_sample(pop$size, pop, grid = grid)
#   pop$pop <- cbind(pop$pop)
#
#   pop
# }
#
#
# gen_sample <- function(n, population, grid = FALSE) {
#   if (grid) {
#     d <- length(settings$casemix$mu)
#     L <- chol(settings$casemix$cov)
#     U <- sobol(n = n + 1, d = d, randomize = "none")[-1, ]
#     Z <- qnorm(U)
#     xs <- t(L %*% t(Z)) +
#       matrix(settings$casemix$mu, nrow = n, ncol = d, byrow = TRUE)
#   } else {
#     xs <- rmvnorm(n, population$casemix$mu, population$casemix$v)
#   }
#   lin <- population$parms[1] + xs %*% population$parms[-1]
#   p <- 1 / (1 + exp(-lin))
#   Y <- rbinom(n, 1, p)
#   dt <- cbind(xs, Y, p)
#   colnames(dt) <- c(paste0("X", 1:ncol(xs)), "Y", "p")
#   dt
# }

vcov_glm <- function(fit, x_matrix) {
  # 1. Validation: Ensure we are dealing with a binomial fit
  if (fit$family$family != "binomial") {
    stop("This function is specifically for binomial family models.")
  }

  # 2. Extract the number of parameters (p)
  p <- fit$rank

  # 3. Use chol2inv on the R part of the QR decomposition.
  # fit$qr$qr contains both Q and R; we only need the upper p x p triangle.
  # This computes (X'WX)^-1 directly and efficiently.
  vcov_matrix <- chol2inv(fit$qr$qr[1:p, 1:p, drop = FALSE])

  # 4. Assign names from the design matrix for usability
  # Note: If you added an intercept manually, ensure x_matrix has that column name
  colnames(vcov_matrix) <- rownames(vcov_matrix) <- colnames(x_matrix)[1:p]

  return(vcov_matrix)
}


calc_nb <- function(sample, pred_val = "pi", z = settings$z) {
  nb0 <- 0
  nb1 <- mean((sample[, pred_val] >= z) * (sample[, 'Y'] == 1)) -
    mean((sample[, pred_val] >= z) * (sample[, 'Y'] == 0)) * z / (1 - z)
  nb2 <- mean(sample[, 'Y'] == 1) - mean(sample[, 'Y'] == 0) * z / (1 - z)
  c(nb0, nb1, nb2)
}


pool_logistic_random <- function(beta_matrix, cov_list) {
  # Automatically detect dimensions
  n_studies <- nrow(beta_matrix)
  n_params <- ncol(beta_matrix) # Detects Intercept + P predictors

  # 1. Prepare long-format data
  yi <- as.vector(t(beta_matrix))
  V <- bldiag(cov_list)

  study_id <- rep(1:n_studies, each = n_params)
  param_id <- rep(1:n_params, n_studies)

  # Component factor: Intercept vs All Predictors
  # Column 1 is Intercept, 2:P are Predictors
  comp <- rep(c("Intercept", "Predictor"), times = c(1, n_params - 1))
  comp_long <- rep(comp, n_studies)

  # 2. Fit the Model
  # Random effects on Intercept only (sigma2[1]); Predictors fixed (sigma2[2]=0)
  model <- rma.mv(
    yi ~ factor(param_id) - 1,
    V = V,
    random = ~ comp_long | study_id,
    struct = "DIAG",
    sigma2 = c(0.1, 0),
    data = data.frame(yi, study_id, param_id, comp_long)
  )

  # 3. Extract Pooled Fixed Effects
  param_names <- c("Intercept", paste0("Predictor_", 1:(n_params - 1)))
  fixed_effects <- data.frame(
    term = param_names,
    estimate = as.vector(model$beta),
    se = model$se,
    ci_lb = model$ci.lb,
    ci_ub = model$ci.ub
  )

  # 4. Extract and Filter Random Effects
  re_raw <- ranef(model)[[1]]
  # Identify rows containing "Intercept" to get 1 row per study
  random_effects <- re_raw[grep("Intercept", rownames(re_raw)), ]

  random_effects_df <- data.frame(
    study_id = 1:n_studies,
    intercept_deviation = random_effects$intrcpt,
    se = random_effects$se,
    pi_lb = random_effects$pi.lb,
    pi_ub = random_effects$pi.ub
  )

  return(list(
    model = "RE",
    fixed_effects = fixed_effects,
    fixed_effects_vcov = vcov(model),
    random_effects = random_effects_df,
    tau2_intercept = model$sigma2[1],
    n_params = n_params,
    raw_model = model
  ))
}


predict_pooled_logistic_random <- function(
  pool_results,
  new_X,
  method = "pm", # "pm", "pe", "sobol"
  study_index = NULL,
  m_sobol = NULL
) {
  # pool_results: output from the function above
  # new_X: matrix of predictors (N_obs x P-1)

  # 1. Extract dimensions and coefficients
  n_params <- pool_results$n_params
  pooled_int <- pool_results$fixed_effects$estimate[1]
  betas <- pool_results$fixed_effects$estimate[2:n_params]
  tau2 <- pool_results$tau2_intercept

  new_X <- as.matrix(new_X)

  # Check if new_X dimensions match the model
  if (ncol(new_X) != (n_params - 1)) {
    stop(paste("new_X must have", n_params - 1, "columns."))
  }

  # 2. Case A: Existing Study (Conditional)
  if (!is.null(study_index)) {
    deviation <- pool_results$random_effects$intercept_deviation[study_index]
    log_odds <- (pooled_int + deviation) + (new_X %*% betas)

    if (method == "pe") {
      return(as.vector(plogis(log_odds)))
    }

    covmat <- pool_results$fixed_effects_vcov
    covmat[1, 1] <- covmat[1, 1] +
      pool_results$random_effects[study_index, 'se']^2

    if (method == "sobol") {
      models <- generate_sobol_mvn(
        method,
        c(deviation + pooled_int, betas),
        covmat
      )
      probabilities <- rowMeans(plogis(cbind(1, new_X) %*% t(models)))
      return(probabilities)
    }

    if (method == "pm") {
      X <- cbind(1, new_X)
      eta <- X %*% c(deviation + pooled_int, betas)
      se_eta <- sqrt(rowSums(X %*% covmat * X))
      probs <- sapply(1:length(eta), function(idx) {
        mcmapper:::elogitnorm(eta[idx], se_eta[idx])
      })
      return(probs)
    }
    stop(
      "m was not properly defined it should be a non-negative integer, with m=0 (default) indicating posterior mean calculations"
    )
    # 3. Case B: New Population (Numerical Integration)
  } else {
    stop(
      "Study id not found - there should be no need for prediction for a new centre!"
    )
    # fixed_lp <- pooled_int + (new_X %*% betas)
    #
    # # Handle zero heterogeneity case
    # if (tau2 < 1e-9) {
    #   return(as.vector(plogis(fixed_lp)))
    # }
    #
    # tau <- sqrt(tau2)
    #
    # # Numerical integration using adaptive quadrature
    # marginal_calc <- function(lp_val) {
    #   integrand <- function(u) {
    #     plogis(lp_val + u) * dnorm(u, mean = 0, sd = tau)
    #   }
    #   # Integrate over the range where the normal density is non-zero
    #   res <- try(
    #     integrate(integrand, lower = -8 * tau, upper = 8 * tau)$value,
    #     silent = TRUE
    #   )
    #   if (inherits(res, "try-error")) {
    #     return(plogis(lp_val))
    #   }
    #   return(res)
    # }
    #
    # probabilities <- sapply(fixed_lp, marginal_calc)
    # return(as.vector(probabilities))
  }
}


pool_logistic_fixed <- function(beta_matrix, cov_list) {
  library(metafor)

  # 1. Automatically detect dimensions
  n_studies <- nrow(beta_matrix)
  n_params <- ncol(beta_matrix)

  # 2. Prepare long-format data
  # Flattening betas: Study1(b0, b1...), Study2(b0, b1...)
  yi <- as.vector(t(beta_matrix))

  # Construct the block-diagonal covariance matrix
  V <- bldiag(cov_list)

  # Create identifiers for the long format
  study_id <- rep(1:n_studies, each = n_params)
  param_id <- rep(1:n_params, n_studies)

  # 3. Fit the Fixed-Effects Model
  # We remove the 'random' argument entirely to force a fixed-effects model
  model <- rma.mv(
    yi ~ factor(param_id) - 1,
    V = V,
    data = data.frame(yi, study_id, param_id)
  )

  # 4. Extract Pooled Estimates
  param_names <- c("Intercept", paste0("Predictor_", 1:(n_params - 1)))

  results_df <- data.frame(
    term = param_names,
    estimate = as.vector(model$beta),
    std.error = model$se,
    z_stat = model$zval,
    p_val = model$pval,
    ci_lb = model$ci.lb,
    ci_ub = model$ci.ub
  )

  # 5. Return Results
  return(list(
    model = "FE",
    fixed_effects = results_df,
    fixed_effects_vcov = vcov(model),
    n_studies = n_studies,
    n_params = n_params,
    QE = model$QE, # Test for Heterogeneity
    QEp = model$QEp, # p-value for Heterogeneity
    raw_model = model
  ))
}


# method: pm: posterior mean; pe: point estimate; sobol: sobol method
predict_pooled_logistic_fixed <- function(
  pool_results,
  new_X,
  method = "pm",
  m_sobol = NULL
) {
  # pool_results: output from your updated pool_logistic_fixed function
  # new_X: matrix or data frame of predictors (N_obs x P-1)

  # 1. Extract dimensions and coefficients from your updated list structure
  n_params <- pool_results$n_params

  # Accessing the 'fixed_effects' dataframe from your pooler output
  estimates <- pool_results$fixed_effects$estimate

  pooled_int <- estimates[1] # The common Intercept
  betas <- estimates[2:n_params] # The common Predictor coefficients

  new_X <- as.matrix(new_X)

  # 2. Validation
  if (ncol(new_X) != (n_params - 1)) {
    stop(paste(
      "new_X must have",
      n_params - 1,
      "columns to match the model (excluding Intercept)."
    ))
  }

  if (method == "pe") {
    lp <- pooled_int + (new_X %*% betas)
    probabilities <- plogis(as.vector(lp))
    return(probabilities)
  }

  covmat <- pool_results$fixed_effects_vcov

  if (method == "pm") {
    X <- cbind(1, new_X)
    eta <- X %*% estimates
    se_eta <- sqrt(rowSums(X %*% covmat * X))
    probs <- sapply(1:length(eta), function(idx) {
      mcmapper:::elogitnorm(eta[idx], se_eta[idx])
    })
    return(probs)
  }

  if (method == "sobol") {
    models <- generate_sobol_mvn(
      m_sobol,
      estimates,
      covmat
    )
    probabilities <- rowMeans(plogis(cbind(1, new_X) %*% t(models)))
    return(probabilities)
  }
}


D_KL <- function(p, q) {
  mean(p * log2(p / q) + (1 - p) * log2((1 - p) / (1 - q)))
}


generate_sobol_mvn <- function(n, mean_vec, sigma_mat) {
  # 1. Determine the dimension
  d <- length(mean_vec)

  # 2. Basic validation
  if (nrow(sigma_mat) != d || ncol(sigma_mat) != d) {
    stop("Dimensions of mean_vec and sigma_mat do not match.")
  }

  # 3. Generate N Sobol points in d dimensions (values between 0 and 1)
  # We use scrambling to improve the properties of the sequence
  sob_points <- suppressWarnings(randtoolbox::sobol(
    n = n,
    dim = d,
    scrambling = 1
  ))

  # 4. Transform [0, 1] points to Standard Normal N(0, 1) points
  # Using the quantile function (Inverse CDF)
  z_points <- qnorm(sob_points)

  # 5. Apply the Cholesky Decomposition to get the target covariance
  # sigma_mat = L %*% t(L)
  # We use the lower triangular matrix
  L <- chol(sigma_mat)

  # 6. Shift and scale: X = mu + ZL (where Z is N x d)
  # Note: Since z_points is N x d, we multiply by L (d x d)
  mvn_points <- t(apply(z_points, 1, function(z) mean_vec + t(L) %*% z))

  return(mvn_points)
}


apply_formula <- function(f, df) {
  # 1. Convert formula to string and remove the tilde (~)
  f_str <- as.character(f)
  if (length(f_str) > 1) {
    f_str <- f_str[length(f_str)]
  }

  # 2. Replace the R interaction colon (:) with standard multiplication (*)
  # so that R can evaluate it as a math expression
  math_string <- gsub(":", "*", f_str)

  # 3. Evaluate the string using the data frame columns as variables
  result <- with(df, eval(parse(text = math_string)))

  return(result)
}


apply_formula_mx <- function(f, X) {
  f_str <- as.character(f)
  if (length(f_str) > 1) {
    f_str <- f_str[length(f_str)]
  }

  # Replace X1, X2, etc. with X[,1], X[,2], etc.
  math_string <- gsub("X(\\d+)", "X[,\\1]", f_str)

  # Replace interaction colon with multiplication
  math_string <- gsub(":", "*", math_string)

  eval(parse(text = math_string))
}


extract_terms <- function(f) {
  fm_str <- as.character(f)[2] # grabs the RHS

  # Extract term names with regex
  terms_vec <- regmatches(
    fm_str,
    gregexpr("[A-Za-z][A-Za-z0-9]*(?::[A-Za-z][A-Za-z0-9]*)*", fm_str)
  )[[1]]

  terms_vec
}


extract_coefs <- function(f) {
  fm_str <- as.character(f)[2]

  # Extract all "coefficient * term" pairs
  matches <- str_extract_all(
    fm_str,
    "-?\\d+(\\.\\d+)?\\s*\\*\\s*[A-Za-z][A-Za-z0-9]*(:[A-Za-z][A-Za-z0-9]*)*"
  )[[1]]

  intercept <- as.numeric(str_extract(fm_str, "^\\s*-?\\d+(\\.\\d+)?"))

  coef_vec <- unname(setNames(
    as.numeric(str_extract(matches, "-?\\d+(\\.\\d+)?")),
    str_extract(matches, "[A-Za-z][A-Za-z0-9]*(:[A-Za-z][A-Za-z0-9]*)*")
  ))

  c(intercept, coef_vec)
}


model_matrix_mx <- function(fm, mx) {
  # Get term labels e.g. c("X1", "X2", "X1:X2")
  terms_vec <- attr(terms(fm), "term.labels")

  # For each term, evaluate it against the matrix columns
  cols <- lapply(terms_vec, function(term) {
    # Replace X1 -> mx[,"X1"], colon -> *
    expr <- gsub("(\\w+)", 'mx[,"\\1"]', term)
    expr <- gsub(":", "*", expr)
    eval(parse(text = expr))
  })

  # Combine into a matrix
  do.call(cbind, setNames(cols, terms_vec))
}


elogitnorm <- function(
  mu,
  sigma,
  method = c("quadrature", "mackay", "integrate"),
  control = list()
) {
  method <- match.arg(method)

  switch(
    method,
    mackay = {
      kappa <- 1 / sqrt(1 + (base::pi / 8) * sigma^2)
      plogis(kappa * mu)
    },
    integrate = {
      mapply(mcmapper:::elogitnorm, mu, sigma)
    },
    quadrature = {
      K <- if (is.null(control$K)) 30 else control$K
      gh <- statmod::gauss.quad(K, kind = "hermite")
      z <- gh$nodes
      w <- gh$weights
      n <- length(mu)
      X <- matrix(mu, n, K) +
        matrix(sigma * sqrt(2), n, K) * matrix(z, n, K, byrow = TRUE)
      drop(plogis(X) %*% w) / sqrt(pi)
    }
  )
}


predict_pm <- function(
  X,
  coefs,
  V,
  method = c("quadrature", "integrate", "mackay"),
  return_sd = FALSE
) {
  method <- match.arg(method)
  # Ensure inputs are matrices/vectors
  X <- as.matrix(X)
  eta <- as.vector(X %*% coefs)
  # Efficiently compute diagonal of X %*% V %*% t(X)
  se_eta <- sqrt(rowSums((X %*% V) * X))
  pi_val <- elogitnorm(eta, se_eta, method)

  if (return_sd) {
    return(list(pi = pi_val, sd_eta = se_eta))
  }
  pi_val
}


# =========================================================================
# Soft-label c-statistic (weighted AUC where each observation has a
# probabilistic "case" identity).
#
# Definition:
#   c = sum_{i != j} pi_i (1 - pi_j) * I[pred_i > pred_j]
#       --------------------------------------------------
#       sum_{i != j} pi_i (1 - pi_j)
#
# with ties contributing 0.5. Equivalent to the AUC under the model
# where each observation is independently a case with probability pi_i.
#
# Complexity: O(n log n), via sorting + cumulative sums.
# =========================================================================

cstat_soft <- function(pred, pi_true, sorted = FALSE) {
  if (length(pred) != length(pi_true)) {
    stop("length mismatch")
  }
  ok <- !is.na(pred) & !is.na(pi_true)
  pred <- pred[ok]
  pi_true <- pi_true[ok]
  n <- length(pred)
  if (n < 2) {
    return(NA_real_)
  }

  # Sort by prediction (skip if caller guarantees sorted input)
  if (sorted) {
    pred_s <- pred
    case_w <- pi_true
    ctrl_w <- 1 - pi_true
  } else {
    ord <- order(pred)
    pred_s <- pred[ord]
    case_w <- pi_true[ord]
    ctrl_w <- 1 - pi_true[ord]
  }

  # Total case- and control-weight (used in denominator and tie correction)
  total_case <- sum(case_w)
  total_ctrl <- sum(ctrl_w)

  # Group observations sharing the same prediction value to handle ties.
  # rle on the sorted predictions identifies tie groups in O(n).
  rl <- rle(pred_s)
  group <- rep(seq_along(rl$lengths), rl$lengths)
  end <- cumsum(rl$lengths)
  start <- end - rl$lengths + 1

  # For each group g, control-weight strictly below g (predictions < group's value)
  # is the cumulative control-weight up to start[g] - 1.
  cum_ctrl_full <- c(0, cumsum(ctrl_w)) # length n+1
  ctrl_below <- cum_ctrl_full[start] # weight of controls strictly below

  # Sum case-weight per group
  case_per_group <- as.numeric(tapply(case_w, group, sum))
  ctrl_per_group <- as.numeric(tapply(ctrl_w, group, sum))

  # Concordant contributions: cases in group g paired with controls strictly below
  num_concordant <- sum(case_per_group * ctrl_below)

  # Tie contributions (half-credit) WITHIN each group of equal predictions.
  # For obs i,j in the same group with i != j, contribution is
  #   0.5 * pi_i * (1 - pi_j) + 0.5 * pi_j * (1 - pi_i)
  # Summed over i != j in group g this is
  #   0.5 * (case_g * ctrl_g - sum(case_w_i * ctrl_w_i for i in g))
  # The subtracted term removes the i = j diagonal (which doesn't count).
  case_ctrl_diag_in_group <- as.numeric(tapply(case_w * ctrl_w, group, sum))
  num_ties <- 0.5 *
    sum(case_per_group * ctrl_per_group - case_ctrl_diag_in_group)

  # Denominator: all i != j case-weight × control-weight pairs.
  # = (sum case_w) * (sum ctrl_w) - sum(case_w_i * ctrl_w_i) (diagonal)
  den <- total_case * total_ctrl - sum(case_w * ctrl_w)
  if (den <= 0) {
    return(NA_real_)
  }

  (num_concordant + num_ties) / den
}


# =========================================================================
# Verification harness
# Compares cstat_soft against:
#   1. A naive O(n^2) implementation (definition direct)
#   2. The WeightedROC stacking trick (if WeightedROC is installed)
#   3. pROC::auc on hard labels, when pi_true is binary (sanity check
#      that the soft-label formula reduces to standard AUC)
# Run this once to convince yourself the implementation is right.
# =========================================================================

cstat_soft_naive <- function(pred, pi_true) {
  n <- length(pred)
  num <- 0
  den <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        next
      }
      w <- pi_true[i] * (1 - pi_true[j])
      tie <- (pred[i] == pred[j])
      num <- num + w * (as.numeric(pred[i] > pred[j]) + 0.5 * tie)
      den <- den + w
    }
  }
  num / den
}

verify_cstat_soft <- function() {
  set.seed(42)

  # Test 1: small example, no ties
  n <- 100
  pi_true <- runif(n, 0.05, 0.5)
  pred <- pi_true + rnorm(n, 0, 0.05)
  pred <- pmin(pmax(pred, 0.001), 0.999)

  fast <- cstat_soft(pred, pi_true)
  naive <- cstat_soft_naive(pred, pi_true)
  cat(sprintf(
    "Test 1 (n=100, no ties):     fast=%.6f  naive=%.6f  diff=%.2e\n",
    fast,
    naive,
    abs(fast - naive)
  ))

  # Test 2: with ties (force some)
  pred_tied <- round(pred, 2) # creates ties
  fast <- cstat_soft(pred_tied, pi_true)
  naive <- cstat_soft_naive(pred_tied, pi_true)
  cat(sprintf(
    "Test 2 (n=100, with ties):   fast=%.6f  naive=%.6f  diff=%.2e\n",
    fast,
    naive,
    abs(fast - naive)
  ))

  # Test 3: hard labels (pi_true in {0,1}) — should match standard AUC
  if (requireNamespace("pROC", quietly = TRUE)) {
    pi_hard <- as.numeric(runif(n) < pi_true)
    if (length(unique(pi_hard)) == 2) {
      fast <- cstat_soft(pred, pi_hard)
      proc <- as.numeric(pROC::auc(pi_hard, pred, quiet = TRUE))
      cat(sprintf(
        "Test 3 (hard labels vs pROC): fast=%.6f  pROC=%.6f  diff=%.2e\n",
        fast,
        proc,
        abs(fast - proc)
      ))
    }
  }

  # Test 4: WeightedROC stacking trick
  if (requireNamespace("WeightedROC", quietly = TRUE)) {
    guess <- c(pred, pred)
    label <- c(rep(1, length(pred)), rep(0, length(pred)))
    weight <- c(pi_true, 1 - pi_true)
    keep <- weight > 0
    wroc <- WeightedROC::WeightedROC(guess[keep], label[keep], weight[keep])
    trick <- WeightedROC::WeightedAUC(wroc)
    fast <- cstat_soft(pred, pi_true)
    cat(sprintf(
      "Test 4 (vs WeightedROC trick): fast=%.6f  trick=%.6f  diff=%.2e\n",
      fast,
      trick,
      abs(fast - trick)
    ))
  }

  # Test 5: timing on realistic n
  set.seed(1)
  n_big <- 1e5
  pi_big <- runif(n_big, 0.01, 0.5)
  pred_big <- pi_big + rnorm(n_big, 0, 0.05)
  pred_big <- pmin(pmax(pred_big, 0.001), 0.999)
  t0 <- Sys.time()
  v <- cstat_soft(pred_big, pi_big)
  t1 <- Sys.time()
  cat(sprintf(
    "Test 5 (n=%d):              c=%.4f, time=%.2fs\n",
    n_big,
    v,
    as.numeric(t1 - t0, units = "secs")
  ))

  invisible(NULL)
}

# Uncomment to run verification
# verify_cstat_soft()

# =============================================================================
# elogitnorm: Expected Value of a Logit-Normal Random Variable
#
# Computes E[sigma(eta)] where eta ~ N(mu, sigma).
#
# This is the simpler (unconditional) counterpart to pelogitnorm.
# There is no closed form — three methods are provided:
#
#   "mackay"      - MacKay (1992) probit rescaling:
#                     E[sigma(eta)] ~= Phi(mu / sqrt(1 + pi/8 * sigma^2))
#                   Single function call, no numerical integration needed.
#                   Accurate for moderate sigma; degrades for sigma >> 1.
#
#   "integrate"   - Direct numerical integration via R's integrate().
#                   Accurate across all regimes; slightly slower.
#
#   "monte_carlo" - Draw eta ~ N(mu, sigma), return mean(plogis(eta)).
#                   Exact in the limit; returns Monte Carlo SE.
#
# Arguments:
#   mu      - mean of the latent Gaussian eta
#   sigma  -   SD of eta (must be > 0)
#   method  - "mackay", "integrate", or "monte_carlo"
#   log_p   - if TRUE, return log of the result
#   tol     - relative tolerance for numerical integration
#             (ignored for mackay and monte_carlo)
#   mc_T    - number of Monte Carlo samples (only for monte_carlo)
#   mc_seed - optional RNG seed for reproducibility (only for monte_carlo)
#
# Returns a list with:
#   $ev     - E[sigma(eta)]
#   $method - method used
#   $mc_se  - Monte Carlo standard error (only for monte_carlo method)
# =============================================================================

pelogitnorm <- function(
  mu,
  sigma,
  z,
  method = c("integrate", "probit", "monte_carlo")
) {
  method <- match.arg(method)
  zeta <- qlogis(z) # Truncation point on the logit scale

  # 1. Denominator: P(eta > zeta)
  denom <- pnorm(zeta, mu, sigma, lower.tail = FALSE)

  # 2. Numerator: Integral of plogis(eta) * pdf(eta) from zeta to Inf
  if (method == "integrate") {
    num <- integrate(
      function(x) plogis(x) * dnorm(x, mu, sigma),
      lower = zeta,
      upper = Inf
    )$value
  } else if (method == "probit") {
    kappa <- 1 / sqrt(1 + (pi / 8) * sigma^2)
    rho <- (kappa * sigma) / sqrt(1 + (kappa * sigma)^2)
    upper1 <- (kappa * mu) / sqrt(1 + (kappa * sigma)^2)
    upper2 <- (mu - zeta) / sigma

    num <- mvtnorm::pmvnorm(
      upper = c(upper1, upper2),
      corr = matrix(c(1, rho, rho, 1), 2, 2)
    )[1]
  } else if (method == "monte_carlo") {
    eta <- rnorm(1e6, mu, sigma)
    num <- mean(plogis(eta[eta > zeta])) * denom # Back-calculating numerator for consistency
    return(mean(plogis(eta[eta > zeta]))) # Or just return mean of kept samples
  }

  return(num / denom)
}


evpi_ind <- function(mu, sigma, z) {
  tp <- 1 - mcmapper:::plogitnorm(z, mu, sigma)
  pe <- pelogitnorm(mu, sigma, z)
  T1 <- tp * pe + (1 - tp) * z - z #E(max(p,z))-z = E(max(p-z,0))
  T2 <- max(elogitnorm(mu, sigma) - z, 0)

  (T1 - T2) / (1 - z)
}


svcov <- function(model) {
  # 1. Extract basic components
  if (!inherits(model, "glm") || model$family$family != "binomial") {
    stop("This function is designed for logistic GLMs.")
  }

  X <- model.matrix(model)
  p <- fitted(model)
  y <- model$y

  # 2. The Bread (B): Inverse Hessian
  # For logistic, Hessian is X'WX where W = diag(p*(1-p))
  w_s <- p * (1 - p)
  bread_inv <- solve(crossprod(X, X * w_s))

  # 3. The Meat (M): Variance of the scores
  # The scores are (y - p) * X
  res <- y - p
  meat <- crossprod(X * res)

  # 4. The Sandwich: B^-1 %*% M %*% B^-1
  vcov_hc0 <- bread_inv %*% meat %*% bread_inv

  # Add names for clarity
  colnames(vcov_hc0) <- rownames(vcov_hc0) <- colnames(X)

  return(vcov_hc0)
}


# =========================================================================
# logf_glm: logistic regression with a log-F(m, m) prior on slopes via
# Greenland's data-augmentation trick.
#
# Reference: Greenland and Mansournia (2015), Penalization, bias reduction,
# and default priors in logistic and related categorical and survival
# regressions. Statistics in Medicine, 34(23): 3133-3143.
#
# Defaults reflect Greenland & Mansournia's recommendations:
#   - m = 1 (log-F(1, 1), their preferred general-purpose default; matches
#     Firth / Jeffreys in the one-parameter case).
#   - Intercept flat (penalize_intercept = FALSE). Preserves calibration-
#     in-the-large at the MAP.
#   - vcov from the augmented fit (not the exact-Laplace-at-beta-hat
#     version). Fast and matches what glm's vcov() returns on the
#     augmented data.
#
# Standardization:
#   standardize = FALSE (default) — follows Greenland & Mansournia
#   Section 7, which argues for manually scaling predictors to
#   contextually meaningful units rather than relying on SD-based
#   standardization. Set standardize = TRUE for an automated pipeline
#   where predictors have very different natural scales; the function
#   then internally scales non-intercept columns to training SD = 1
#   before fitting and back-transforms coefficients and vcov afterwards.
#
# The m argument can be a scalar or a vector of length ncol(x) with
# NA / Inf indicating "leave this coefficient unpenalised". The intercept
# is always unpenalised by default regardless of its m-entry.
# =========================================================================

logf_glm <- function(
  x,
  y,
  m = 2,
  weights = NULL,
  vcov_method = "augmented", # c("exact", "augmented"),
  standardize = FALSE,
  penalize_intercept = FALSE,
  intercept_col = 1,
  tol = 1e-8,
  maxit = 50
) {
  vcov_method <- match.arg(vcov_method, c("augmented", "exact"))
  if (!is.matrix(x)) {
    stop("x must be a matrix (use model.matrix())")
  }
  if (length(y) != nrow(x)) {
    stop("length(y) must equal nrow(x)")
  }

  n <- nrow(x)
  p <- ncol(x)
  if (is.null(weights)) {
    weights <- rep(1, n)
  }

  # Expand scalar m to a vector of length p
  if (length(m) == 1L) {
    m <- rep(m, p)
  } else if (length(m) != p) {
    stop(sprintf(
      "m must be length 1 or ncol(x) = %d; got length %d",
      p,
      length(m)
    ))
  }

  # Determine which coefficients are penalised:
  #   - m must be finite and positive
  #   - intercept gets flat prior by default (penalize_intercept = FALSE)
  is_finite_pos <- is.finite(m) & !is.na(m) & m > 0
  is_penalised <- is_finite_pos
  if (!penalize_intercept) {
    if (intercept_col < 1 || intercept_col > p) {
      stop("intercept_col out of range")
    }
    is_penalised[intercept_col] <- FALSE
  }
  penalised_idx <- which(is_penalised)
  n_pen <- length(penalised_idx)
  m_pen <- m[penalised_idx] # per-coefficient prior strengths

  # ---- Standardization (optional) ----
  # Divide non-intercept columns by their training SD. All downstream
  # fitting uses the standardized matrix; we back-transform at the end.
  # The penalty is applied to the STANDARDIZED coefficients: per-
  # standardized-unit shrinkage rather than per-raw-unit.
  s <- rep(1.0, p)
  if (standardize) {
    slope_cols <- setdiff(seq_len(p), intercept_col)
    if (length(slope_cols) > 0L) {
      s_sl <- apply(x[, slope_cols, drop = FALSE], 2L, stats::sd)
      # Zero-variance guard: leave those columns unscaled
      s_sl[s_sl == 0 | !is.finite(s_sl)] <- 1
      s[slope_cols] <- s_sl
      x_fit <- x
      x_fit[, slope_cols] <- sweep(
        x[, slope_cols, drop = FALSE],
        2L,
        s_sl,
        `/`
      )
    } else {
      x_fit <- x
    }
  } else {
    x_fit <- x
  }

  # ---- Build augmented data ----
  # For each penalised coefficient j with prior strength m_j, add a
  # pseudo-row with predictor 1 in column j and 0 elsewhere (including
  # the intercept column), pseudo-response 0.5 on weight m_j
  # (so y* = m_j/2 successes on m_j trials). Pseudo-rows having 0 in
  # the intercept column is what preserves calibration-in-the-large:
  # the intercept score equation on the augmented data reduces to the
  # unpenalised form on the real data (since pseudo-rows contribute
  # nothing to the intercept-column score sum).
  if (n_pen > 0) {
    X_star <- matrix(0, nrow = n_pen, ncol = p)
    for (k in seq_along(penalised_idx)) {
      X_star[k, penalised_idx[k]] <- 1
    }
    colnames(X_star) <- colnames(x_fit)
    y_star <- rep(0.5, n_pen)
    w_star <- m_pen # one weight per penalised coef

    X_aug <- rbind(x_fit, X_star)
    y_aug <- c(y, y_star)
    w_aug <- c(weights, w_star)
  } else {
    X_aug <- x_fit
    y_aug <- y
    w_aug <- weights
  }

  # ---- Fit via glm.fit ----
  fit <- suppressWarnings(
    glm.fit(
      X_aug,
      y_aug,
      weights = w_aug,
      family = binomial(),
      control = list(epsilon = tol, maxit = maxit)
    )
  )
  if (!fit$converged) {
    warning("logf_glm: glm.fit did not converge")
  }

  beta <- fit$coefficients # on the STANDARDIZED scale if standardize=TRUE
  names(beta) <- colnames(x_fit)

  # ---- Covariance (on standardized scale) ----
  # Augmented covariance: (X_aug^T W_aug X_aug)^{-1}, using pi(beta) from
  # the augmented fit. This is what vcov(glm) on the augmented data gives.
  # Exact covariance: (X^T W X + Lambda(beta))^{-1} using only the original
  # data and the analytic prior Hessian at beta_hat.

  eta <- as.numeric(x_fit %*% beta)
  pi_hat <- plogis(eta)
  w_data <- weights * pi_hat * (1 - pi_hat) # original-data IRLS weights
  XtWX <- crossprod(x_fit * sqrt(w_data))

  # Analytic negative Hessian of log-F(m_j, m_j) log-prior at beta_j:
  #   log p(beta_j) = const - m_j * log(2 cosh(beta_j / 2))
  # Second derivative:
  #   -d2/dbeta_j2 log p = (m_j/4) * sech^2(beta_j / 2)
  # At beta_j = 0, this is m_j/4 — matching the pseudo-observation
  # contribution (weight m_j * 0.25 = m_j/4 at pi* = 0.5).
  Lambda <- rep(0, p)
  if (n_pen > 0) {
    for (k in seq_along(penalised_idx)) {
      j <- penalised_idx[k]
      Lambda[j] <- (m_pen[k] / 4) / (cosh(beta[j] / 2))^2
    }
  }
  LambdaDiag <- diag(Lambda, nrow = p, ncol = p)

  if (vcov_method == "exact") {
    V <- tryCatch(
      solve(XtWX + LambdaDiag),
      error = function(e) {
        warning(
          "logf_glm: exact covariance matrix is singular; ",
          "falling back to augmented vcov"
        )
        NULL
      }
    )
    if (is.null(V)) {
      vcov_method <- "augmented"
    }
  }
  if (vcov_method == "augmented") {
    # Use the glm fit's standard vcov on augmented data.
    w_aug_irls <- w_aug * fit$fitted.values * (1 - fit$fitted.values)
    V <- tryCatch(
      solve(crossprod(X_aug * sqrt(w_aug_irls))),
      error = function(e) {
        stop("logf_glm: augmented information matrix is singular")
      }
    )
  }

  # ---- Back-transform to original scale ----
  # Only scaling, no centering: beta_orig_j = beta_std_j / s_j and
  # V_orig[j,k] = V_std[j,k] / (s_j * s_k).
  inv_s <- 1 / s
  beta_out <- beta * inv_s
  V_out <- V * tcrossprod(inv_s, inv_s)

  names(beta_out) <- colnames(x)
  dimnames(V_out) <- list(colnames(x), colnames(x))

  # Marginal posterior SEs (sqrt of diagonal of joint covariance).
  se_out <- sqrt(diag(V_out))
  names(se_out) <- colnames(x)

  # eta and fitted values: identical on both scales by construction.
  structure(
    list(
      coefficients = beta_out,
      se = se_out,
      vcov = V_out,
      vcov_method = vcov_method,
      converged = fit$converged,
      m = m,
      standardize = standardize,
      training_sd = s,
      penalised_idx = penalised_idx,
      linear.predictors = eta,
      fitted.values = pi_hat,
      n = n,
      y = y
    ),
    class = "logf_glm"
  )
}

# S3 accessors
coef.logf_glm <- function(object, ...) object$coefficients
vcov.logf_glm <- function(object, ...) object$vcov

# Register S3 methods so dispatch works in parallel workers / fresh sessions.
if (getRversion() >= "3.6") {
  .S3method("coef", "logf_glm", coef.logf_glm)
  .S3method("vcov", "logf_glm", vcov.logf_glm)
}


bayesian_ridge <- function(X, y, unconditional = TRUE) {
  require(mgcv)

  X <- as.matrix(X)
  p <- ncol(X)

  Xs <- scale(X[, -1, drop = FALSE])

  mus <- c(0, attr(Xs, "scaled:center"))
  sds <- c(1, attr(Xs, "scaled:scale"))

  coef_names <- colnames(X)[-1]
  form <- as.formula(
    paste("y ~ 1 +", paste(coef_names, collapse = " + "))
  )

  p <- ncol(Xs)
  S <- diag(p)

  # The name in paraPen MUST match the name of the matrix in the formula
  pen_list <- list("Xs" = list(S))
  # 3. Fit the model
  # By using 'y ~ Xs', gam treats the entire matrix as one parametric block
  dat <- data.frame(y = y)
  dat$Xs <- Xs

  fit <- gam(
    y ~ Xs,
    data = dat,
    family = "binomial",
    method = "REML",
    paraPen = pen_list
  )

  #fit2 <- glm(form, data = df, family = binomial())

  beta_z <- fit$coefficients
  V_z <- vcov(fit, unconditional = unconditional)

  D <- diag(1 / sds)
  D[1, -1] <- -mus[-1] / sds[-1]

  beta <- as.numeric(D %*% beta_z)
  V <- D %*% V_z %*% t(D)

  list(beta = beta, V = V)
}


#' Constrained KL Projection from PPM Risks to a Target Prevalence
#'
#' @param p_ppm Vector of pre-calculated Posterior Predictive Mean risks (length n)
#' @param X Design matrix (n x p), should include the intercept column
#' @param target_sum The target sum of predictions (e.g., sum(y) or n * prevalence)
#' @param beta_start Optional: Starting values for the optimization
#'
#' @return A vector of coefficients for the projected logistic model
project_constrained_kl <- function(p_ppm, X, target_sum, beta_start = NULL) {
  if (!requireNamespace("nloptr", quietly = TRUE)) {
    stop("Please install 'nloptr'")
  }

  X <- as.matrix(X)
  n <- length(p_ppm)

  # 1. Objective Function: Negative Cross-Entropy (KL Divergence)
  # Minimizing KL(p_ppm || q)
  eval_f <- function(params) {
    eta_q <- as.numeric(X %*% params)
    q <- plogis(eta_q)

    # Negative log-likelihood form of KL
    # Note: p_ppm acts as the 'observed' probability target
    eps <- 1e-12
    val <- -sum(p_ppm * log(q + eps) + (1 - p_ppm) * log(1 - q + eps))

    # Gradient: d(KL)/d(beta) = sum( (q - p_ppm) * x_i )
    grad <- colSums((q - p_ppm) * X)

    return(list("objective" = val, "gradient" = grad))
  }

  # 2. Equality Constraint: sum(q) - target_sum = 0
  eval_g_eq <- function(params) {
    q <- plogis(as.numeric(X %*% params))
    val <- sum(q) - target_sum

    # Jacobian: d(sum_q)/d(beta) = sum( q * (1 - q) * x_i )
    jac <- colSums(q * (1 - q) * X)

    return(list("constraints" = val, "jacobian" = jac))
  }

  # 3. Starting Values
  if (is.null(beta_start)) {
    p_adj <- (p_ppm * (n - 1) + 0.5) / n
    beta_start <- coef(lm(qlogis(p_adj) ~ X - 1))
  }

  # 4. Optimization via SLSQP
  res <- nloptr::nloptr(
    x0 = beta_start,
    eval_f = eval_f,
    eval_g_eq = eval_g_eq,
    opts = list(
      "algorithm" = "NLOPT_LD_SLSQP",
      "xtol_rel" = 1.0e-10,
      "maxeval" = 1000
    )
  )

  if (res$status < 1) {
    warning("Optimization message: ", res$message)
  }

  return(res$solution)
}


# =========================================================================
# MCMC hierarchical ridge via a hand-written Stan model (compile once, reuse).
#
# Faithful fully-Bayesian analogue of the mgcv REML ridge (bayesian_ridge):
# the common shrinkage scale `tau` is ESTIMATED via a half-Student-t(3, 0, 1)
# hyperprior and shared across all slopes. Predictors are standardized to
# training SD = 1, so one shared tau penalizes all slopes equally — exactly
# mgcv's S = I penalty on the standardized scale. The intercept has NO prior
# statement (flat / improper uniform), so it is completely unpenalized,
# matching mgcv (intercept outside paraPen) and glmnet (penalty.factor = 0).
#
# Model specification is identical across the three ridge methods (mgcv /
# glmnet / this MCMC); only HOW tau is determined differs (REML plug-in / CV /
# full-Bayes marginalization) — which is exactly what the paper studies.
#
# Implementation notes:
#   - A static Stan model (no brms code injection) is compiled once per worker
#     (lazily, cached) via rstan::stan_model, then reused via rstan::sampling.
#   - Posterior-mean predictions are computed in R from the beta/alpha draws in
#     row-chunks of the holdout set, so we never materialize a full
#     (draws x n_holdout) probability matrix — bounding peak RAM per fit.
# =========================================================================

if (!exists(".bayes_mcmc_cache")) {
  .bayes_mcmc_cache <- new.env(parent = emptyenv())
}

# Hierarchical ridge: standardized predictors X (no intercept column),
# common scale tau shared across slopes, flat intercept. bernoulli_logit_glm
# is the efficient fused GLM likelihood (Stan >= 2.18).
.ridge_stan_code <- "
data {
  int<lower=1> n;
  int<lower=1> p;
  matrix[n, p] X;                       // standardized predictors, no intercept
  array[n] int<lower=0, upper=1> y;
}
parameters {
  real alpha;                           // intercept (flat prior)
  vector[p] beta_raw;                   // standardized slopes (non-centered)
  real<lower=0> tau;                    // common shrinkage scale (estimated)
}
transformed parameters {
  vector[p] beta = beta_raw * tau;      // actual slopes ~ normal(0, tau)
}
model {
  // Non-centered parameterization: beta = beta_raw * tau with beta_raw ~ N(0,1)
  // is mathematically identical to beta ~ N(0, tau) but breaks the beta-tau
  // funnel that causes divergences at low EPV.
  tau ~ student_t(3, 0, 1);             // half-t via <lower=0> constraint
  beta_raw ~ normal(0, 1);
  // alpha: no prior => flat (improper uniform), i.e. unpenalized intercept
  y ~ bernoulli_logit_glm(X, alpha, beta);
}
"

# Lazily compile + cache the Stan model (once per worker).
get_ridge_stanmodel <- function() {
  if (is.null(.bayes_mcmc_cache$ridge_stan)) {
    .bayes_mcmc_cache$ridge_stan <- rstan::stan_model(model_code = .ridge_stan_code)
  }
  .bayes_mcmc_cache$ridge_stan
}

# Fit the hierarchical ridge by MCMC and return the posterior-mean predicted
# probabilities on the holdout, plus convergence diagnostics and (optionally)
# credible-interval coverage / PIT calibration of the linear predictor.
#
#   X_tr, X_po : training / holdout predictor matrices, NO intercept column
#                (e.g. model.matrix(...)[, -1]). Standardization is internal.
#   y          : training binary outcome.
#   eta_pseudo : (optional) length-nrow(X_po) vector of the holdout pseudo-true
#                linear predictor. If supplied together with cri_levels, the
#                empirical CrI coverage and PIT summaries of the posterior of
#                eta are computed against it. The Stan model uses standardized
#                predictors, but eta is invariant to that linear reparameter-
#                ization, so the posterior of eta matches the candidate-scale
#                linear predictor that eta_pseudo lives on.
#   cri_levels : (optional) coverage levels, e.g. c(0.95).
#   chunk      : holdout rows processed per batch when averaging over draws
#                (caps the transient draws x rows block — bounds peak RAM).
#
#   returns: list(pm, rhat_max, n_diverg, converged[, coverage, pit_F_10,
#            pit_F_50, pit_F_90]) or NULL on hard failure.
fit_ridge_stan <- function(X_tr, y, X_po, eta_pseudo = NULL, cri_levels = NULL,
                           chains = 4, iter = 2000, chunk = 2000, seed = NULL) {
  X_tr <- as.matrix(X_tr)
  X_po <- as.matrix(X_po)
  p    <- ncol(X_tr)

  # Training-set standardization (center + unit SD; zero-variance guard),
  # applied identically to the holdout. Matches mgcv / glmnet.
  center <- colMeans(X_tr)
  sds    <- apply(X_tr, 2, stats::sd)
  sds[sds == 0 | !is.finite(sds)] <- 1
  Xs_tr <- sweep(sweep(X_tr, 2, center, "-"), 2, sds, "/")
  Xs_po <- sweep(sweep(X_po, 2, center, "-"), 2, sds, "/")

  sm  <- get_ridge_stanmodel()
  # Deterministic seed (if supplied) makes the fit reproducible regardless of
  # worker / run order — required for the resumable checkpoint design.
  stan_seed <- if (is.null(seed)) sample.int(.Machine$integer.max, 1) else seed
  # adapt_delta = 0.95 (matches rstanarm) + non-centered parameterization
  # together suppress the low-EPV funnel divergences. Sampler warnings are
  # silenced here because convergence is recorded as data (rhat/diverg/ess)
  # and returned to the caller instead.
  fit <- tryCatch(
    suppressWarnings(rstan::sampling(
      sm,
      data    = list(n = nrow(Xs_tr), p = p, X = Xs_tr, y = as.integer(y)),
      chains  = chains, iter = iter, cores = 1, refresh = 0,
      pars    = c("alpha", "beta", "tau"), seed = stan_seed,
      control = list(adapt_delta = 0.95, max_treedepth = 12)
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  # Diagnostics.
  ss       <- rstan::summary(fit)$summary
  rhat_max <- suppressWarnings(max(ss[, "Rhat"],  na.rm = TRUE))
  min_ess  <- suppressWarnings(min(ss[, "n_eff"], na.rm = TRUE))
  sp       <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  n_diverg <- sum(vapply(sp, function(x) sum(x[, "divergent__"]), numeric(1)))

  draws  <- rstan::extract(fit, pars = c("alpha", "beta"))
  alpha  <- draws$alpha                 # length D
  beta   <- draws$beta                  # D x p
  if (is.null(dim(beta))) beta <- matrix(beta, ncol = p)  # p == 1 guard
  betaT  <- t(beta)                     # p x D

  do_cri <- !is.null(eta_pseudo) && length(cri_levels) > 0
  n_po   <- nrow(Xs_po)
  pm     <- numeric(n_po)
  if (do_cri) {
    a_lo    <- (1 - cri_levels) / 2
    a_hi    <- 1 - a_lo
    covered <- numeric(length(cri_levels))
    pit     <- numeric(n_po)
  }

  # Posterior-mean predictions (and, optionally, coverage / PIT), computed in
  # row-chunks of the holdout to bound the transient (rows x draws) matrix.
  starts <- seq(1, n_po, by = chunk)
  for (s in starts) {
    idx <- s:min(s + chunk - 1, n_po)
    eta <- Xs_po[idx, , drop = FALSE] %*% betaT         # |idx| x D
    eta <- sweep(eta, 2, alpha, `+`)                    # add per-draw intercept
    pm[idx] <- rowMeans(plogis(eta))                    # E[ sigma(eta) ] over draws
    if (do_cri) {
      ep        <- eta_pseudo[idx]
      pit[idx]  <- rowMeans(eta <= ep)                  # empirical PIT of eta_pseudo
      for (li in seq_along(cri_levels)) {
        qq <- t(apply(eta, 1, stats::quantile,
                      probs = c(a_lo[li], a_hi[li]), names = FALSE))
        covered[li] <- covered[li] + sum(ep >= qq[, 1] & ep <= qq[, 2])
      }
    }
  }

  out <- list(pm = pm, rhat_max = rhat_max, n_diverg = n_diverg,
              min_ess = min_ess,
              converged = n_diverg == 0 && rhat_max < 1.01)
  if (do_cri) {
    cov <- covered / n_po
    names(cov) <- sprintf("cri_cov_%d", round(100 * cri_levels))
    out$coverage <- cov
    out$pit_F_10 <- mean(pit <= 0.10)
    out$pit_F_50 <- mean(pit <= 0.50)
    out$pit_F_90 <- mean(pit <= 0.90)
  }
  out
}

# Empirical CrI coverage + PIT calibration of the linear predictor from a
# matrix of posterior linear-predictor draws (e.g. rstanarm::posterior_linpred,
# oriented draws x observations). Mirrors fit_ridge_stan's coverage block so
# the MCMC lasso/ridge use identical coverage definitions.
#   eta_draws  : D x n_obs matrix of linear-predictor draws.
#   eta_pseudo : length-n_obs pseudo-true linear predictor.
#   cri_levels : coverage levels, e.g. c(0.95).
#   chunk      : observations per batch (bounds the transposed block).
#   returns: list(coverage = named vector, pit_F_10, pit_F_50, pit_F_90).
cri_pit_from_linpred <- function(eta_draws, eta_pseudo, cri_levels,
                                 chunk = 2000) {
  n_obs   <- ncol(eta_draws)
  a_lo    <- (1 - cri_levels) / 2
  a_hi    <- 1 - a_lo
  covered <- numeric(length(cri_levels))
  pit     <- numeric(n_obs)

  starts <- seq(1, n_obs, by = chunk)
  for (s in starts) {
    idx <- s:min(s + chunk - 1, n_obs)
    blk <- t(eta_draws[, idx, drop = FALSE])            # |idx| x D
    ep  <- eta_pseudo[idx]
    pit[idx] <- rowMeans(blk <= ep)
    for (li in seq_along(cri_levels)) {
      qq <- t(apply(blk, 1, stats::quantile,
                    probs = c(a_lo[li], a_hi[li]), names = FALSE))
      covered[li] <- covered[li] + sum(ep >= qq[, 1] & ep <= qq[, 2])
    }
  }

  cov <- covered / n_obs
  names(cov) <- sprintf("cri_cov_%d", round(100 * cri_levels))
  list(coverage = cov,
       pit_F_10 = mean(pit <= 0.10),
       pit_F_50 = mean(pit <= 0.50),
       pit_F_90 = mean(pit <= 0.90))
}


