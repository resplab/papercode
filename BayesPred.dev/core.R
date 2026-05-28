# =============================================================================
# core.R — Shared utility functions for BayesCPM analyses
#
# Author:      Mohsen Sadatsafavi
# Last update: 2026.05.27
#
# =============================================================================

# log-odds transform; inverse of plogis()
logit <- function(x) {
  log(x / (1 - x))
}


# =============================================================================
# elogitnorm
#
# Computes E[plogis(eta)] where eta ~ N(mu, sigma^2) — the expected predicted
# probability when the linear predictor has a Gaussian distribution.  This
# arises naturally when integrating out posterior uncertainty in beta: if
# beta ~ N(beta_hat, V) and eta = x'beta, then eta ~ N(x'beta_hat, x'Vx).
#
# Three methods:
#   "quadrature" (default) — Gauss-Hermite quadrature with K=30 nodes.
#                            Accurate across all (mu, sigma) regimes.
#   "mackay"               — MacKay (1992) probit-rescaling approximation:
#                            E[sigma(eta)] ~= Phi(mu / sqrt(1 + pi/8 * sigma^2)).
#                            O(1) per observation; slightly less accurate for
#                            large sigma.
#   "integrate"            — Delegates to mcmapper:::elogitnorm (adaptive
#                            numerical integration). Accurate but slower.
#
# Arguments:
#   mu, sigma — mean and SD of the Gaussian linear predictor (vectorised)
#   method    — one of "quadrature", "mackay", "integrate"
#   control   — list; currently only control$K sets the quadrature order
# =============================================================================

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
      # Probit-rescaling: replaces the logistic with a rescaled probit so
      # the integral has a closed form. Accurate when sigma is small.
      kappa <- 1 / sqrt(1 + (base::pi / 8) * sigma^2)
      plogis(kappa * mu)
    },
    integrate = {
      # Adaptive numerical integration via mcmapper's internal routine.
      mapply(mcmapper:::elogitnorm, mu, sigma)
    },
    quadrature = {
      # Gauss-Hermite quadrature. The change of variables
      # eta = mu + sigma * sqrt(2) * t maps the N(mu, sigma^2) integral
      # to the standard Hermite form int f(t) exp(-t^2) dt.
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


# =============================================================================
# predict_pm — posterior-mean predicted probabilities
#
# For a logistic model whose coefficient vector has an approximate Gaussian
# posterior beta ~ N(coefs, V), computes the posterior-mean predicted
# probability for each row of X:
#
#   PM_i = E_beta[ plogis(x_i' beta) ]
#        = elogitnorm(x_i' coefs, sqrt(x_i' V x_i))
#
# This is the Bayes-optimal point prediction under squared-error loss on the
# probability scale, and is strictly shrunk toward 0.5 relative to the
# plug-in prediction plogis(x_i' coefs).
#
# Arguments:
#   X         — n x p design matrix (intercept column must be included)
#   coefs     — p-vector of posterior mean coefficients
#   V         — p x p posterior covariance matrix
#   method    — passed to elogitnorm; "quadrature" (default), "mackay",
#               or "integrate"
#   return_sd — if TRUE, also return sd_eta (per-row SD of the linear
#               predictor), useful for constructing credible intervals on
#               the logit scale
# =============================================================================

predict_pm <- function(
  X,
  coefs,
  V,
  method = c("quadrature", "integrate", "mackay"),
  return_sd = FALSE
) {
  method <- match.arg(method)
  X <- as.matrix(X)
  eta <- as.vector(X %*% coefs) # posterior-mean linear predictor
  se_eta <- sqrt(rowSums((X %*% V) * X)) # SD of eta under the posterior
  pi_val <- elogitnorm(eta, se_eta, method)

  if (return_sd) {
    return(list(pi = pi_val, sd_eta = se_eta))
  }
  pi_val
}


# =========================================================================
# cstat_soft — soft-label (probabilistic) c-statistic
#
# Weighted AUC where each observation has a probabilistic case identity
# pi_true rather than a hard 0/1 label.  Reduces to the standard AUC when
# pi_true is binary.
#
# Definition:
#   c = sum_{i != j} pi_i (1 - pi_j) * I[pred_i > pred_j]
#       --------------------------------------------------
#       sum_{i != j} pi_i (1 - pi_j)
#
# with ties contributing 0.5.
#
# Arguments:
#   pred    — n-vector of predicted probabilities (or any numeric score)
#   pi_true — n-vector of true probabilities (soft labels in [0,1], or
#             hard 0/1 labels for the standard AUC)
#   sorted  — set TRUE if pred is already sorted ascending (skips re-sort)
#
# Complexity: O(n log n) via sorting + cumulative sums.
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

  total_case <- sum(case_w)
  total_ctrl <- sum(ctrl_w)

  # Identify tie groups in O(n) using run-length encoding on the sorted pred.
  rl <- rle(pred_s)
  group <- rep(seq_along(rl$lengths), rl$lengths)
  end <- cumsum(rl$lengths)
  start <- end - rl$lengths + 1

  # Cumulative control-weight; ctrl_below[g] = weight of controls with
  # predictions strictly less than group g's prediction value.
  cum_ctrl_full <- c(0, cumsum(ctrl_w))
  ctrl_below <- cum_ctrl_full[start]

  case_per_group <- as.numeric(tapply(case_w, group, sum))
  ctrl_per_group <- as.numeric(tapply(ctrl_w, group, sum))

  # Concordant pairs: case in group g vs. control with strictly lower pred.
  num_concordant <- sum(case_per_group * ctrl_below)

  # Tied pairs get half-credit. The within-group contribution simplifies to
  # 0.5 * (case_g * ctrl_g - sum_i(case_i * ctrl_i)) after removing the
  # i=j diagonal (a single obs cannot be both case and control).
  case_ctrl_diag_in_group <- as.numeric(tapply(case_w * ctrl_w, group, sum))
  num_ties <- 0.5 *
    sum(case_per_group * ctrl_per_group - case_ctrl_diag_in_group)

  # Denominator: total case-weight × control-weight over all i != j pairs.
  den <- total_case * total_ctrl - sum(case_w * ctrl_w)
  if (den <= 0) {
    return(NA_real_)
  }

  (num_concordant + num_ties) / den
}


# =========================================================================
# logf_glm — logistic regression with a log-F(m, m) prior on slopes
#
# Implements Greenland & Mansournia's (2015) data-augmentation trick: for
# each penalised slope, one pseudo-observation (x_j = 1, all others = 0,
# y* = 0.5, weight = m) is appended to the data before calling glm.fit().
# This is exactly equivalent to placing a log-F(m, m) prior on beta_j and
# finding the MAP estimate.
#
# The intercept is left unpenalised by default (penalize_intercept = FALSE),
# which preserves calibration-in-the-large at the MAP.
#
# Reference: Greenland S, Mansournia MA (2015). Statistics in Medicine,
#   34(23): 3133-3143.
#
# Arguments:
#   x                  — n x p model matrix (from model.matrix(); must
#                        include intercept column)
#   y                  — n-vector of binary outcomes (0/1)
#   m                  — prior strength; scalar or p-vector (NA/Inf = no
#                        penalty for that coefficient). Default m=2 is
#                        weakly informative per Greenland & Mansournia.
#   weights            — optional observation weights (length n)
#   vcov_method        — "augmented" (default): vcov from the augmented
#                        glm.fit, matching what vcov(glm()) returns;
#                        "exact": analytic (X'WX + prior Hessian)^{-1}
#   standardize        — if TRUE, slope columns are divided by their
#                        training SD before fitting and back-transformed
#                        afterwards; useful when predictors are on very
#                        different scales
#   penalize_intercept — if FALSE (default), the intercept is always
#                        excluded from the penalty regardless of m
#   intercept_col      — column index of the intercept in x (default 1)
#   tol, maxit         — convergence tolerance and max iterations for
#                        glm.fit
#
# Returns a list of class "logf_glm" with elements:
#   coefficients, se, vcov, converged, fitted.values, linear.predictors,
#   m, standardize, training_sd, penalised_idx, n, y
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

  # Expand scalar m to a per-coefficient vector of length p.
  if (length(m) == 1L) {
    m <- rep(m, p)
  } else if (length(m) != p) {
    stop(sprintf(
      "m must be length 1 or ncol(x) = %d; got length %d",
      p,
      length(m)
    ))
  }

  # A coefficient is penalised only when m is finite and positive. The
  # intercept is excluded unless penalize_intercept = TRUE.
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
  m_pen <- m[penalised_idx]

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
      s_sl[s_sl == 0 | !is.finite(s_sl)] <- 1 # zero-variance guard
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
  # For each penalised coefficient j, add a pseudo-row: predictor 1 in
  # column j, 0 elsewhere (including the intercept), y* = 0.5, weight = m_j.
  # Setting the intercept column to 0 ensures the pseudo-rows do not
  # perturb the intercept score equation, preserving calibration-in-the-large.
  if (n_pen > 0) {
    X_star <- matrix(0, nrow = n_pen, ncol = p)
    for (k in seq_along(penalised_idx)) {
      X_star[k, penalised_idx[k]] <- 1
    }
    colnames(X_star) <- colnames(x_fit)
    y_star <- rep(0.5, n_pen)
    w_star <- m_pen

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

  beta <- fit$coefficients # on the standardized scale if standardize = TRUE
  names(beta) <- colnames(x_fit)

  # ---- Covariance matrix ----
  # "augmented": (X_aug' W_aug X_aug)^{-1} — same as vcov(glm()) on the
  #              augmented data; fast and consistent with the MAP estimate.
  # "exact":     (X' W X + Lambda)^{-1} — analytic, using the true prior
  #              Hessian at beta_hat. Falls back to "augmented" if singular.
  eta <- as.numeric(x_fit %*% beta)
  pi_hat <- plogis(eta)
  w_data <- weights * pi_hat * (1 - pi_hat)
  XtWX <- crossprod(x_fit * sqrt(w_data))

  # Prior Hessian contribution: -d2/dbeta_j2 log p(beta_j) = (m_j/4) sech^2(beta_j/2).
  # At beta_j = 0 this equals m_j/4, matching the pseudo-row IRLS weight.
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
    w_aug_irls <- w_aug * fit$fitted.values * (1 - fit$fitted.values)
    V <- tryCatch(
      solve(crossprod(X_aug * sqrt(w_aug_irls))),
      error = function(e) {
        stop("logf_glm: augmented information matrix is singular")
      }
    )
  }

  # ---- Back-transform to original (unstandardized) scale ----
  # beta_orig_j = beta_std_j / s_j;  V_orig[j,k] = V_std[j,k] / (s_j * s_k).
  inv_s <- 1 / s
  beta_out <- beta * inv_s
  V_out <- V * tcrossprod(inv_s, inv_s)

  names(beta_out) <- colnames(x)
  dimnames(V_out) <- list(colnames(x), colnames(x))

  se_out <- sqrt(diag(V_out))
  names(se_out) <- colnames(x)

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

# S3 accessors so that coef() and vcov() dispatch correctly, including in
# parallel workers where the class may not yet be registered.
coef.logf_glm <- function(object, ...) object$coefficients
vcov.logf_glm <- function(object, ...) object$vcov

if (getRversion() >= "3.6") {
  .S3method("coef", "logf_glm", coef.logf_glm)
  .S3method("vcov", "logf_glm", vcov.logf_glm)
}


# =============================================================================
# bayesian_ridge — Bayesian ridge logistic regression
#
# Fits a logistic model with an L2 (ridge) penalty on the slopes using
# mgcv::gam() with a parametric penalty block. REML is used to estimate the
# penalty strength (= prior precision) from the data, so this is equivalent
# to placing an isotropic Gaussian prior on the slopes with an
# empirically-estimated variance.
#
# The model is fit internally on standardized predictors (zero-mean, unit-SD)
# so that the isotropic prior is scale-invariant. The returned beta and V are
# back-transformed to the original predictor scale.
#
# Arguments:
#   X             — n x p design matrix (must include intercept column)
#   y             — n-vector of binary outcomes (0/1)
#   unconditional — passed to vcov(fit, unconditional = ...); if TRUE, the
#                   returned covariance accounts for smoothing-parameter
#                   uncertainty (recommended for prediction intervals)
#
# Returns a list with:
#   beta — p-vector of posterior-mean coefficients on the original scale
#   V    — p x p posterior covariance on the original scale
# =============================================================================

bayesian_ridge <- function(X, y, unconditional = TRUE) {
  require(mgcv)

  X <- as.matrix(X)
  p <- ncol(X)

  # Standardize slopes; keep track of centers and scales for back-transform.
  Xs <- scale(X[, -1, drop = FALSE])
  mus <- c(0, attr(Xs, "scaled:center"))
  sds <- c(1, attr(Xs, "scaled:scale"))

  coef_names <- colnames(X)[-1]
  form <- as.formula(
    paste("y ~ 1 +", paste(coef_names, collapse = " + "))
  )

  p <- ncol(Xs)
  S <- diag(p) # identity penalty matrix = ridge

  # paraPen name must match the matrix name in the formula (here "Xs").
  pen_list <- list("Xs" = list(S))
  dat <- data.frame(y = y)
  dat$Xs <- Xs

  # gam() with paraPen treats Xs as a parametric block with a REML-estimated
  # ridge penalty, producing a Laplace-approximated Gaussian posterior.
  fit <- gam(
    y ~ Xs,
    data = dat,
    family = "binomial",
    method = "REML",
    paraPen = pen_list
  )

  beta_z <- fit$coefficients
  V_z <- vcov(fit, unconditional = unconditional)

  # Back-transform: beta_orig = D * beta_std, V_orig = D * V_std * D'.
  # D encodes both the scaling and the mean-shift into the intercept.
  D <- diag(1 / sds)
  D[1, -1] <- -mus[-1] / sds[-1]

  beta <- as.numeric(D %*% beta_z)
  V <- D %*% V_z %*% t(D)

  list(beta = beta, V = V)
}


# =============================================================================
# project_constrained_kl — KL-optimal logistic projection with a prevalence
#                          equality constraint
#
# Finds the logistic coefficient vector beta* that minimises the KL divergence
# KL(p_ppm || q(beta)) subject to sum(q(beta)) = target_sum, where
# q_i(beta) = plogis(x_i' beta).
#
# Use case: after computing PPM risks (p_ppm) on the training set, re-fit a
# logistic model that (a) is as close as possible to p_ppm in KL sense and
# (b) exactly reproduces the observed event count (CITL constraint).  This
# "constrained self-projection" preserves calibration-in-the-large by design.
#
# Arguments:
#   p_ppm       — length-n vector of pre-computed PPM predicted risks
#   X           — n x p design matrix (intercept column included)
#   target_sum  — target sum of predictions; typically sum(y) to preserve
#                 the observed event count
#   beta_start  — optional starting values; defaults to an OLS fit on the
#                 logit-transformed p_ppm
#
# Returns: p-vector of projected logistic coefficients
# =============================================================================

project_constrained_kl <- function(p_ppm, X, target_sum, beta_start = NULL) {
  if (!requireNamespace("nloptr", quietly = TRUE)) {
    stop("Please install 'nloptr'")
  }

  X <- as.matrix(X)
  n <- length(p_ppm)

  # Objective: negative cross-entropy (= KL up to a constant in p_ppm).
  eval_f <- function(params) {
    eta_q <- as.numeric(X %*% params)
    q <- plogis(eta_q)
    eps <- 1e-12
    val <- -sum(p_ppm * log(q + eps) + (1 - p_ppm) * log(1 - q + eps))
    grad <- colSums((q - p_ppm) * X) # d(KL)/d(beta)
    return(list("objective" = val, "gradient" = grad))
  }

  # Equality constraint: sum(q) = target_sum (calibration-in-the-large).
  eval_g_eq <- function(params) {
    q <- plogis(as.numeric(X %*% params))
    val <- sum(q) - target_sum
    jac <- colSums(q * (1 - q) * X) # d(sum_q)/d(beta)
    return(list("constraints" = val, "jacobian" = jac))
  }

  # Warm start: OLS on logit(p_ppm) gives a good initial beta.
  if (is.null(beta_start)) {
    p_adj <- (p_ppm * (n - 1) + 0.5) / n # Laplace-smoothed to avoid logit(0/1)
    beta_start <- coef(lm(qlogis(p_adj) ~ X - 1))
  }

  # Sequential Least-Squares Programming — handles equality constraints and
  # provides gradient information for efficiency.
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
