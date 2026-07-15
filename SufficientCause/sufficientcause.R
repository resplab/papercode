# =============================================================================
# sufficientcause.R
# =============================================================================

library(mvtnorm)
library(glue)
library(ggplot2)
library(tidyverse)
library(mcmapper)
library(RColorBrewer)
library(nleqslv)

# These are defaults. The ResGen file overrides them with its own settings.
settings <- list(
  PROGNOSTIC = 1,
  TYPE = 1,
  p_src = c(0.25, 0.30, 0.15), # previous value: c(0.25, 0.75, 0.50)
  p_des = c(0.30, 0.40, 0.20), # previous value: c(1 / 3, 0.80, 2 / 3)
  prev_ratio = 1.25,
  p_src_vars = c(0.01, 0.01, 0.01), # Variance of logitnorm dists for simulations
  n_sim = 10000
)

# Define a color palette
colors <- brewer.pal(4, "Set2")

logit <- function(x) {
  log(x / (1 - x))
}
expit <- function(x) {
  1 / (1 + exp(-x))
}


# Function to compute moments of logit-normal given mu, sigma
logitnorm_moments <- function(mu, sigma) {
  f <- function(x) plogis(x) * dnorm(x, mean = mu, sd = sigma)
  g <- function(x) (plogis(x)^2) * dnorm(x, mean = mu, sd = sigma)

  mean_val <- integrate(f, -Inf, Inf)$value
  second_moment <- integrate(g, -Inf, Inf)$value
  var_val <- second_moment - mean_val^2

  return(c(mean = mean_val, var = var_val))
}

# Solver function
logitnorm_params <- function(
  target_mean,
  target_var,
  mu_init = 0,
  sigma_init = 1
) {
  # System of equations: match mean and variance
  equations <- function(par) {
    mu <- par[1]
    sigma <- exp(par[2]) # enforce sigma > 0
    mom <- logitnorm_moments(mu, sigma)
    return(c(mom["mean"] - target_mean, mom["var"] - target_var))
  }

  sol <- nleqslv(c(mu_init, log(sigma_init)), equations, method = "Broyden")
  mu_hat <- sol$x[1]
  sigma_hat <- exp(sol$x[2])

  list(mu = mu_hat, sigma = sigma_hat, conv = sol$termcd)
}


cov_from_vars_rho <- function(vars, rho) {
  p <- length(vars)
  if (any(vars < 0)) {
    stop("Variances must be nonnegative.")
  }
  if (rho <= -1 / (p - 1) || rho >= 1) {
    stop("rho must be in (-1/(p-1), 1) for PD.")
  }
  sds <- sqrt(vars)
  R <- matrix(rho, p, p)
  diag(R) <- 1
  Sigma <- diag(sds) %*% R %*% diag(sds)
  dimnames(Sigma) <- list(names(vars), names(vars))
  Sigma
}


#Contingency table to probability
p_from_ct <- function(ct = c(A = 0.25, B = 0.25, C = 0.25, D = 0.25)) {
  if (settings$PROGNOSTIC == 1) {
    T <- ct[1] + ct[2]
    U <- ct[1] / T
    V <- ct[3] / ((1 - T) * U)
  } else {
    T <- ct[1] + ct[3] #Actually D
    U <- ct[1] / T
    V <- ct[2] / ((1 - T) * U)
  }
  unname(unlist(c(T, U, V)))
}


calc_m <- function(p) {
  M <- FALSE
  if (is.vector(p)) {
    p <- t(p)
    M <- TRUE
  }
  val <- cbind(
    m000 = unname((1 - p[, 1]) * (1 - p[, 2]) * (1 - p[, 3])),
    m001 = unname((1 - p[, 1]) * (1 - p[, 2]) * p[, 3]),
    m010 = unname((1 - p[, 1]) * p[, 2] * (1 - p[, 3])),
    m011 = unname((1 - p[, 1]) * p[, 2] * p[, 3]),
    m100 = unname(p[, 1] * (1 - p[, 2]) * (1 - p[, 3])),
    m101 = unname(p[, 1] * (1 - p[, 2]) * p[, 3]),
    m110 = unname(p[, 1] * p[, 2] * (1 - p[, 3])),
    m111 = unname(p[, 1] * p[, 2] * p[, 3])
  )

  if (M) {
    val[1, ]
  } else {
    val
  }
}


calc_metrics <- function(m) {
  M <- FALSE
  if (is.vector(m)) {
    m <- t(m)
    M <- TRUE
  }

  if (settings$PROGNOSTIC) {
    if (settings$TYPE == 1) {
      A <- m[, 'm110'] + m[, 'm111']
      B <- m[, 'm100'] + m[, 'm101']
      C <- m[, 'm011']
      D <- m[, 'm000'] + m[, 'm001'] + m[, 'm010']
    } else if (settings$TYPE == 2) {
      A <- m[, 'm101'] + m[, 'm110'] + m[, 'm111']
      B <- m[, 'm100']
      C <- m[, 'm010'] + m[, 'm011']
      D <- m[, 'm000'] + m[, 'm001']
    } else {
      stop("You should not be here!")
    }
  } else {
    if (settings$TYPE == 1) {
      A <- m[, 'm110'] + m[, 'm111']
      B <- m[, 'm011']
      C <- m[, 'm100'] + m[, 'm101']
      D <- m[, 'm000'] + m[, 'm001'] + m[, 'm010']
    } else {
      #TODO
    }
  }

  prev <- A + C
  px <- A + B

  ppv <- A / (A + B)
  npv <- D / (C + D)

  se <- A / (A + C)
  sp <- D / (B + D)

  lrp <- se / (1 - sp)
  lrn <- (1 - se) / sp
  rr <- A / (A + B) / C * (C + D)
  or <- A * D / (B * C)

  alpha0 <- px
  beta0 <- (m[, 'm010'] + m[, 'm011']) /
    (m[, 'm000'] + m[, 'm001'] + m[, 'm010'] + m[, 'm011'])
  betaT <- (m[, 'm110'] + m[, 'm111']) /
    (m[, 'm100'] + m[, 'm101'] + m[, 'm110'] + m[, 'm111']) -
    beta0
  delta0 <- m[, 'm001'] / (m[, 'm000'] + m[, 'm001'])
  deltaT <- m[, 'm101'] / (m[, 'm100'] + m[, 'm101']) - delta0
  deltaU <- m[, 'm011'] / (m[, 'm010'] + m[, 'm011']) - delta0
  deltaTU <- m[, 'm111'] /
    (m[, 'm110'] + m[, 'm111']) -
    delta0 -
    deltaT -
    deltaU

  val <- cbind(
    prev = unname(prev),
    px = unname(px),
    A = unname(A),
    B = unname(B),
    C = unname(C),
    D = unname(D),
    ppv = unname(ppv),
    npv = unname(npv),
    se = unname(se),
    sp = unname(sp),
    lrp = unname(lrp),
    lrn = unname(lrn),
    rr = unname(rr),
    or = unname(or),
    alpha0 = unname(alpha0),
    beta0 = unname(beta0),
    betaT = unname(betaT),
    delta0 = unname(delta0),
    deltaT = unname(deltaT),
    deltaU = unname(deltaU),
    deltaTU = unname(deltaTU)
  )

  if (M) {
    val[1, ]
  } else {
    val
  }
}


# Given an existing p and a new prevalence, generates new p under different
# transportability assumptions.
#   "sesp"    : transportation by accuracy (SE/SP preserved, uses target prevalence)
#   "por"     : proportional-odds via the cubic in eAppendix 1
#   "prop_or" : proportional-odds via direct root finding (equivalent to "por")
update_p <- function(
  p,
  target_prev,
  assumption
) {
  if (assumption == "sesp") {
    metrics <- calc_metrics(calc_m(p))
    A <- target_prev * metrics['se']
    B <- (1 - target_prev) * (1 - metrics['sp'])
    C <- target_prev - A
    D <- (1 - target_prev) - B
    return(p_from_ct(c(A, B, C, D)))
  }

  # por: proportional-odds solved via the cubic (eAppendix 1)
  if (assumption == "por") {
    f <- function(x, t, u, v, a) {
      c3 <- t * u * v * (1 - a)
      c2 <- (3 * a - 2) *
        t *
        u *
        v -
        a * (t * u + t * v + u * v) +
        t * u +
        u * v
      c1 <- -a * (3 * t * u * v - 2 * t * u - 2 * t * v - 2 * u * v + t + u + v)
      c0 <- -a * (1 - t) * (1 - u) * (1 - v)

      c3 * x^3 + c2 * x^2 + c1 * x + c0
    }
    if (
      sign(f(0, p[1], p[2], p[3], target_prev)) ==
        sign(f(1000, p[1], p[2], p[3], target_prev))
    ) {
      warning("No odds-ratio between 0 and 1000.")
    }
    x <- uniroot(
      f,
      c(0, max(100000, 2 * target_prev / (min(p)))),
      p[1],
      p[2],
      p[3],
      target_prev
    )$root
    #message("Odds-ratio is ", x)
    T_ <- x * p[1] / (1 - p[1] + x * p[1])
    U_ <- x * p[2] / (1 - p[2] + x * p[2])
    V_ <- x * p[3] / (1 - p[3] + x * p[3])
    return(c(T_, U_, V_))
  }

  # prop_or: proportional-odds solved by direct root finding (equivalent to por)
  if (assumption == "prop_or") {
    apply_or <- function(p, z) {
      ifelse(p == p^2, p, {
        s1 <- p / (1 - p) * z
        s1 / (1 + s1)
      })
    }

    if (settings$PROGNOSTIC == 1) {
      f <- function(x) {
        p2 <- apply_or(p, c(x, x, x))
        if (settings$TYPE == 1) {
          prev <- p2[1] * p2[2] + p2[2] * p2[3] - p2[1] * p2[2] * p2[3]
        } else {
          prev <- p2[1] * p2[3] + p2[2] - p2[1] * p2[2] * p2[3]
        }
        prev - target_prev
      }

      res <- uniroot(f, interval = c(0.0001, 20000))
      or <- res$root
      #message(paste("OR was", or))
      return(apply_or(p, c(or, or, or)))
    } else {
      or <- target_prev / (1 - target_prev) / (p[1] / (1 - p[1]))
      return(apply_or(p, or))
    }
  }
}


# Kullback-Leibler divergence between two contingency tables (bits).
# abcd = true (target) table; ABCD = candidate (transported) table.
KL_ABCD <- function(abcd, ABCD) {
  sum(abcd * log2(abcd / ABCD))
}


draw_dependence_plot <- function(
  p = settings$p_src,
  assumption = c("T", "U", "V", "TU", "TV", "UV", "TUV"),
  title = ""
) {
  base_metrics <- calc_metrics(calc_m(p))
  odds <- p / (1 - p)
  ors <- c(1 / 4, 1 / 3, 1 / 2, 2 / 3, 3 / 4, 1, 4 / 3, 3 / 2, 2, 3, 4)
  out <- data.frame()

  if (assumption == "T") {
    for (x in ors) {
      p2 <- p
      p2[1] <- (odds[1] * x) / (1 + odds[1] * x)
      Ms <- calc_metrics(calc_m(p2))
      out <- rbind(out, Ms)
    }
    colnames(out) <- names(Ms)
  }
  if (assumption == "U") {
    for (x in ors) {
      p2 <- p
      p2[2] <- (odds[2] * x) / (1 + odds[2] * x)
      Ms <- calc_metrics(calc_m(p2))
      out <- rbind(out, Ms)
    }
    colnames(out) <- names(Ms)
  }
  if (assumption == "V") {
    for (x in ors) {
      p2 <- p
      p2[3] <- (odds[3] * x) / (1 + odds[3] * x)
      Ms <- calc_metrics(calc_m(p2))
      out <- rbind(out, Ms)
    }
    colnames(out) <- names(Ms)
  }
  if (assumption == "TU") {
    for (x in ors) {
      p1 <- p2 <- p
      p2[1] <- (odds[1] * x) / (1 + odds[1] * x)
      p2[2] <- (odds[2] * x) / (1 + odds[2] * x)
      Ms <- calc_metrics(calc_m(p2))
      out <- rbind(out, Ms)
    }
    colnames(out) <- names(Ms)
  }
  if (assumption == "TV") {
    for (x in ors) {
      p1 <- p2 <- p
      p2[1] <- (odds[1] * x) / (1 + odds[1] * x)
      p2[3] <- (odds[3] * x) / (1 + odds[3] * x)
      Ms <- calc_metrics(calc_m(p2))
      out <- rbind(out, Ms)
    }
    colnames(out) <- names(Ms)
  }
  if (assumption == "UV") {
    for (x in ors) {
      p1 <- p2 <- p
      p2[2] <- (odds[2] * x) / (1 + odds[2] * x)
      p2[3] <- (odds[3] * x) / (1 + odds[3] * x)
      Ms <- calc_metrics(calc_m(p2))
      out <- rbind(out, Ms)
    }
    colnames(out) <- names(Ms)
  }
  if (assumption == "TUV") {
    for (x in ors) {
      p2 <- (odds * x) / (1 + odds * x)
      Ms <- calc_metrics(calc_m(p2))
      out <- rbind(out, Ms)
    }
    colnames(out) <- names(Ms)
  }

  data_long <- out %>%
    mutate(Prevalence = out[, 'prev']) %>%
    pivot_longer(
      cols = c(se, sp, ppv, npv), # Reshape to long format
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      variable = factor(
        variable,
        levels = c("ppv", "npv", "se", "sp")
      )
    )

  # Create ggplot
  plt <-
    ggplot(
      data_long,
      aes(
        x = Prevalence,
        y = value,
        group = variable,
        color = variable,
        linetype = variable
      )
    ) +
    geom_line(size = 2) +
    scale_linetype_manual(
      values = c("dotdash", "dashed", "longdash", "twodash")
    ) +
    scale_color_manual(
      values = colors,
      labels = c("PPV", "NPV", "SE", "SP")
    ) +
    labs(x = "Prevalence", y = "Value", title = title) +
    theme_bw() +
    theme(
      legend.position = "none",
      legend.text = element_text(size = 20),
      #plot.title = element_text(hjust = 0.5, size = 32),
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 24),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      legend.title = element_blank()
    ) +
    guides(
      color = guide_legend(
        keywidth = 5,
        override.aes = list(
          size = 2,
          linetype = c("dotdash", "dashed", "longdash", "twodash")
        )
      ),
      linetype = "none"
    )
  plt
}


simulate_updating_scenarios <- function(
  n_sim = settings$n_sim,
  scenario = c(
    "T",
    "U",
    "V",
    "TU",
    "TV",
    "UV",
    "ind",
    "wdep",
    "mdep",
    "sdep",
    "fdep",
    "ind_unif",
    "max_entropy"
  )
) {
  mus <- c(0, 0, 0) #logit(settings$p_src)
  sigmas <- c(1, 1, 1)

  if (scenario == "ind") {
    covmat <- cov_from_vars_rho(sigmas^2, 0)
    TUVs <- expit(rmvnorm(n_sim * 2, mus, covmat))
    m <- calc_m(TUVs)
  }
  if (scenario == "wdep") {
    covmat <- cov_from_vars_rho(sigmas^2, 0.25)
    TUVs <- expit(rmvnorm(n_sim * 2, mus, covmat))
    m <- calc_m(TUVs)
  }
  if (scenario == "mdep") {
    covmat <- cov_from_vars_rho(sigmas^2, 0.5)
    TUVs <- expit(rmvnorm(n_sim * 2, mus, covmat))
    m <- calc_m(TUVs)
  }
  if (scenario == "sdep") {
    covmat <- cov_from_vars_rho(sigmas^2, 0.75)
    TUVs <- expit(rmvnorm(n_sim * 2, mus, covmat))
    m <- calc_m(TUVs)
  }
  if (scenario == "fdep") {
    tmp <- rnorm(n_sim * 2)
    TUVs <- cbind(
      expit(tmp * sigmas[1] + mus[1]),
      expit(tmp * sigmas[2] + mus[2]),
      expit(tmp * sigmas[3] + mus[3])
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "T") {
    TUVs <- cbind(
      rlogitnorm(n_sim * 2, mus[1], sigmas[1]),
      rep(rlogitnorm(n_sim, mus[2], sigmas[2]), 2),
      rep(rlogitnorm(n_sim, mus[3], sigmas[3]), 2)
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "U") {
    TUVs <- cbind(
      rep(rlogitnorm(n_sim, mus[1], sigmas[1]), 2),
      rlogitnorm(n_sim * 2, mus[2], sigmas[2]),
      rep(rlogitnorm(n_sim, mus[3], sigmas[3]), 2)
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "V") {
    TUVs <- cbind(
      rep(rlogitnorm(n_sim, mus[1], sigmas[1]), 2),
      rep(rlogitnorm(n_sim, mus[2], sigmas[2]), 2),
      rlogitnorm(n_sim * 2, mus[3], sigmas[3])
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "TU") {
    tmp <- rnorm(n_sim * 2)
    TUVs <- cbind(
      expit(tmp * sigmas[1] + mus[2]),
      expit(tmp * sigmas[1] + mus[2]),
      rep(rlogitnorm(n_sim, mus[3], sigmas[3]), 2)
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "TV") {
    tmp <- rnorm(n_sim * 2)
    TUVs <- cbind(
      expit(tmp * sigmas[1] + mus[1]),
      rep(rlogitnorm(n_sim, mus[2], sigmas[2]), 2),
      expit(tmp * sigmas[3] + mus[3])
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "UV") {
    tmp <- rnorm(n_sim * 2)
    TUVs <- cbind(
      rep(rlogitnorm(n_sim, mus[1], sigmas[1]), 2),
      expit(tmp * sigmas[2] + mus[2]),
      expit(tmp * sigmas[3] + mus[3])
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "ind_unif") {
    TUVs <- cbind(
      runif(2 * n_sim),
      runif(2 * n_sim),
      runif(2 * n_sim)
    )
    m <- calc_m(TUVs)
  }
  if (scenario == "max_entropy") {
    m <- rbeta(8 * n_sim * 2, 1, 1)
    dim(m) <- c(2 * n_sim, 8)
    m <- m / rowSums(m)
    colnames(m) <- c(
      'm000',
      'm001',
      'm010',
      'm011',
      'm100',
      'm101',
      'm110',
      'm111'
    )
    T <- m[, 'm100'] + m[, 'm101'] + m[, 'm110'] + m[, 'm111']
    U <- m[, 'm010'] + m[, 'm011'] + m[, 'm110'] + m[, 'm111']
    V <- m[, 'm001'] + m[, 'm011'] + m[, 'm101'] + m[, 'm111']
    TUVs <- cbind(T, U, V)
  }

  metrics1 <- calc_metrics(m[1:n_sim, ])
  metrics2 <- calc_metrics(m[(n_sim + 1):(2 * n_sim), ])

  KL <- c(sesp = 0, ppvnpv = 0, prop_or = 0)
  N <- 0

  for (i in 1:n_sim) {
    ABCDt <- metrics2[i, c('A', 'B', 'C', 'D')]
    PD <- ABCDt[1] + ABCDt[3] # target outcome prevalence
    PT <- ABCDt[1] + ABCDt[2] # target marker positivity

    # By accuracy: preserve source SE/SP, use target prevalence (Bayes)
    A <- PD * metrics1[i, 'se']
    B <- (1 - PD) * (1 - metrics1[i, 'sp'])
    C <- PD - A
    D <- (1 - PD) - B
    ABCDsesp <- c(A, B, C, D)
    KL['sesp'] <- KL['sesp'] + KL_ABCD(ABCDt, ABCDsesp)

    # By predictive values (as is): preserve source PPV/NPV, use target marker positivity
    A <- PT * metrics1[i, 'ppv']
    B <- PT - A
    C <- (1 - PT) * (1 - metrics1[i, 'npv'])
    D <- (1 - PT) - C
    ABCDppvnpv <- c(A, B, C, D)
    KL['ppvnpv'] <- KL['ppvnpv'] + KL_ABCD(ABCDt, ABCDppvnpv)

    # Proportional-odds
    q <- update_p(
      TUVs[i, ],
      target_prev = PD,
      assumption = "por"
    )
    ABCDprop_or <- calc_metrics(calc_m(q))[c('A', 'B', 'C', 'D')]
    KL['prop_or'] <- KL['prop_or'] + KL_ABCD(ABCDt, ABCDprop_or)

    N <- N + 1
  }
  c(
    KL = KL / N,
    cor.se = cor(metrics1[, 'prev'], metrics1[, 'se']),
    cor.sp = cor(metrics1[, 'prev'], metrics1[, 'sp']),
    cor.ppv = cor(metrics1[, 'prev'], metrics1[, 'ppv']),
    cor.npv = cor(metrics1[, 'prev'], metrics1[, 'npv'])
  )
}
