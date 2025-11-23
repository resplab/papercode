library(mvtnorm)
library(glue)
library(ggplot2)
library(tidyverse)
library(mcmapper)
library(RColorBrewer)
library(nleqslv)

#This is defualt. The Res gen file overrides with its own settings
settings <- list(
  PROGNOSTIC = 1,
  TYPE = 1,
  p_src = c(0.25, 0.75, 0.50),
  p_des = c(1 / 3, 0.80, 2 / 3),
  #ct_src = c(0.3, 0.3, 0.08, 0.32),
  #ct_des = c(0.4800000, 0.3200000, 0.0600000, 0.1400000),
  #p_src = c(1/3, 0.5, 2/3),
  #p_src = c(0.5, 0.5, 0.5),
  prev_ratio = 1.25,
  p_src_vars = c(0.01, 0.01, 0.01), #Variance of logitnorm dists for simulations
  n_sim = 10000
)

# Define a color palettesim
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


#Given an existing p and a new prevalence, generates new p under different scenarions. "Naive" is the logistic intercept updating
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

  if (assumption == "ppvnpv") {
    metrics <- calc_metrics(calc_m(p))

    p <- (target_prev - 1 + metrics['npv']) /
      (metrics['ppv'] - 1 + metrics['npv'])

    A <- p * metrics['ppv']
    B <- p * (1 - metrics['ppv'])
    C <- (1 - p) * (1 - metrics['npv'])
    D <- 1 - A - B - C
    return(p_from_ct(c(A, B, C, D)))
  }

  #por solves using cubic; prop_or solves using root finding
  if (assumption == "por") {
    f <- function(x,t,u,v,a)
    {
      c3 <- t*u*v * (1-a)
      c2 <- (3*a-2)*t*u*v - a*(t*u + t*v + u*v) + t*u + u*v
      c1 <- -a*(3*t*u*v - 2*t*u - 2*t*v - 2*u*v + t + u + v)
      c0 <- -a*(1-t)*(1-u)*(1-v)
      
      c3*x^3+c2*x^2+c1*x+c0
    }
    if(sign(f(0, p[1], p[2], p[3], target_prev))==sign(f(1000, p[1], p[2], p[3], target_prev)))
    {
      warning("No odds-ratio between 0 and 1000.")
    }
    x <- uniroot(f, c(0, max(100000,2*target_prev/(min(p)))), p[1], p[2], p[3], target_prev)$root
    #message("Odds-ratio is ", x)
    T_ <- x * p[1] / (1 - p[1]+ x * p[1])
    U_ <- x * p[2] / (1 - p[2] + x * p[2])
    V_ <- x * p[3] / (1 - p[3] + x * p[3])
    return(c(T_, U_, V_))
  }

  if (assumption == "prop_or") {
    apply_or <- function(p, z) {
      ifelse(p == p^2, p, {
        s1 <- p / (1 - p) * z
        s1 / (1 + s1)
      })
    }

    if (settings$PROGNOSTIC == 1) {
      f = function(x) {
        if (assumption == "T") {
          upda
          p2 <- apply_or(p, c(x, 1, 1))
        }
        if (assumption == "U") {
          p2 <- apply_or(p, c(1, x, 1))
        }
        if (assumption == "V") {
          p2 <- apply_or(p, c(1, 1, x))
        }
        if (assumption == "prop_or") {
          p2 <- apply_or(p, c(x, x, x))
        }
        if (settings$TYPE == 1) {
          prev <- p2[1] * p2[2] + p2[2] * p2[3] - p2[1] * p2[2] * p2[3]
        } else {
          prev <- p2[1] * p2[3] + p2[2] - p2[1] * p2[2] * p2[3]
        }
        prev - target_prev
      }

      res <- uniroot(f, interval = c(0.0001, 20000))
      or <- res$root
      #message(paste("OR was",or))

      if (assumption == "T") {
        or <- c(or, 1, 1)
      }
      if (assumption == "U") {
        or <- c(1, or, 1)
      }
      if (assumption == "V") {
        or <- c(1, 1, or)
      }
      if (assumption == "prop_or") {
        or <- c(or, or, or)
      }
      return(apply_or(p, or))
    } else {
      or <- target_prev / (1 - target_prev) / (p[1] / (1 - p[1]))

      return(apply_or(p, or))
    }
  }
}


# line_data <- list(
#   "N" = 11,
#   "ncontrols" = c(167, 48, 12, 58, 24, 75, 453, 38, 195, 136, 20),
#   "tn" = c(155, 46, 12, 56, 24, 74, 445, 38, 193, 136, 20),
#   "ncases" = c(203, 18, 9, 42, 15, 103, 425, 27, 109, 87, 59),
#   "tp" = c(150, 9, 8, 30, 10, 53, 282, 7, 53, 48, 57)
# )
#
#
# ABCD_data <- data.frame(
#   A = line_data$tp,
#   B = line_data$ncontrols - line_data$tn,
#   C = line_data$ncases - line_data$tp,
#   D = line_data$tn
# )
# ABCD_data <- ABCD_data / rowSums(ABCD_data)
#
# tbl <- cbind(
#   A = line_data$tp,
#   B = line_data$ncontrols - line_data$tn,
#   C = line_data$ncases - line_data$tp,
#   D = line_data$tn
# )

chisq_val <- function(ABCD, abcd) {
  goods <- which(abcd > 0)
  sum(((abcd[goods] - ABCD[goods])^2) / abcd[goods])
}


data_generator <- function(n = 100) {
  bx <- by <- bz <- rep(NA, n)
  Px <- Py <- Pz <- rep(NA, n)
  N <- rep(NA, n)
  P000 <- P001 <- P010 <- P011 <- P100 <- P101 <- P110 <- P111 <- rep(NA, n)
  P <- X <- matrix(NA, nrow = n, ncol = 4)

  mux <- 0
  vx <- 1

  muy <- 0.5
  vy <- 0.001

  muz <- 1
  vz <- 0.001

  for (i in 1:n) {
    bx[i] <- rnorm(1, mux, sqrt(vx))
    by[i] <- rnorm(1, muy, sqrt(vy))
    bz[i] <- rnorm(1, muz, sqrt(vz))

    Px[i] <- 1 / (1 + exp(-bx[i]))
    Py[i] <- 1 / (1 + exp(-by[i]))
    Pz[i] <- 1 / (1 + exp(-bz[i]))

    P000[i] <- (1 - Px[i]) * (1 - Py[i]) * (1 - Pz[i])
    P001[i] <- (1 - Px[i]) * (1 - Py[i]) * Pz[i]
    P010[i] <- (1 - Px[i]) * Py[i] * (1 - Pz[i])
    P011[i] <- (1 - Px[i]) * Py[i] * Pz[i]
    P100[i] <- Px[i] * (1 - Py[i]) * (1 - Pz[i])
    P101[i] <- Px[i] * (1 - Py[i]) * Pz[i]
    P110[i] <- Px[i] * Py[i] * (1 - Pz[i])
    P111[i] <- Px[i] * Py[i] * Pz[i]

    P[i, 1] <- P101[i] + P110[i] + P111[i]
    P[i, 2] <- P100[i]
    P[i, 3] <- P001[i] + P011[i]
    P[i, 4] <- P000[i] + P010[i]

    X[i, ] <- rmultinom(1, 100, P[i, ])
    N[i] <- 100
  }

  colnames(X) <- c('A', 'B', 'C', 'D')
  X
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
      plot.title = element_text(hjust = 0.5, size = 32),
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

#
KL_bernouli <- function(p, q) {
  p * log2(p) / log2(q) + (1 - p) * log2(1 - p) / log2(1 - q)
}

KL_ABCD <- function(abcd, ABCD) {
  sum(abcd * log2(abcd / ABCD))
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
    "TUV",
    "indl",
    "wdep",
    "mdep",
    "sdep",
    "fdep",
    "max_entropy",
    "ind_unif"
  )
) {
  # parms1 <- unlist(logitnorm_params(
  #   0,
  #   settings$p_src_vars[1]
  # ))[1:2]
  # parms2 <- unlist(logitnorm_params(
  #   0,
  #   settings$p_src_vars[2]
  # ))[1:2]
  # parms3 <- unlist(logitnorm_params(
  #   0,
  #   settings$p_src_vars[3]
  # ))[1:2]

  mus <- c(0, 0, 0)
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

  SS <- BS <- KL <- c(sesp = 0, ppvnpv = 0, prop_or = 0)
  N <- 0

  for (i in 1:n_sim) {
    # cat(i, "|")
    #ABCDs <- metrics1[i, c('A', 'B', 'C', 'D')]
    ABCDt <- metrics2[i, c('A', 'B', 'C', 'D')]
    PD <- ABCDt[1] + ABCDt[3]
    PT <- ABCDt[1] + ABCDt[2]

    #sesp
    A <- PD * metrics1[i, 'se']
    B <- (1 - PD) * (1 - metrics1[i, 'sp'])
    C <- PD - A
    D <- (1 - PD) - B
    ABCDsesp <- c(A, B, C, D)
    SS['sesp'] <- SS['sesp'] + sum((ABCDsesp - ABCDt)^2)
    xsesp <- PD *
      (C / (A + C))^2 +
      (1 - PD) * (B / (B + D))^2
    BS['sesp'] <- BS['sesp'] + xsesp
    KL['sesp'] <- KL['sesp'] + KL_ABCD(ABCDt, ABCDsesp)

    #ppvnpv (ie as is)
    A <- PT * metrics1[i, 'ppv']
    B <- PT - A
    C <- (1 - PT) * (1 - metrics1[i, 'npv'])
    D <- (1 - PT) - C
    ABCDppvnpv <- c(A, B, C, D)
    SS['ppvnpv'] <- SS['ppvnpv'] + sum((ABCDppvnpv - ABCDt)^2)
    xppvnpv <- PD *
      (C / (A + C))^2 +
      (1 - PD) * (B / (B + D))^2
    BS['ppvnpv'] <- BS['ppvnpv'] + xppvnpv
    KL['ppvnpv'] <- KL['ppvnpv'] + KL_ABCD(ABCDt, ABCDppvnpv)

    #prop_or
    q <- update_p(
      TUVs[i, ],
      target_prev = ABCDt[1] + ABCDt[3],
      assumption = "por"
    )
    ABCDprop_or <- calc_metrics(calc_m(q))[c('A', 'B', 'C', 'D')]
    SS['prop_or'] <- SS['prop_or'] + sum((ABCDprop_or - ABCDt)^2)
    xprop_or <- PD *
      (ABCDprop_or[3] / (ABCDprop_or[1] + ABCDprop_or[3]))^2 +
      (1 - PD) * (ABCDprop_or[3] / (ABCDprop_or[2] + ABCDprop_or[4]))^2
    BS['prop_or'] <- BS['prop_or'] + xprop_or
    KL['prop_or'] <- KL['prop_or'] + KL_ABCD(ABCDt, ABCDprop_or)

    N <- N + 1
  }
  c(
    SS = SS / N,
    BS = BS / N,
    KL = KL / N,
    cor.se = cor(metrics1[, 'prev'], metrics1[, 'se']),
    cor.sp = cor(metrics1[, 'prev'], metrics1[, 'sp']),
    cor.ppv = cor(metrics1[, 'prev'], metrics1[, 'ppv']),
    cor.npv = cor(metrics1[, 'prev'], metrics1[, 'npv'])
  )
}
