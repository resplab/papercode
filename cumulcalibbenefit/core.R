#By Mohsen Sadatsafavi
#Date (last modified): February 11, 2026

logit <- function(x) {
  log(x / (1 - x))
}
expit <- function(x) {
  exp(x) / (1 + exp(x))
}


ate <- function(y, a) {
  A <- sum((1 - a) * y)
  B <- sum(1 - a)
  C <- sum(a * y)
  D <- sum(a)
  ate <- A / B - C / D
  v <- A / B * (1 - A / B) / B + C / D * (1 - C / D) / D

  c(ate = ate, var = v)
}

gen_data <- function(n, parms, r = 0.5) {
  x <- rnorm(n, 0, 1) #rep(seq(-2,2,length.out=n/2),each=2) #
  t <- rbinom(n, 1, r) #rep(c(0,1),n/2) #

  p0 <- 1 / (1 + exp(-(parms[1] + parms[2] * x)))
  p1 <- 1 / (1 + exp(-(parms[1] + parms[2] * x + parms[3] + parms[4] * x)))
  Y0 <- rbinom(length(p0), 1, p0)
  Y1 <- rbinom(length(p1), 1, p1)
  Y <- t * Y1 + (1 - t) * Y0
  h = p0 - p1
  df <- data.frame(h = h, a = t, Y = Y, p0 = p0, p1 = p1, x = x)
  df <- df[order(df$h), ]
  df
}


mess <- function(df, type = "", ex_parms = list()) {
  if (type == "") {} else if (type == "HalfRisk") {
    df$p0 <- df$p0 / 2
    df$h <- df$h / 2
  } else if (type == "HalfBenefit") {
    df$h <- df$h / 2
  } else if (type == "HalfRiskPreserveBenefit") {
    df$p0 <- df$p0 / 2
  } else if (type == "Overfit") {
    df$h <- sin(asin(df$h - mean(df$h)) * 2) + mean(df$h)
  } else {
    stop("Messy mess!")
  }
  df
}


test <- function(df, method = 3, test = "2p", use_pi = TRUE, aux = TRUE) {
  df <- df[order(df$h), ]
  n <- nrow(df)
  k <- 1:n
  k1 <- cumsum(df$t)
  k0 <- k - k1
  k0[which(k0 == 0)] <- 1
  k1[which(k1 == 0)] <- 1
  Y1 <- cumsum(df$Y)
  Y01 <- cumsum((1 - df$t) * df$Y)
  Y11 <- cumsum(df$t * df$Y)

  if (method == 1) {
    C_k <- k * (Y01 / k0 - Y11 / k1)

    X_k <- (1 - df$t) *
      ((k * df$Y * k0 - k * Y01 + k0 * Y01 + k0) / (k0^2 + k0) - Y11 / k1) -
      df$t *
        ((k * df$Y * k1 - k * Y11 + k1 * Y11 + k1) / (k1^2 + k1) - Y01 / k0)

    if (use_pi) {
      c_k <- k *
        (cumsum((1 - df$t) * df$p0) / k0 - cumsum(df$t * (df$p0 - df$h)) / k1)
      #Sigma2_k <- k^2*((1-df$t)*df$p0*(1-df$p0)/(c(0,k0)[-(n+1)]+1)^2 + df$t*(df$p0-df$h)*(1-df$p0+df$h)/(c(0,k1)[-(n+1)]+1)^2)
      Sigma2_k <- k^2 *
        ((1 - df$t) *
          df$p0 *
          (1 - df$p0) /
          k0^2 +
          df$t * (df$p0 - df$h) * (1 - df$p0 + df$h) / k1^2)
      v_k <- cumsum(Sigma2_k)
    } else {
      #browser()
      c_k <- cumsum(df$h) #cumsum(df$h) #k/k1*cumsum(df$t*df$h) #
      v_k <- k^2 *
        (Y01 / k0 * (1 - Y01 / k0) / k0 + Y11 / k1 * (1 - Y11 / k1) / k1)
    }

    T <- v_k[n]
    t <- v_k / T #cH
    S <- (C_k - c_k) / sqrt(T)

    if (test == "1p") {
      q <- max(abs(S))
      p <- 1 - cumulcalib::pMAD_BM(q)
    }
    if (test == "2p") {
      pA <- 2 * pnorm(-abs(S[n]))
      qB <- max(abs(S - t * S[n]))
      pB <- 1 - cumulcalib::pKolmogorov(qB)
      p <- 1 - pchisq(-2 * (log(pA) + log(pB)), 4)
    }
  }

  if (method == 2) {
    C_k <- k * (Y01 / k0 - Y11 / k1)

    if (use_pi) {
      X_k <- c(0, C_k[-1] - C_k[-n])
      x_k = ifelse(
        df$t,
        c(0, Y01[-n]) /
          c(0, k0[-n]) -
          k * (c(0, Y11[-n]) + (df$p0 - df$h)) / k1 +
          (k - 1) * c(0, Y11[-n]) / (c(0, k1[-n])),
        (1 - df$t) *
          (k *
            (c(0, Y01[-n]) + df$p0) /
            k0 -
            (k - 1) * c(0, Y01[-n]) / c(0, k0[-n]) -
            c(0, Y11[-n]) / c(0, k1[-n]))
      )
      x_k[which(is.nan(x_k))] <- 0
      c_k <- cumsum(x_k)
      Sigma2_k <- k^2 *
        ((1 - df$t) *
          df$p0 *
          (1 - df$p0) /
          k0^2 +
          df$t * (df$p0 - df$h) * (1 - df$p0 + df$h) / k1^2)
      v_k <- cumsum(Sigma2_k)
    } else {
      #browser()
      c_k <- cumsum(df$h) #cumsum(df$h) #k/k1*cumsum(df$t*df$h) #
      v_k <- k^2 *
        (Y01 / k0 * (1 - Y01 / k0) / k0 + Y11 / k1 * (1 - Y11 / k1) / k1)
    }

    T <- v_k[n]
    t <- v_k / T #cH
    S <- (C_k - c_k) / sqrt(T)

    if (test == "1p") {
      q <- max(abs(S))
      p <- 1 - cumulcalib::pMAD_BM(q)
    }
    if (test == "2p") {
      pA <- 2 * pnorm(-abs(S[n]))
      qB <- max(abs(S - t * S[n]))
      pB <- 1 - cumulcalib::pKolmogorov(qB)
      p <- 1 - pchisq(-2 * (log(pA) + log(pB)), 4)
    }
  }

  if (method == 3) {
    C <- k * (Y01 / k0 - Y11 / k1)

    if (use_pi) {
      X <- c(0, C[-1] - C[-n])
      # X2=ifelse(df$t,
      #           c(0,Y01[-n])/c(0,k0[-n])-k*(c(0,Y11[-n])+(df$Y))/k1+(k-1)*c(0,Y11[-n])/(c(0,k1[-n])),
      #           (1-df$t)*(k*(c(0,Y01[-n])+df$Y)/k0-(k-1)*c(0,Y01[-n])/c(0,k0[-n])-c(0,Y11[-n])/c(0,k1[-n])))
      # X2[which(is.nan(X2))]<-0
      mu = ifelse(
        df$t,
        c(0, Y01[-n]) /
          c(0, k0[-n]) -
          k * (c(0, Y11[-n]) + (df$p0 - df$h)) / k1 +
          (k - 1) * c(0, Y11[-n]) / (c(0, k1[-n])),
        (1 - df$t) *
          (k *
            (c(0, Y01[-n]) + df$p0) /
            k0 -
            (k - 1) * c(0, Y01[-n]) / c(0, k0[-n]) -
            c(0, Y11[-n]) / c(0, k1[-n]))
      )
      mu[which(is.nan(mu))] <- 0
      X_mu <- k *
        ((1 - df$t) * (df$Y - df$p0) / k0 - df$t * (df$Y - df$p0 + df$h) / k1)
      sigma2 <- k^2 *
        ((1 - df$t) *
          df$p0 *
          (1 - df$p0) /
          k0^2 +
          df$t * (df$p0 - df$h) * (1 - df$p0 + df$h) / k1^2)
      s2 <- cumsum(sigma2)
      s2_n <- s2[n]
      S <- (cumsum(X_mu)) / sqrt(s2_n)
    } else {
      mu <- df$h
      s2 <- k^2 *
        (Y01 / k0 * (1 - Y01 / k0) / k0 + Y11 / k1 * (1 - Y11 / k1) / k1)
      s2_n <- s2[n]
      S <- (C - cumsum(mu)) / sqrt(s2_n)
    }

    t <- s2 / s2_n #cH
    #S <- (C-cumsum(mu))/sqrt(s2_n)

    if (test == "1p") {
      q <- max(abs(S))
      p <- 1 - cumulcalib::pMAD_BM(q)
    }
    if (test == "2p") {
      pA <- 2 * pnorm(-abs(S[n]))
      qB <- max(abs(S - t * S[n]))
      pB <- 1 - cumulcalib::pKolmogorov(qB)
      p <- 1 - pchisq(-2 * (log(pA) + log(pB)), 4)
    }
  }

  out <- list(S = cbind(t, S), p = p)
  if (aux) {
    out$aux <- as.list(environment())
  }

  out
}
