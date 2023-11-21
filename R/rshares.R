rdir_maxent <- function(N, shares, ...) {
  out <- find_gamma_maxent2(shares, eval_f = eval_f, ...)
  sample <- gtools::rdirichlet(N, shares * out$solution)
  #attr(sample, 'nloptr') <- out
  return(sample)
}

rdir1 <- function(N, length) {
  rdirichlet(N, rep(1, length))
}

rgdir <- function(n, mu, u) {
  alpha <- (mu / u) ^ 2
  beta <- mu / (u) ^ 2
  k <- length(alpha)
  x <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
    x[, i] <- rgamma(n, shape = alpha[i], rate = beta[i])
  }
  return(x / rowSums(x))
}


rshares <- function(N, alpha, beta = NULL) {
  if (isTRUE(all.equal(var(alpha), 0)) & is.null(beta)) {
    # Dirichlet 1
    rdir1(N, length = length(alpha))
  } else if (is.null(beta)) {
    # Dirichlet MaxEnt
    rdir_maxent(N, alpha)
  } else if (!is.null(beta)) {
    # Gen. Dirichlet
    rgdir(N, alpha, beta)
  } else {
    stop('Case not implemented atm.')
  }
}


# rshares(100, c(3,3,3)) %>% boxplot
# rshares(100, c(0.1, 0.3, 0.6)) %>% boxplot
# rshares(100, c(0.1, 0.3, 0.6), c(0.1, 0.01, 0.2)) %>% boxplot





