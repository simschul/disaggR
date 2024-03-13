rdir_maxent <- function(N, shares, ...) {
  out <- find_gamma_maxent2(shares, eval_f = eval_f, ...)
  #sample <- gtools::rdirichlet(N, shares * out$solution)
  sample <- rdir2(N, alpha = shares, gamma = out$solution)
  #attr(sample, 'nloptr') <- out
  return(sample)
}

rdir1 <- function(N, length) {
  sample <- rdirichlet(N, rep(1, length))
  return(sample)
}

# rgdir <- function(n, mu, u) {
#   alpha <- (mu / u) ^ 2
#   beta <- mu / (u) ^ 2
#   k <- length(alpha)
#   x <- matrix(0, nrow = n, ncol = k)
#   for (i in 1:k) {
#     x[, i] <- rgamma(n, shape = alpha[i], rate = beta[i])
#   }
#   return(x / rowSums(x))
# }

#' Title
#'
#' @param n
#' @param alpha
#' @param beta
#'
#' @return
#' @export
#'
#' @examples
rdirg <- function(n, alpha, beta) {

  if (!isTRUE(all.equal(names(alpha), names(beta)))) {
    stop('alpha and beta need to have the same column names. can also be both NULL')
  }

  alpha2 <- (alpha / beta) ^ 2
  beta2 <- alpha / (beta) ^ 2
  k <- length(alpha2)
  x <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
    x[, i] <- rgamma(n, shape = alpha2[i], rate = beta2[i])
  }
  sample <- x / rowSums(x)
  colnames(sample) <- names(alpha)
  return(sample)
}

#' Random Dirichlet distrubted numbers.
#' Adds the gamma parameter compared to the standardt Dir variant.
#' @param n
#' @param alpha
#' @param gamma
#'
#' @return
#' @export
#'
#' @examples
rdir <- function(n, alpha, gamma) {
  sample <- gtools::rdirichlet(n, alpha * gamma)
  colnames(sample) <- names(alpha)
  return(sample)

}

#' Similar to `rdir` but adding a threshold argument.
#' For all variables `i` with an alpha below that threshold, the `rate` argument of
#' the `rgamma` functions is set to `1 / alpha[i]`, and `shape` is set `1`. This ensures less extreme values
#' for those variables which presumably originate from the know issues of
#' generating random gamma distributed number with a very small `shape` parameter.
#'
#' The reason for setting rate and shape parameters for small alphas like this is the following:
#' The gamma distribution (similar to the beta distribution), generates well behaved
#' random numbers only if the variables SD is not greater than the MEAN.
#' The gamma distribution can also be parametrised by a mean and sd using the
#' following relation between mean/sd and the shape and rate parameters:
#' $$
#' rate = \frac{\mu}{\sigma^2} \\
#' shape = (\frac{\mu}{\sigma})^2
#' $$
#' (derived from: https://en.wikipedia.org/wiki/Gamma_distribution#Mean_and_variance)
#'
#'
#' See `?rgamma`: "Note that for smallish values of shape (and moderate scale) a large parts of the mass of the Gamma distribution is on values of x so near zero that they will be represented as zero in computer arithmetic. So rgamma may well return values which will be represented as zero."
#'
#' Note: the current implementation is rather a pragmatic workaround for this
#' issue. Both the threshold parameter and the formula to set the `rate` parameter
#' are somehow arbitrarily chosen. However, since only variables with a very small
#' mean are affected this shouldn't affect overall results too much.
#'
#' @param n
#' @param alpha
#' @param gamma
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
rdir2 <- function(n, alpha, gamma, threshold = 1E-2) {
  alpha <- gamma * alpha
  l <- length(alpha)
  rate <- rep(1, l)
  #rate[alpha < threshold] <- sqrt(1/alpha[alpha < threshold])
  rate[alpha < threshold] <- 1 / alpha[alpha < threshold]
  alpha[alpha < threshold] <- 1
  x <- matrix(rgamma(l * n, alpha, rate), ncol = l, byrow = TRUE)
  # x <- matrix(rgamma2(n, alpha, rate), ncol = l, byrow = FALSE)
  sm <- rowSums(x)
  sample <- x/sm
  colnames(sample) <- names(alpha)
  return(sample)
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
    rdirg(N, alpha, beta)
  } else {
    stop('Case not implemented atm.')
  }
}


# rshares(100, c(3,3,3)) %>% boxplot
# rshares(100, c(0.1, 0.3, 0.6)) %>% boxplot
# rshares(100, c(0.1, 0.3, 0.6), c(0.1, 0.01, 0.2)) %>% boxplot





