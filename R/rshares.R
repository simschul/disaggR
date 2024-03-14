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
#' See `?rgamma`: "Note that for smallish values of shape (and moderate scale) a
#' large parts of the mass of the Gamma distribution is on values of x so
#' near zero that they will be represented as zero in computer arithmetic.
#' So rgamma may well return values which will be represented as zero."
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


# rshares <- function(N, alpha, beta = NULL) {
#   if (isTRUE(all.equal(var(alpha), 0)) & is.null(beta)) {
#     # Dirichlet 1
#     rdir1(N, length = length(alpha))
#   } else if (is.null(beta)) {
#     # Dirichlet MaxEnt
#     rdir_maxent(N, alpha)
#   } else if (!is.null(beta)) {
#     # Gen. Dirichlet
#     rdirg(N, alpha, beta)
#   } else {
#     stop('Case not implemented atm.')
#   }
# }

rshares <- function(n, alpha, beta = NULL,
                    na_action = 'remove', max_iter = 1E3) {
  if (is.null(beta)) {
    beta <- rep(NA, length(alpha))
    names(beta) <- names(alpha)
  }
  if (na_action == 'remove') {
    beta <- beta[!is.na(alpha)]
    alpha <- alpha[!is.na(alpha)]
  } else if (na_action == 'fill') {
    alpha[is.na(alpha)] <- (1 - sum(alpha, na.rm = TRUE)) / length(alpha[is.na(alpha)])
  } else {
    stop('na_action must be either "remove" or "fill"!')
  }

  if (sum(alpha) != 1) {
    stop('alpha must sum to one! If you have NAs in your alpha consider setting "na_action" to "fill". ')
  }

  K <- length(alpha)
  has_mean_only <- is.finite(alpha) & !is.finite(beta)
  has_sd_only <- is.finite(beta) & !is.finite(alpha)
  has_both <- is.finite(alpha) & is.finite(beta)

  if (all(has_both)) {
    # Generalised dirichlet
    sample <- rdirg(n, alpha, beta)
  } else if (all(has_mean_only)) {
    # Dirichlet with maxent fitted gamma
    sample <- rdir_maxent(N, alpha)
  } else {
    # partial info: nested approach
    sample <- matrix(0, nrow = n, ncol = K)
    colnames(sample) <- names(alpha)

    if (sum(has_both) > 0){
      # sample all shares with both mean + sd
      sample[, has_both] <- rbeta3(n, mean = alpha[has_both],
                                   sd = beta[has_both], max_iter = max_iter)
    }

    if (sum(has_mean_only) > 0) {
      # rescale alpha to sum to one
      alpha2 <- alpha[has_mean_only] / sum(alpha[has_mean_only])
      # sample all shares with mean only
      sample_temp <- rdir_maxent(n, alpha2)
      # rescale sample to make rows sum to one
      sample[, has_mean_only] <- sample_temp * (1-rowSums(sample))
    }
  }
  return(sample)
}


# rshares(100, c(3,3,3)) %>% boxplot
# rshares(100, c(0.1, 0.3, 0.6)) %>% boxplot
# rshares(100, c(0.1, 0.3, 0.6), c(0.1, 0.01, 0.2)) %>% boxplot



#' Generate random numbers from the beta distribution. Similar to `base::rbeta`, but:
#' - vectorized
#' - parametrised with mean and sd (derived from: https://en.wikipedia.org/wiki/Beta_distribution#Mean_and_variance)
#' - Ensures that the sum of the samples do not exceed 1.
#'
#' It is similar to a generalised Dirichlet, however the main functional difference
#' is that the samples do NOT sum to 1 (but are <= 1).
#'
#' The constraint that the sample sum is <= 1 is currently achieved by sampling
#' as long until `n` samples exist that satisfy the constraint. Depending on the
#' parameter combination this can be quite computational intensive.
#'
#' Note that the beta distribution is only defined if (mean * (1-mean)) < sd^2
#'
#' @param n number of samples
#' @param mean a vector of means (between 0 and 1, sum(means) <= 1)
#' @param sda vector of standard devaitions
#' @param fix if set to TRUE (default) the SD of all variables for which the
#' parameter combination of mean and sd is undedefined ((mean * (1-mean)) < sd^2).
#' If set to FALSE the function will throw an error message if there are undefined
#' parameter combinations.
#' @param max_iter the maximum number of iterations to sample.
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
rbeta3 <- function(n, means, sds, fix = TRUE, max_iter = 1E3) {
  var <- sds^2
  undef_comb <- (means * (1-means)) < var
  if (!all(!undef_comb)) {
    if (isTRUE(fix)) var[undef_comb] <- means[undef_comb]^2
    else stop('The beta distribution is not defined for the
                                    parameter combination you provided!
                                    sd must be smaller or equal sqrt(means*(1-means))')

  }
  # from: https://en.wikipedia.org/wiki/Beta_distribution#Mean_and_variance
  alpha <- means * (((means * (1-means)) / var) - 1)
  beta <- (1-means)*(((means * (1-means)) / var) - 1)

  k<- length(means)
  x <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
    x[, i] <- rbeta(n, alpha[i], beta[i])
  }
  larger_one <- rowSums(x) > 1
  count <- 0
  while(sum(larger_one) > 0) {
    for (i in 1:k) {
      x[larger_one, i] <- rbeta(sum(larger_one), alpha[i], beta[i])
    }
    larger_one <- rowSums(x) > 1
    count <- count + 1
    if (count > max_iter) stop('max_iter is reached. the combinations of means
    and sds you provided does allow to generate `n` random samples that are
                               not larger than 1. Either increase max_iter, or
                               change parameter combination. ')
  }

  return(x)
}

