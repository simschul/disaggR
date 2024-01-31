rdir_maxent <- function(N, shares, ...) {
  out <- find_gamma_maxent2(shares, eval_f = eval_f, ...)
  sample <- gtools::rdirichlet(N, shares * out$solution)
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





