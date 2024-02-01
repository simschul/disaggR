#' Derivative of the Dirichlet entropy function.
#' Derivated using the `Deriv` package and `autodiffr`.
#' For derivation see script:
#'
#' @param x
#' @param shares
#'
#' @return
#' @export
#'
#' @examples
dirichlet_entropy_grad <- function (x, shares)
{
  #
  .e1 <- shares * x
  .e2 <- sum(.e1)
  -(((1 - prod(gamma(.e1))/(beta2(.e1) * gamma(.e2))) * digamma(.e2) +
       (.e2 - length(.e1)) * trigamma(.e2)) * sum(shares) -
      sum(shares * ((.e1 - 1) * trigamma(.e1) + digamma(.e1))))
}

beta2 <- function(alpha) {
  prod(gamma(alpha)) / gamma(sum(alpha))
}



# Objective Function
eval_f <- function(x, shares) {
  # from:  https://en.wikipedia.org/wiki/Dirichlet_distribution#Entropy
  # calculates the negative entropy of a dirichlet distribution given `shares` (sum to 1) and a concentration parameter `x`, so that `alpha = shares * x`
  alpha <- x * shares
  K <- length(alpha)
  psi <- digamma(alpha)
  alpha0 <- sum(alpha)
  (-(log(beta2(alpha)) + (alpha0 - K) * digamma(alpha0) - sum((alpha - 1) * psi)))
}


#' Like `find_gamma_maxent`: Finds the Gamma value which maximises the entropy of a Dirichlet distribution
#' with given alphas (=sector shares), using the nloptr optimazation package.
#'
#' BUT: including the first derivative of the Dir Entropy function `dirichlet_entropy_grad`,
#' and thus is **much** faster than `find_gamma_maxent`.
#'
#' @param shares
#' @param eval_f
#' @param eval_grad_f
#' @param x0
#' @param bounds
#' @param shares_lb lower bound of shares. Only values LARGER than that will be considered (set this to e.g. 1E-4) to increase chance of convergence
#' @param local_opts
#' @param opts
#'
#' @return
#' @export
#'
#' @examples
find_gamma_maxent2 <- function(shares,
                               eval_f = eval_f,
                               eval_grad_f = dirichlet_entropy_grad,
                               x0 = 1, # initial value of gamma
                               x0_n_tries = 100,
                               bounds = c(0.001, 300),
                               shares_lb = 0,
                               local_opts = list( "algorithm" = "NLOPT_LD_MMA", # optim options
                                                  "xtol_rel" = 1.0e-4 ),
                               opts = list( "algorithm"= "NLOPT_GD_STOGO",
                                            "xtol_rel"= 1.0e-4,
                                            "maxeval"= 1E3,
                                            "local_opts" = local_opts,
                                            "print_level" = 0 )

) {

  # remove shares of zero
  #shares <- shares[which(shares > shares_lb)]

  # lower and upper bounds
  lb <- bounds[1]
  ub <- bounds[2]

  count <- 0
  while(!is.finite(eval_f(x = x0, shares = shares))
        | !is.finite(eval_grad_f(x = x0, shares = shares))) {
    if (count > x0_n_tries) {
      #stop('Error: Could not find an initial value x0 which is defined by eval_f and/or eval_grad_f. Either increase x0_n_tries (defaul: 100), or increase the parameter space with the bounds argument')
      warning('Warning: Could not find an initial value x0 which is defined by eval_f and/or eval_grad_f. Either increase x0_n_tries (defaul: 100), or increase the parameter space with the bounds argument. Q&D solution: x0 is set to 1')
      x0 <- 1

      # remove all very small shares
      shares[shares < shares_lb]

      break
    } else {
      x0 <- runif(1, min = lb, max = ub)
      count <- count + 1
    }
  }

  # Run the optimizer
  res <- nloptr ( x0 = x0,
                  eval_f = eval_f,
                  eval_grad_f = eval_grad_f,
                  lb = lb,
                  ub = ub,
                  opts = opts,
                  shares = shares
  )

  return(res)
}

