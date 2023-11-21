ragg <- function(N, mu_0, sd_0 = NULL, a = 0, b = Inf) {
  if (!is.null(mu_0) & !is.null(sd_0) & a == -Inf & b == Inf) {
    # Normal distribution
    rnorm(N, mu_0, sd_0)
  } else if (!is.null(mu_0) & !is.null(sd_0) & a == 0 & b == Inf) {
    # Truncated normal
    rtruncnorm(N, a = a, b = b, mean = mu_0, sd = sd_0)
  } else if (!is.null(mu_0) & is.null(sd_0) & a == 0 & b == Inf) {
    # Exponential
    rexp(N, rate = 1 / mu_0)
  } else if (is.null(mu_0) & is.null(sd_0) & is.finite(a) & is.finite(b)) {
    # uniform
    runif(N, min = a, max = b)
  } else {
    stop('Case not implemented atm.')
  }

}
ragg(N = 100, mu_0 = 1) %>% hist
ragg(N = 100, mu_0 = 1, sd = 0.5) %>% hist
ragg(N = 100, mu_0 = 1, sd_0 = 0.5, a = -Inf) %>% hist
