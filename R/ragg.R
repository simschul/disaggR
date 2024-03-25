ragg <- function(n, mean, sd = NULL, min = 0, max = Inf) {
  if (!is.null(mean) & !is.null(sd) & min == -Inf & max == Inf) {
    # Normal distribution
    rnorm(n, mean, sd)
  } else if (!is.null(mean) & !is.null(sd)) {
    # Truncated normal
    rtruncnorm(n, a = min, b = max, mean = mean, sd = sd)
  } else if (!is.null(mean) & is.null(sd) & min == 0 & max == Inf) {
    # Exponential
    rexp(n, rate = 1 / mean)
  } else if (is.null(mean) & is.null(sd) & is.finite(min) & is.finite(max)) {
    # uniform
    runif(n, min = min, max = max)
  } else {
    stop('Case not implemented atm.')
  }
}
# ragg(N = 100, mean = 1) %>% hist
# ragg(N = 100, mean = 1, sd = 0.5) %>% hist
# ragg(N = 100, mean = 1, sd = 0.5, min = -Inf) %>% hist
