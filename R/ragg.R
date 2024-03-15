ragg <- function(N, mean, sd = NULL, min = 0, max = Inf) {
  if (!is.null(mean) & !is.null(sd) & min == -Inf & max == Inf) {
    # Normal distribution
    rnorm(N, mean, sd)
  } else if (!is.null(mean) & !is.null(sd)) {
    # Truncated normal
    rtruncnorm(N, a = min, b = max, mean = mean, sd = sd)
  } else if (!is.null(mean) & is.null(sd) & min == 0 & max == Inf) {
    # Exponential
    rexp(N, rate = 1 / mean)
  } else if (is.null(mean) & is.null(sd) & is.finite(min) & is.finite(max)) {
    # uniform
    runif(N, min = min, max = max)
  } else {
    stop('Case not implemented atm.')
  }
}
# ragg(N = 100, mean = 1) %>% hist
# ragg(N = 100, mean = 1, sd = 0.5) %>% hist
# ragg(N = 100, mean = 1, sd = 0.5, min = -Inf) %>% hist
