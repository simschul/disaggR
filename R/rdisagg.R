rdisagg <- function(N, mu_0, sd_0 = NULL, a = 0, b = Inf,
                    alpha, beta = NULL) {
  sample_agg <- ragg(N = N, mu_0 = mu_0, sd_0 = sd_0, a = a, b = b)
  sample_shares <- rshares(N = N, alpha = alpha, beta = beta)
  sample_disagg <- sample_shares * sample_agg

  return(sample_disagg)
}
# temp1 <- ragg(N = 3, mu_0 = 100)
# temp2 <- rshares(N = 3, alpha = c(0.1, 0.3, 0.6))
# temp3 <- temp2 * temp1
# rdisagg(N = 100, mu_0 = 100, sd_0 = 13, a = 0, b = Inf,
#         alpha = c(0.1, 0.3, 0.6)) %>% boxplot
