rdisagg <- function(n, mean_0, sd_0 = NULL, min = 0, max = Inf,
                    shares, sds = NULL) {
  sample_agg <- ragg(n = n, mean = mean_0, sd = sd_0, min = min, max = max)
  sample_shares <- rshares(n = n, shares = shares, sds = sds)
  sample_disagg <- sample_shares * sample_agg

  #names(sample_disagg) <- names(alpha)
  return(sample_disagg)
}
# temp1 <- ragg(N = 3, mu_0 = 100)
# temp2 <- rshares(N = 3, alpha = c(0.1, 0.3, 0.6))
# temp3 <- temp2 * temp1
# rdisagg(N = 100, mu_0 = 100, sd_0 = 13, a = 0, b = Inf,
#         alpha = c(0.1, 0.3, 0.6)) %>% boxplot
