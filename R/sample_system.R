#' Title
#'
#' @param x
#' @param ct
#'
#' @return
#' @export
#'
#' @examples
generate_sampling_fun <- function(x, ct) {
  # Capture the current package versions
  disaggR_version <- as.character(packageVersion("disaggR"))
  nloptr_version <- as.character(packageVersion("nloptr"))


  function(n) {
    # Inside the generated function: Check package versions
    if (as.character(packageVersion("disaggR")) != disaggR_version) {
      stop("disaggR package version does not match the version used to generate this function.")
    }

    if (as.character(packageVersion("nloptr")) != nloptr_version) {
      stop("nloptr package version does not match the version used to generate this function.")
    }


    # choose dists
    dists_agg <- lapply(x, function(x) {
      generate_distribution_agg(mean = x$mean, sd = x$sd, a = x$a, b = x$b)
    })

    dists_shares <- lapply(1:nrow(ct), function(x) {
      generate_distribution_shares(alpha = ct$alpha[[x]], beta = ct$beta[[x]])
    })


    # Step 1: Sample from both lists of functions
    samples_agg <- lapply(dists_agg, function(x) x(n))
    samples_shares <- lapply(dists_shares, function(x) x(n))

    # Step 2: Multiply both samples
    sample_comb <- Map(`*`, samples_agg, samples_shares)

    # Step 3: Sum the matrices name-sensitively
    sample_comb2 <- sum_matrices(sample_comb)

    # Step 4 (optional): Convert samples of input variables (x) to matrix
    #sample_agg <- do.call('cbind', samples_agg)
    # return(sample_agg)

    return(sample_comb2)
  }
}


#' Title
#'
#' @param data a data.frame containing all input information
#' @param alpha
#' @param beta
#' @param y names
#' @param x column name of the information of the aggregate. The column must be a list of length nrow(data). Each list element itself is another list storing the information on the aggregate. The element of this inner elements must be named. Can include "mean", "sd", "a", "b" at the moment.
#'
#' @return
#' @export
#'
#' @examples
#' data <- structure(list(x = list(list(mean = 1, sd = 0.1, a = 0), list(
#'mean = 10, a = 0), list(a = 0, b = 20), list(mean = 20, sd = 1)),
#'alpha = list(c(`1` = 0.1, `2` = 0.3, `3` = 0.6), c(`1` = 0.3,
#'                                                   `2` = 0.7),
#'                                                   c(`2` = 0.333333333333333,
#'                                                   `4` = 0.333333333333333,
#'                                                   `5` = 0.333333333333333),
#'                                                   c(`5` = 1)),
#'                                                   beta = list(c(`1` = NA_real_,
#'                                                   `2` = NA_real_, `3` = NA_real_),
#'                                                   c(`1` = 0.1, `2` = 0.05),
#'                                                   c(`2` = NA_real_, `4` = NA_real_,
#'                                                   `5` = NA_real_), c(`5` = NA_real_)),
#'                                                   y = list(1:3, 1:2, c(2L, 4L, 5L), 5L)),
#'                                                   row.names = c(NA, -4L),
#'                                                   class = c("data.table", "data.frame"))
#'
#' fun <- generate_sampling_fun2(data)
#' fun(10)
#'
#'
#'
#'
#'




generate_sampling_fun2 <- function(data,
                                  x = 'x',
                                  alpha = 'alpha',
                                  beta = 'beta',
                                  y = 'y') {
  # Capture the current package versions
  disaggR_version <- as.character(packageVersion("disaggR"))
  nloptr_version <- as.character(packageVersion("nloptr"))


  function(n) {
    # Inside the generated function: Check package versions
    if (as.character(packageVersion("disaggR")) != disaggR_version) {
      stop("disaggR package version does not match the version used to generate this function.")
    }

    if (as.character(packageVersion("nloptr")) != nloptr_version) {
      stop("nloptr package version does not match the version used to generate this function.")
    }


    # choose dists
    dists_agg <- lapply(data[[x]], function(x) {
      generate_distribution_agg(mean = x$mean, sd = x$sd, a = x$a, b = x$b)
    })

    dists_shares <- lapply(1:nrow(data), function(x) {
      generate_distribution_shares(alpha = data[[alpha]][[x]], beta = data[[beta]][[x]])
    })


    # Step 1: Sample from both lists of functions
    samples_agg <- lapply(dists_agg, function(x) x(n))
    samples_shares <- lapply(dists_shares, function(x) x(n))

    # Step 2: Multiply both samples
    sample_comb <- Map(`*`, samples_agg, samples_shares)

    # Step 3: Sum the matrices name-sensitively
    sample_comb2 <- sum_matrices(sample_comb)

    # Step 4 (optional): Convert samples of input variables (x) to matrix
    #sample_agg <- do.call('cbind', samples_agg)
    # return(sample_agg)

    return(sample_comb2)
  }
}



sample_system <- function(n,
                          data,
                          x = 'x',
                          alpha = 'alpha',
                          beta = 'beta',
                          y = 'y') {

  # choose dists
  dists_agg <- lapply(data[[x]], function(x) {
    # Check if x is a list
    if (is.list(x)) {
      generate_distribution_agg(mean = x$mean, sd = x$sd, a = x$a, b = x$b)
    } else if (is.numeric(x) && is.null(dim(x))) {
      x
    }
  })

  dists_shares <- lapply(1:nrow(data), function(x) {
    generate_distribution_shares(alpha = data[[alpha]][[x]], beta = data[[beta]][[x]])
  })


  # Step 1: Sample from both lists of functions
  samples_agg <- lapply(dists_agg, function(x) {
    if (is.function(x)) x(n)
    else x
  } )
  samples_shares <- lapply(dists_shares, function(x) x(n))

  # Step 2: Multiply both samples
  sample_comb <- Map(`*`, samples_agg, samples_shares)

  # Step 3: Sum the matrices name-sensitively
  sample_comb2 <- sum_matrices(sample_comb)

  # Step 4 (optional): Convert samples of input variables (x) to matrix
  #sample_agg <- do.call('cbind', samples_agg)
  # return(sample_agg)

  return(sample_comb2)

}

