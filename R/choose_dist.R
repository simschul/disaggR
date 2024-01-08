choose_dist_agg <- function(mean, sd, a, b) {
  if (is.null(mean)) mean <- NA
  if (is.null(sd)) sd <- NA
  if (is.null(a)) a <- NA
  if (is.null(b)) b <- NA

  if (is.finite(a) & is.finite(b) & a >= b) {
    warning('a must be < b')
    return(NA)
  }
  if (is.finite(sd) & sd <=0) {
    warning('sd must be positive')
    return(NA)
  }
  if (is.finite(mean) & is.finite(a) & mean <= a) {
    warning('mean must be > a')
    return(NA)
  }
  if (is.finite(mean) & is.finite(b) & mean >= b) {
    warning('mean must be < b')
    return(NA)
  }

  if (is.finite(mean)) {
    if (is.finite(sd)) {
      if (is.finite(a) | is.finite(b)) {
        # return(generate_distribution('rtruncnorm', mean = mean, sd = sd,
        #                              a = a, b = b))

        return('truncnorm')
      } else {
        # return(generate_distribution('rnorm', mean = mean, sd = sd))
        return('norm')
      }
    } else if (isTRUE(all.equal(a, 0)) & !is.finite(b)) {
      return('exp')
    } else {
      return(NA)
    }
  } else if (is.finite(a) & is.finite(b) & !is.finite(sd)) {
    return('unif')
  } else {
    NA
  }
}


choose_dist_disagg <- function(alpha, beta) {
  if (all(is.na(beta))) beta <- NULL

  if (isTRUE(all.equal(var(alpha), 0)) & is.null(beta)) {
    # Dirichlet 1
    return('dir1')
  } else if (is.null(beta)) {
    # Dirichlet MaxEnt
    return('dir_maxent')
  } else if (!is.null(beta)) {
    # Gen. Dirichlet
    return('gdir')
  } else {
    return(NA)
  }
}


#' Title
#'
#' @param fun
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_distribution <- function(fun, ...) {
  options_allowed <- c('runif', 'rnorm', 'rlnorm', 'rtruncnorm', 'rexp',
                       'rweibull', 'rdir_maxent', 'rdir1', 'rgdir')
  fun_name <- as.character(substitute(fun))

  if (!(fun_name %in% options_allowed)) {
    stop(paste0("'fun' should be one of ", toString(options_allowed)))
  }

  # Capture the additional arguments as a list
  args <- as.list(match.call())[-c(1,2)]  # Remove the first two elements

  # Construct argument string with names and values
  args_strings <- sapply(names(args), function(arg_name) {
    arg_value <- args[[arg_name]]
    if(is.character(arg_value)) {
      return(paste0(arg_name, " = '", arg_value, "'"))
    } else {
      return(paste0(arg_name, " = ", arg_value))
    }
  })

  # Create function body as a string
  fun_body_string <- sprintf("function(n) %s(n, %s)",
                             fun_name,
                             paste(args_strings, collapse = ", "))

  # Create a new function from the string
  new_fun <- eval(parse(text = fun_body_string), envir = parent.frame())

  return(new_fun)
}

