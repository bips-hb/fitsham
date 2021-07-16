#' @export
fit <- function(X, y, method = c("lasso", "enet", "bs", "fs"), ...) {
  args <- list(...)

  ### check whether the necessary arguments are there
  check_args <- function(args, needed_args) {
    given_args <- names(args)
    if (length(setdiff( needed_args, given_args )) != 0) {
      stop("requires additional arguments for this method to run")
    }
  }

  switch(method,
         "fs" = check_args(args, c("maxsteps")),
         "lasso" = check_args(args), c("lambda"),
         "bs" = check_args(args, c("maxsubsets")),
         "enet" = check_args(args, c("lambda", "alpha")))

  ### Apply the method
  switch(method,
         fs = obj    <- bestsubset::fs(X, y, intercept = FALSE,
                                       maxsteps = args$maxsteps),
         bs = obj    <- bestsubset::bs(X, y, intercept = FALSE,
                                       k = 0:args$maxsubsets, time.limit = 180),
         lasso = obj <- bestsubset::lasso(X, y, intercept = FALSE, alpha = 1, nrelax = 1,
                                          lambda = args$lambda),
         enet = obj  <- bestsubset::lasso(x, y, intercept = FALSE, alpha = alpha, nrelax = 1,
                                  lambda = args$lambda)
  )

  obj
}
