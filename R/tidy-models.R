# These are wrappers for modeling functions that return models as tidy datasets, ala the broom pkg.

#' Modification of the lm function to (1) include a clustered se option and (2)
#' return a tidy model.
#'
#' @param ... Any parameters to be passed to lm, including formula.
#' @param cluster A vector to be used for clustering.
#'
#' @return A tidy model.
#'
#' @export
lm.tidy <- function(..., cluster = NULL) {
  require(broom)
  model <- lm(...)

  # cluster or not
  if(is.null(cluster))
    model.r <- model
  else
    model.r <- clustered_se(model, cluster)

  # tidy, works fine with coeftest objects
  model.r <- tidy(model.r)

  # Add outcome attribute
  attr(model.r, "y") <- as.character(model$terms[[2]])
  model.r
}

#' Modification of the glm function to (1) include a clustered se option and (2)
#' return a tidy model.
#'
#' @param ... Any parameters to be passed to lm, including formula.
#' @param cluster A vector to be used for clustering.
#'
#' @return A tidy model.
#'
#' @export
logit.tidy <- function(..., cluster = NULL) {
  require(broom)
  model <- glm(..., binomial(link = 'logit'))

  # cluster or not
  if(is.null(cluster))
    model.r <- model
  else
    model.r <- clustered_se(model, cluster)

  # tidy, works fine with coeftest objects
  model.r <- tidy(model.r)

  # Add outcome attribute
  attr(model.r, "y") <- as.character(model$terms[[2]])
  model.r
}

#' Modification of the lfe::felm function to return a tidy model.
#'
#' @param ... Any parameters to be passed to felm, including formula.
#'
#' @return A tidy model.
#'
#' @export
felm.tidy <- function(...) {
  require(lfe)
  require(broom)
  model <- felm(...)
  model.r <- tidy(model)
  attr(model.r, "y") <- model$lhs
  model.r
}

#' Cluster standard errors for the given linear model. Specifically for
#' panel results.
#'
#' @param model The model.
#' @param cluster A vector that can be used to cluster observations.
#'
#' @return A coeftest object with clustered standard errors.
#'
#' @export
clustered_se <- function(model, cluster) {
  require(lmtest)
  require(clubSandwich)
  coeftest(model,
           vcovCR(model,
                  cluster,
                  type = "CR1"),
           df = length(unique(cluster)) - 1)
}
