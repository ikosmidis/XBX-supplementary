## beta01 -------------------------------------------------------------------------

beta01 <- function(object1, object2)
{
  ## FIXME: sanity checks?

  ## extract full response
  yh <- model.response(model.frame(object1))
  y <- model.response(model.frame(object2))
  y[yh == "0"] <- 0
  y[yh == "1"] <- 1

  ## type of boundary: only 0, only 1, or both 0 and 1
  boundary <- if(inherits(object1, "glm")) {
    if("1" %in% levels(yh)) "1" else "0"
  } else {
    "0/1"
  }

  ## collect relevant information
  rval <- list(
    hurdle = object1,
    betareg = object2,
    loglik = as.numeric(logLik(object2)) + as.numeric(logLik(object1)),
    df = attr(logLik(object2), "df") + ifelse(inherits(object1, "vglm"),
                                             object1@df.total - object1@df.residual,
                                             attr(logLik(object1), "df")),
    nobs = length(y),
    y = y,
    boundary = boundary
  )
  class(rval) <- "beta01"
  return(rval)
}

nobs.beta01 <- function(object, ...) object$nobs

logLik.beta01 <- function(object, ...) structure(object$loglik, df = object$df, class = "logLik")

terms.beta01 <- function(x, ...) x$betareg$terms

newresponse.beta01 <- function(object, newdata, na.action = na.pass, ...) {
  y <- newresponse(object$betareg, newdata = newdata, na.action = na.pass, ...)
  y[[2]] <- NULL
  return(y)
}

prodist.beta01 <- function(object, ...) {
  ## extract parameters of beta distributions
  mu <- predict(object$betareg, ..., type = "parameters")
  phi <- mu$phi
  mu <- mu$mu

  ## extract boundary probabilities
  if(object$boundary == "0/1") {
      hurdle_type <- ifelse(inherits(object$hurdle, "vglm"), "response", "prob")
      p <- predict(object$hurdle, ..., type = hurdle_type)
    p0 <- p[, 1L]
    p1 <- p[, 3L]
  } else if(object$boundary == "1") {
    p1 <- predict(object$hurdle, ..., type = "response")
    p0 <- 0 * p1
  } else {
    p0 <- predict(object$hurdle, ..., type = "response")
    p1 <- 0 * p0
  }

  ## set up distributions3 object
  Beta01(mu, phi, p0, p1)
}
