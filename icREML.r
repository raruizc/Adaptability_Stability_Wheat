## Function icREML
##
##' For a list of fitted asreml objects, finds the AIC and BIC for each
##' model in the list
##'
##' This function calculates the AIC and BIC for each fitted asreml model in a list.
##' Options are set and each model is updated.  The elements required for the AIC and
##' BIC are then calculated.
##'
##' @title Find the AIC and BIC for a set of models fitted using asreml
##' @param fm A \code{list} of asreml fitted model objects
##' @param scale A scalar to scale the variance matrix of the estimated
##' fixed effects (to ensure numerical stability of a log-determinant).
##' Default value is 1.
##' @return A data frame.  The data frame has the following components
##' \itemize{
##' \item \code{model} : the names of the models
##' \item \code{loglik} : the full log-likelihood for each model
##' \item \code{p} :  the number of fixed effects parameters for each model
##' \item \code{q} : the number of (non-zero) variance parameters for each model.
##' \item \code{b} : the number of variance parameters that are fixed or on the
##' boundary.  These parameters are not counted in the AIC or BIC.
##' \item \code{AIC} : the AIC for each model
##' \item \code{BIC} : the BIC for each model
##' \item \code{logdet} : the log-determinant used in adjusting the residual
##' log-likelihood for each model
##' }
##'
##' @author Ari Verbyla (ari.verbyla at csiro.au)
##'
icREML <- function(fm, scale=1) {
    if(!is.list(fm)) stop(" Models need to be in a list\n")
    if(is.null(names(fm))) namesfm <- paste("fm", 1:length(fm))
    else namesfm <- names(fm)
    require(asreml)
    asreml.options(Cfixed = TRUE, gammaPar=FALSE)
    fm <- lapply(fm, function(el) {
        if(is.null(el$Cfixed)) {
            out <- update.asreml(el) } #update(el, maxit=1)
        else out <- el
        out})
    logl <- lapply(fm, function(el) el$loglik)
    summ <- lapply(fm, function(el) summary(el, coef=TRUE)$coef.fixed)
    which.X0 <- lapply(summ, function(el) !is.na(el[, "z.ratio"]))
    p.0 <- lapply(which.X0, function(el) sum(el))
    Cfixed <- lapply(fm, function(el) el$Cfixed)
    logdet <- lapply(1:length(fm), function(el, Cfixed, which.X0, scale) {
        log(prod(svd(as.matrix(scale*Cfixed[[el]][which.X0[[el]], which.X0[[el]]]))$d))
    }, Cfixed, which.X0, scale)
    vparam <- lapply(fm, function(el) summary(el)$varcomp)
    q.0 <- lapply(vparam, function(el) sum(!(el$bound == "F" | el$bound == "B")))
    b.0 <- lapply(vparam, function(el) sum(el$bound == "F" | el$bound == "B"))
    logl <- lapply(1:length(fm), function(el, logl, logdet, p.0) {
        logl[[el]] - logdet[[el]]/2}, logl, logdet,p.0)
    aic <- unlist(lapply(1:length(fm), function(el, logl, p.0, q.0) {
        -2*logl[[el]] + 2*(p.0[[el]] + q.0[[el]])}, logl, p.0, q.0))
    bic <- unlist(lapply(1:length(fm), function(el, logl, p.0, q.0, fm) {
        -2*logl[[el]] + log(fm[[el]]$nedf+p.0[[el]])*(p.0[[el]] + q.0[[el]])},
        logl, p.0, q.0, fm))
    results <- data.frame(model=namesfm, loglik = unlist(logl), p=unlist(p.0),
                          q=unlist(q.0), b = unlist(b.0), AIC = aic, BIC = bic, logdet=unlist(logdet))
    row.names(results) <- 1:dim(results)[1]
    invisible(results)
}
