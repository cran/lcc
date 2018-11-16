#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lccWrapper.R                                                 #
# Contains: lccWrapper function                                      #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 18/06/2018                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal function to to prepare the \code{lccBuilder} function
##'
##' @description This is an internally called function used to prepare the \code{\link[lcc]{lccBuilder}} function.
##'
##' @usage NULL
##'
##' @details returns a vector or list containing the longitudinal concordance correlation estimates.
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@usp.br}
##'
##' @keywords internal
lccWrapper <- function(model, q_f, tk, diffbeta, n.delta) {
  G <- getVarCov(model)
  q_r <- dim(G)[1] - 1
  deltas <- getDelta(model = model)
  delta <- deltas$delta
  deltal <- deltas$deltal
  g <- deltas$g
  sig2_epsilon <- model$sigma^2
  rho <- lccBuilder(G = G, diffbeta = diffbeta, tk = tk, q_r = q_r,
                     q_f = q_f, g = g, sig2_epsilon = sig2_epsilon,
                     delta = delta, deltal = deltal, model = model)
  if(is.na(rho[[2]])) {
    return(rho[[1]])
  }else(return(rho[[n.delta]]))
}