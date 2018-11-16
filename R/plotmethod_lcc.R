#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: plotmethod_lcc.R                                              #
# Contains: lccPlot function                                          #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 18/06/2018                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################
##' @title Plot an lcc object
##' 
##' @description A plot of predictions versus the time covariate is generated. Predicted values are joined by lines while sampled observations are represented by circles. If the argument \code{components=TRUE} is considered in the \code{lcc} object, single plots of each statistics are returned on differents pages.           
##'
##' @usage
##' lccPlot(obj, control, ...)
##'     
##' @param obj an object inheriting from class "lcc", representing a fitted lcc model.
##' 
##' @param control a list of control values or character strings returned by the function \code{\link{plotControl}}. Defaults to an empty list.  The list may contain the following components:
##' \describe{
##' \item{\code{shape}:}{draw points considering a shape parameter. Possible shape values are the numbers 0 to 25, and 32 to 127; see \code{\link[ggplot2]{aes_linetype_size_shape}}. Default is \code{1}.}
##'
##' \item{\code{colour}:}{specification for lines color. Default is \code{"black"}.}
##'
##' \item{\code{size}:}{specification for lines size. Should be specified with a numerical value (in millimetres); see \code{\link[ggplot2]{aes_linetype_size_shape}}. Default is \code{0.5}.}
##'
##' \item{\code{xlab}:}{title for the \code{x} axis.  Default is \code{"Time"}.}
##'
##' \item{\code{LCC_ylab}:}{title for the \code{y} axis related with LCC. Default is \code{"LCC"}.}
##'
##' \item{\code{LPC_ylab}:}{title for the \code{y} axis related with LPC. Default is \code{"LPC"}.}
##'
##' \item{\code{LA_ylab}:}{title for the \code{y} axis related with LA. Default is \code{"LA"}.}
##'
##' \item{\code{LCC_scale_y_continuous}:}{numeric vector of length two providing limits of the scale related to LCC. Default is \code{c(0,1)}.}
##'
##' \item{\code{LPC_scale_y_continuous}:}{numeric vector of length two providing limits of the scale related to LPC. Default is \code{c(0,1)}.}
##'
##' \item{\code{LA_scale_y_continuous}:}{numeric vector of length two providing limits of the scale related to LA. Default is \code{c(0,1)}.}
##'
##' \item{\code{all.plot}:}{\code{viewport} functions for the \code{lcc} class. If \code{TRUE}, the default, returns an object created by the \code{\link[grid]{viewport}} function with multiple plots on a single page. If \code{FALSE} returns a single \code{\link[ggplot2]{ggplot}} object by different pages.}
##' }
##' 
##' @param ... not used.
##' 
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@usp.br}
##' 
##' @references Lin, L. A Concordance Correlation Coefficient to Evaluate Reproducibility. \emph{Biometrics}, 45, n. 1, 255-268, 1989. 
##' @references Oliveira, T.P.; Hinde, J.; Zocchi S.S. Longitudinal Concordance Correlation Function Based on Variance Components: An Application in Fruit Color Analysis. \emph{Journal of Agricultural, Biological, and Environmental Statistics}, v. 23, n. 2, 233–254, 2018.
##' 
##' @seealso \code{\link[lcc]{lcc}}.
##' 
##' @examples
##'
##' data(hue)
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1<-lcc(dataset = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2,
##'          components=TRUE)
##' lccPlot(fm1)
##'          
##' @export

lccPlot<-function(obj, control = list(), ...){
  
  # Arguments for the plot
  plot.cons<-plotControl(shape=1, colour="black",
    size=0.5, xlab = "Time",
    LCC_ylab = "LCC", LPC_ylab = "LPC",
    LA_ylab = "LA",
    LCC_scale_y_continuous=c(0,1),
    LPC_scale_y_continuous=c(0,1),
    LA_scale_y_continuous=c(0,1),
    all.plot = TRUE)
  if(length(control)){
    nms <- names(control)
    if (!is.list(control) || is.null(nms))
      stop("'control' argument must be a named list")
    pos <- pmatch(nms, names(plot.cons))
    if (any(nap <- is.na(pos))) {
      warning(sprintf(ngettext(length(nap), "unrecognized plot element named %s ignored",
        "unrecognized plot elements named %s ignored"),
        paste(sQuote(nms[nap]), collapse = ", ")), domain = NA)
      pos <- pos[!nap]
      control <- control[!nap]
    }
    for(i in 1:length(pos)){
      plot.cons[[pos[i]]]<-control[[i]]
    }
  }
  

  #Standard arguments
  nd<-obj$plot_info$nd
  model<-obj$model
  tk.plot<-obj$plot_info$tk.plot
  tk.plot2<-obj$plot_info$tk.plot2
  ldb<-obj$plot_info$ldb
  ci<-obj$plot_info$ci
  components<-obj$plot_info$components
  
    if(ci==FALSE) {
      if(ldb == 1) {
          plot_lcc(rho=obj$plot_info$rho, tk.plot= tk.plot,
            tk.plot2=tk.plot2, ldb=ldb,
            model=model, ci = ci,
            arg=plot.cons)
        if(components==TRUE){
            plot_lpc(LPC=obj$plot_info$rho.pearson, tk.plot= tk.plot,
              tk.plot2=tk.plot2, ldb=ldb,
              model=model, ci = ci, arg = plot.cons)
            plot_la(Cb=obj$plot_info$Cb, tk.plot= tk.plot,
              tk.plot2=tk.plot2, ldb=ldb,
              model=model, ci = ci, arg = plot.cons)
        }
      } else {
          plot_lcc(rho=obj$plot_info$rho, tk.plot= tk.plot,
            tk.plot2=tk.plot2, ldb=ldb, model=model,
            ci = ci, arg = plot.cons)
        if(components==TRUE){
            plot_lpc(LPC=obj$plot_info$rho.pearson, tk.plot= tk.plot,
              tk.plot2=tk.plot2, ldb=ldb, model=model,
              ci = ci, arg = plot.cons)
            plot_la(Cb=obj$plot_info$Cb, tk.plot= tk.plot,
              tk.plot2=tk.plot2, ldb=ldb, model=model,
              ci = ci, arg = plot.cons)
        }
      }
    }else{
      ENV.LCC<-obj$plot_info$ENV.LCC
        plot_lcc(rho=obj$plot_info$rho, ENV.LCC=ENV.LCC, tk.plot= tk.plot,
          tk.plot2=tk.plot2, ldb=ldb, model=model,
          ci = ci, arg = plot.cons)
        if(components==TRUE){
        ENV.LPC<-obj$plot_info$ENV.LPC
        ENV.Cb<-obj$plot_info$ENV.LA
        plot_lpc(LPC=obj$plot_info$rho.pearson,ENV.LPC=ENV.LPC, tk.plot= tk.plot,
          tk.plot2=tk.plot2, ldb=ldb, model=model, ci = ci, arg = plot.cons)
        plot_la(Cb=obj$plot_info$Cb,ENV.Cb = ENV.Cb, tk.plot= tk.plot,
          tk.plot2=tk.plot2, ldb=ldb, model=model, ci = ci, arg = plot.cons)
        }
    }
}