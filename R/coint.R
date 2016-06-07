#'
#' @name coint
#' @title Long and Short-Run coefficients
#' @description Present LR and SR coefficients of an ardl (Auto Regressive Distributed Lag) object
#' @details Uses delta method to calculate standard-errors of the Long Run (LR) coefficients.
#' 
#' @param obj An object of class \code{ardl}. 
#' @param type (optional) Select results format that can be \code{c("txt","tex")}. Defaults to \code{"tex"}.
#' @param file (optional) Filename to save results. Defaults to \code{NULL}.
#' @return Character strings. 
#'  
#' @export 
# @import xtable xtable
# @importFrom xtable xtable
# @importFrom xtable print.xtable
#' 
#' @seealso \code{\link{bounds.test}}
#' 
#' @examples
#' 
#' data("br_month")
#' phillips <- ardl( cpi~prod+ner|d_lula, data=br_month, ylag=2, xlag=c(1,2), case=5 )
#' coint( phillips )
#' coint( phillips, type="txt", file="phillips.txt" )
#' coint( phillips, type="tex", file="phillips.tex" )

coint <- function( obj, type="txt", file=NULL ){
  
  if (!inherits(obj,"ardl")) stop("Class of the argument must be ardl.")
  
  match.arg(type,c("tex","txt")) 

  CaptionLR <- "Long-Run Coefficients (Cointegration Relation)"
  CaptionSR <- paste0("Short-Run Coefficients. Dependent variable is d(",obj$lhs,")")

  if (type=="txt") {
    if (!is.null(file)) sink(file, append=FALSE)
    
    cat("AutoRegressive Distributed Lag model\n")
    cat("Dependent variable: ",obj$lhs,"\n\n")
    cat("Call:\n", paste(deparse(attr(obj$model,"terms")), sep = "\n", collapse = "\n"), "\n", sep = "")

    cat("",rep("_",60),sep="","\n")
    cat(     CaptionSR,"\n")
    cat(     rep("_",60),sep="","\n")
    print_coeff( obj$coeff_sr, obj$coeff_sr_sd )

    cat("",rep("_",60),sep="","\n")
    cat(     CaptionLR,"\n")
    cat(     rep("_",60),sep="","\n")
    print_coeff( obj$coeff_lr, obj$coeff_lr_sd )
    
    if (!is.null(file)) sink()
  }

  if (type=="tex") {
    # TODO: header
    xtabLR <- xtable::xtable( print_coeff( obj$coeff_lr, obj$coeff_lr_sd ),caption=CaptionLR )
    xtabSR <- xtable::xtable( print_coeff( obj$coeff_sr, obj$coeff_sr_sd ),caption=CaptionSR )
    if (!is.null(file)) {
      sink(file, append=FALSE)
      cat("%\\documentclass{article}\n%\\begin{document}\n\n")
      sink()
      xtable::print.xtable(xtabLR, type = "latex", file = file, append=TRUE )
      xtable::print.xtable(xtabSR, type = "latex", file = file, append=TRUE )
      sink(file, append=TRUE)
      cat("\n\n%\\end{document}")
      sink()
    }
  }
  
}

print_coeff <- function( coeff, sd ){
  # builds a matrix with 4 columns to call printCoefmat()
  cmat <- cbind( coeff, sd, coeff/sd, 2*(1-pnorm(abs(coeff/sd))) ) #2*pnorm(-coeff/sd )
  cmat <- matrix( cmat, ncol=4 )
  colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
  rownames(cmat) <- names(coeff)
  printCoefmat( cmat, digits=4, signif.stars=TRUE, P.values = TRUE )
}
