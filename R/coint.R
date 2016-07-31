#'
#' @name coint
#' @title Long and Short-Run coefficients
#' @description Present LR and SR coefficients of an ardl (Auto Regressive Distributed Lag) object
#' @details Uses delta method to calculate standard-errors of the Long Run (LR) coefficients.
#' 
#' @param obj An object of class \code{ardl}. 
#' @param type (optional) Select results format that can be \code{c("txt","tex")}. Defaults to \code{"tex"}.
#' @param file (optional) Filename to save results. Defaults to \code{NULL}.
#' @param caption (optional) Caption (title) of the table. Defaults to \code{NULL}.
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

coint <- function( obj, type="txt", file=NULL, caption=NULL ){
  
  if (!inherits(obj,"ardl")) stop("Class of the argument must be ardl.")
  
  match.arg(type,c("tex","txt")) 

  CaptionLR <- paste0("Long-Run Coefficients. Dependent variable is ",obj$lhs,"") # Cointegration Relation
  CaptionSR <- paste0("Short-Run Coefficients. Dependent variable is d(",obj$lhs,")")

  if (type=="txt") {
    if (!is.null(file)) sink(file, append=FALSE)
    
    cat("AutoRegressive Distributed Lag model\n")
    cat("Dependent variable: ",obj$lhs,"\n\n")
    cat("Call:\n", paste(deparse(attr(obj$model,"terms")), sep = "\n", collapse = "\n"), "\n", sep = "")

    cat("",rep("_",60),sep="","\n")
    cat(     CaptionSR,"\n")
    cat(     rep("_",60),sep="","\n")
    cmat <- coeff_matrix( obj$coeff_sr, obj$coeff_sr_sd )
    printCoefmat( cmat, digits=4, signif.stars=TRUE, P.values = TRUE )  
    
    cat("",rep("_",60),sep="","\n")
    cat(     CaptionLR,"\n")
    cat(     rep("_",60),sep="","\n")
    cmat <- coeff_matrix( obj$coeff_lr, obj$coeff_lr_sd )
    printCoefmat( cmat, digits=4, signif.stars=TRUE, P.values = TRUE ) 
    
    if (!is.null(file)) sink()
  }

  if (type=="tex") {
    if (!is.null(file)) {
      
      sink(file, append=FALSE)
      cat("%\\documentclass{article} \\begin{document}\n\n")
      
      cat("\\begin{table}[ht] \n")
      cat("\\centering \n")
      if (missing(caption) || is.null(caption))
        cat("\\caption{AutoRegressive Distributed Lag model (ARDL)} \n")
      else
        cat("\\caption{",caption,"} \n")
      
      cat("\\begin{tabular}{l} \n")
      cat("\\hline \n")
      cat( CaptionLR, "\n" ) 
      cat("\\end{tabular} \n") 
      
      cat("\\begin{tabular}{lrrrrl} \n")
      cat("\\hline \n")
      cat("& Estimate & Std.Err & Z value & Pr($>$z) & \\\\ \n") 
      cat("\\hline \n")

      cmat <- coeff_matrix( obj$coeff_lr, obj$coeff_lr_sd )
      k <- dim(cmat)[1]
      for (i in 1:k) {
        cat( names(obj$coeff_lr)[i]    ," & ",
             sprintf("%4.4f",cmat[i,1])," & ",
             sprintf("%4.4f",cmat[i,2])," & ",
             sprintf("%4.4f",cmat[i,3])," & ",
             sprintf("%4.4f",cmat[i,4])," & ",stars(cmat[i,4])," \\\\ \n")
      }
      
      cat("\\hline \n")
      cat("\\end{tabular} \n")
      cat(" ")
      cat("\\begin{tabular}{l} \n")
      cat( CaptionSR, "\n" ) 
      cat("\\end{tabular} \n") 

      cat(" ")
      cat("\\begin{tabular}{lrrrrl} \n")
      cat("\\hline \n")
      cat("& Estimate & Std.Err & Z value & Pr($>$z) & \\\\ \n") 
      cat("\\hline \n")
      
      cmat <- coeff_matrix( obj$coeff_sr, obj$coeff_sr_sd )
      k <- dim(cmat)[1]
      for (i in 1:k) {
        cat( names(obj$coeff_sr)[i]    ," & ",
             sprintf("%4.4f",cmat[i,1])," & ",
             sprintf("%4.4f",cmat[i,2])," & ",
             sprintf("%4.4f",cmat[i,3])," & ",
             sprintf("%4.4f",cmat[i,4])," & ",stars(cmat[i,4])," \\\\ \n")
      }
      
      cat("\\hline \n")
      cat("\\end{tabular} \n")
  
      cat("\\begin{tabular}{l} \n") 
      cat("Significance: '***' 0.1\\% '**' 1\\%  '*' 5\\%  '.' 10\\% \n")
      cat("\\end{tabular} \n")
      
      cat("\\end{table} \n")
      cat("\n % \\end{document}")
      sink()
    }
  }
}

stars <- function (pval){
  stars <- ""
  if (!is.na(pval)) {
    # Significance: ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
    if (pval>0.05  && pval<= 0.10)   stars <- "."
    if (pval>0.01  && pval<= 0.05)   stars <- "*"
    if (pval>0.001 && pval<= 0.01)   stars <- "**"  
    if (pval<= 0.001)                stars <- "***"
  }
  stars 
}

coeff_matrix <- function( coeff, sd ){
  # builds a matrix with 4 columns to call printCoefmat()
  cmat <- cbind( coeff, sd, coeff/sd, 2*(1-pnorm(abs(coeff/sd))) ) #2*pnorm(-coeff/sd )
  cmat <- matrix( cmat, ncol=4 )
  colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
  rownames(cmat) <- names(coeff)
  #printCoefmat( cmat, digits=4, signif.stars=TRUE, P.values = TRUE )
  cmat 
}
