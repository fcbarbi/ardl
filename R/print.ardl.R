#'
#' @name print.ardl
#' @title Print ARDL model
#' @description Generics to print the S3 class ardl (Auto Regressive Distributed Lag Model).
#' @details Calls \code{print.dynlm()}.
#' 
#' @param x A class \code{ardl} object. 
#' @param ... ignored
#'  
#' @export 
#  (deprecated) @S3method print ardl
#' 

print.ardl <- function( x, ... ) {
  
  if (!inherits(x,"ardl")) stop("Class of the argument must be ardl.")

  cat("\nAutoRegressive Distributed Lag model\n")  #  sprintf( "ARDL(%i,%i,%i)",ylag,xlag )
  NextMethod() # calls print.dynlm()
  
  cat("\n Long-term coefficients:\n")   
  print( x$coeff_lr )

  cat("\n\n Short-term coefficients\n")
  print( x$coeff_sr )
  
}

