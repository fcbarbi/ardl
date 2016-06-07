#'
#' @name summary.ardl
#' @description Generic to detail the S3 class ardl (Auto Regressive Distributed Lag).
#' 
#' @details Bypass summary.dynlm()
#' 
#' @param ardlobj A class \code{ardl} object. 
#' @return (nothing)
#'  
#' @export summary.ardl
#' @example
#' m1 <- ardl( y~x2+x3|x1, ylag=2, xlag=c(1,2), case=1 )
#' summary(m1)

summary.ardl <- function( obj ){ 
  if (!inherits(obj,"ardl")) stop("Class of the argument must be ardl.")
  
  ## call dynlm method
  rval <- NextMethod()
  
  # Add Long Run (LR) coeffs
  
  class(rval) <- c("summary.ardl", class(rval))
  return(rval)
}
