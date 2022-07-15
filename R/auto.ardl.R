#' 
#' @name auto.ardl
#' @title auto.ardl
#' 
#' @description Searchs for the best ARDL model, given maximum lag for the explained variable \code{y} and regressors \code{x}.
#' @details This function selects the models to test, the actual estimation is done by \code{ardl()}.
#' 
#' @param formula Formula as in \code{ y ~ x1 + x2 | x3 } where \code{x1} and \code{x2} are tested for different lags up to \code{xmax}. Note that \code{x3} is "fixed" so it is not lagged.  
#' @param data A dataframe or time referenced object with data in columns.
#' @param subset (optional) Filter rows from the dataframe. Defaults to \code{NULL}.
#' @param ymax (optional) Maximum lag of the dependent variable \code{y}. Defaults to 4.
#' @param xmax (optional) Maximum lag of the explaining variable(s) \code{x}. Defaults to 4.
#' @param ic (optional) Information criteria used for selection. Can be \code{"aic","bic","r2","ll"}. Defaults to "bic".
#' @param verbose (optional) Lists all the models tested. Defaults to FALSE.
#' @param case (optional) Number of the case according to PSS (2001) to chose which table to use.
#' @return An object of class \code{ardl}.
#' 
#' @export auto.ardl 
#' @seealso \code{\link{ardl}}
#'
#' @examples
#' 
#' data(br_month)
#' 
#' # show the selection process with verbose=TRUE
#' taylor0 <- auto.ardl( mpr~cpi+prod+reer, data=br_month, ymax=4, xmax=c(4,4,4), verbose=TRUE )
#' summary( taylor0 )
#' 
#' # note the differences by varying the case (intercept and trend) specification 
#' phillips1 <- auto.ardl( cpi~mpr+prod+reer|d_lula, data=br_month, ymax=4, xmax=c(4,4,4), case=1 )
#' phillips2 <- auto.ardl( cpi~mpr+prod+reer|d_lula, data=br_month, ymax=4, xmax=c(4,4,4), case=3 )
#' phillips3 <- auto.ardl( cpi~mpr+prod+reer|d_lula, data=br_month, ymax=4, xmax=c(4,4,4), case=5 )
#' BIC(phillips1) # 86.42611 best!
#' BIC(phillips2) # 91.0541
#' BIC(phillips3) # 92.84055
#' 
#' # change the information criteria 
#' phillips4 <- auto.ardl( cpi~mpr+prod+reer|d_lula, data=br_month, ymax=4, xmax=c(4,4,4), ic="aic" )
#' phillips5 <- auto.ardl( cpi~mpr+prod+reer|d_lula, data=br_month, ymax=4, xmax=c(4,4,4), ic="r2" )
#' phillips6 <- auto.ardl( cpi~mpr+prod+reer|d_lula, data=br_month, ymax=4, xmax=c(4,4,4), ic="bic" )

## if the "canonical" equation is y ~ x1+x2|x3 considering case 5 with lags for y = 1 
## and x = c(1,2) the "expanded" equation is y ~ L(y)+trend(y) + x1+x2+L(x2,1)+L(x2,2) + x3

auto.ardl <- function( formula, data, subset=NULL, ymax=4, xmax=4, case=3, ic="bic", verbose=FALSE ) {
  if (missing(formula)) stop ("Formula must be supplied.") # !is.formula(fm)
  if (!inherits(formula,"formula")) stop("First argument must be a formula (not a string...)")
  
  match.arg( ic, c("aic","bic","r2","ll") )
  infoCrit <- ic 
  
  if (missing(data)) stop("data must be supplied") # is.null(data)
  T <- dim(data)[1]

  if (ymax<0 || ymax>=T) stop('Check ymax: it should be between 1 and nbr-1 observations')
  if (any(xmax < 0 | xmax >= T)) stop('Check xmax: it should be between 1 and nbr-1 observations')
  
  if (is.na(match( case, c(1,3,5) ))) stop("case must be 1, 3 or 5")

  K  <- length(formulaExplode(formula)[[2]]) ## nbr of variable regressors

  ## baseline is no lag
  ybest <- 0
  xbest <- rep(0,K)
  
  if (verbose) 
    cat("\nARDL automatic model selection using",infoCrit,"with ymax=",ymax,"and xmax=",xmax,"\n")
  
  # find best fit for y lags and save it in ybest  
  for (i in 1:ymax) {
    mod <- ardl( formula, data=data, subset=subset, ylag=i, xlag=rep(0,K), case=case, quiet=TRUE )
    if (verbose) cat("Model",deparse(attr(mod$model,"terms")),"has",infoCrit,"=",valIC(mod,infoCrit),"\n")
    if (ybest==0) ic_best <- valIC(mod,infoCrit)
    if (isBetterModel(mod,infoCrit,ic_best)) { 
      ybest <- i
      ic_best <- valIC(mod,infoCrit)
    }  
  }
  #if (DEBUG) cat("ybest =",ybest,"\n")

  for (i in 1:K) {
    xlag <- xbest
    ## baseline for regressor i is no lag 
    mod <- ardl( formula, data=data, subset=subset, ylag=ybest, xlag=xlag, case=case, quiet=TRUE )  
    ic_best <- valIC(mod,infoCrit)
    for (j in 1:xmax[i]) {
      xlag <- xbest
      xlag[i] <- j 
      mod <- ardl( formula, data=data, subset=subset, ylag=ybest, xlag=xlag, case=case, quiet=TRUE )
      if (verbose) cat("Model",deparse(attr(mod$model,"terms")),"has",infoCrit,"=",valIC(mod,infoCrit),"\n")
      if (isBetterModel(mod,infoCrit,ic_best)) { 
        xbest[i] <- j
        ic_best <- valIC(mod,infoCrit)
      } 
    }  
  }
  
  mod <- ardl( formula, data=data, subset=subset, ylag=ybest, xlag=xbest, case=case, quiet=TRUE )
  if (verbose) cat("Best model is",deparse(attr(mod$model,"terms")),"chosen by",infoCrit,"=",valIC(mod,infoCrit),"\n")
  mod 
}


#eof
