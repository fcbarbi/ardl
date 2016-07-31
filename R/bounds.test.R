#' 
#' @name bounds.test
#' @title Performs PSS (2001) bounds test for I(0) and I(1) regressors.
#' @description Calculates the Pesaran, Shin and Smith (2001) statistics and list the critical values.
#' @details Presents the bounds test for I(0) or I(1) regressors with critical values from PSS (2001). 
#' #TODO: include critical values from Narayan (2005) paper for small samples. 
#' @param obj An ardl object to be tested.
# @param tables (optional) Select the tables with critical values, can be \code{"pss"} or \code{"narayan"}. Defaults to \code{"pss"}
# @param type (optional) Select results format that can be \code{c("txt","tex")}. Defaults to \code{"tex"}. 
# @param file (optional) Filename to save results. Defaults to \code{NULL}.
# @param append (optional) If \code{TRUE} appends to the file. Defaults to \code{FALSE}.
#' @return An object of class \code{ardl}.
#' @export
# @importFrom lmtest waldtest
#'  
#' @seealso \code{\link{coint}}  
#' @references 
#' 
#'          Pesaran, M.H. and Shin, Yongcheol and Smith, Richard (2001) Bounds testing approaches to the analysis of level relationships. \emph{Journal of Applied Econometrics}.
#
#          Narayan, P.K. (2005) Reformulating Critical Values for the Bounds Fstatistics Approach to Cointegration: An Application to the Tourism Demand Model for Fiji. Monash University. Department of Economics Discussion Papers.
#' 
#' @examples 
#' data(br_month)
#' m1 <- ardl( mpr~cpi+prod+ner|d_lula, data=br_month, ylag=2, xlag=c(0,0,0), case=3 )
#' bounds.test( m1 )
#' 

## --------------------------------------------------------
bounds.test <- function( obj ) {  # , tables="pss", append=FALSE, file=NULL ){
  
#DEBUG <- TRUE  
  tables="pss" 
  append=FALSE
  file=NULL
  
  val <- NULL		
  val <- rbind(val,	c(2.44,3.28,3.15,4.11,3.88,4.92,4.81,6.02)	)
  val <- rbind(val,	c(2.17,3.19,2.72,3.83,3.22,4.5,3.88,5.3)	)
  val <- rbind(val,	c(2.01,3.1,2.45,3.63,2.87,4.16,3.42,4.84)	)
  val <- rbind(val,	c(1.9,3.01,2.26,3.48,2.62,3.9,3.07,4.44)	)
  val <- rbind(val,	c(1.81,2.93,2.14,3.34,2.44,3.71,2.82,4.21)	)
  val <- rbind(val,	c(1.75,2.87,2.04,3.24,2.32,3.59,2.66,4.05)	)
  val <- rbind(val,	c(1.7,2.83,1.97,3.18,2.22,3.49,2.54,3.91)	)
  val <- rbind(val,	c(1.66,2.79,1.91,3.11,2.15,3.4,2.45,3.79)	)
  val <- rbind(val,	c(1.63,2.75,1.86,3.05,2.08,3.33,2.34,3.68)	)
  val <- rbind(val,	c(1.6,2.72,1.82,2.99,2.02,3.27,2.26,3.6)	)
  case1 <- data.frame( K=1:10, value=matrix(val,nrow=10,ncol=8))  
  colnames(case1)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")

  val <- NULL		
  val <- rbind(val,	c(3.02,3.51,3.62,4.16,4.18,4.79,4.94,5.58)	)
  val <- rbind(val,	c(2.63,3.35,3.1,3.87,3.55,4.38,4.13,5)	)
  val <- rbind(val,	c(2.37,3.2,2.79,3.67,3.15,4.08,3.65,4.66)	)
  val <- rbind(val,	c(2.2,3.09,2.56,3.49,2.88,3.87,3.29,4.37)	)
  val <- rbind(val,	c(2.08,3,2.39,3.38,2.7,3.73,3.06,4.15)	)
  val <- rbind(val,	c(1.99,2.94,2.27,3.28,2.55,3.61,2.88,3.99)	)
  val <- rbind(val,	c(1.92,2.89,2.17,3.21,2.43,3.51,2.73,3.9)	)
  val <- rbind(val,	c(1.85,2.85,2.11,3.15,2.33,3.42,2.62,3.77)	)
  val <- rbind(val,	c(1.8,2.8,2.04,3.08,2.24,3.35,2.5,3.68)	)
  val <- rbind(val,	c(1.76,2.77,1.98,3.04,2.18,3.28,2.41,3.61)	)
  case2 <- data.frame( K=1:10, value=matrix(val,nrow=10,ncol=8))  
  colnames(case2)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")
  
  val <- NULL	
  val <- rbind(val,	c(4.04,4.78,4.94,5.73,5.77,6.68,6.84,7.84)	)
  val <- rbind(val,	c(3.17,4.14,3.79,4.85,4.41,5.52,5.15,6.36)	)
  val <- rbind(val,	c(2.72,3.77,3.23,4.35,3.69,4.89,4.29,5.61)	)
  val <- rbind(val,	c(2.45,3.52,2.86,4.01,3.25,4.49,3.74,5.06)	)
  val <- rbind(val,	c(2.26,3.35,2.62,3.79,2.96,4.18,3.41,4.68)	)
  val <- rbind(val,	c(2.12,3.23,2.45,3.61,2.75,3.99,3.15,4.43)	)
  val <- rbind(val,	c(2.03,3.13,2.32,3.5,2.6,3.84,2.96,4.26)	)
  val <- rbind(val,	c(1.95,3.06,2.22,3.39,2.48,3.7,2.79,4.1)	)
  val <- rbind(val,	c(1.88,2.99,2.14,3.3,2.37,3.6,2.65,3.97)	)
  val <- rbind(val,	c(1.83,2.94,2.06,3.24,2.28,3.5,2.54,3.86)	)
  case3 <- data.frame( K=1:10, value=matrix(val,nrow=10,ncol=8))  
  colnames(case3)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")
  
  val <- NULL		
  val <- rbind(val,	c(4.05,4.49,4.68,5.15,5.3,5.83,6.1,6.73)	)
  val <- rbind(val,	c(3.38,4.02,3.88,4.61,4.37,5.16,4.99,5.85)	)
  val <- rbind(val,	c(2.97,3.74,3.38,4.23,3.8,4.68,4.3,5.23)	)
  val <- rbind(val,	c(2.68,3.53,3.05,3.97,3.4,4.36,3.81,4.92)	)
  val <- rbind(val,	c(2.49,3.38,2.81,3.76,3.11,4.13,3.5,4.63)	)
  val <- rbind(val,	c(2.33,3.25,2.63,3.62,2.9,3.94,3.27,4.39)	)
  val <- rbind(val,	c(2.22,3.17,2.5,3.5,2.76,3.81,3.07,4.23)	)
  val <- rbind(val,	c(2.13,3.09,2.38,3.41,2.62,3.7,2.93,4.06)	)
  val <- rbind(val,	c(2.05,3.02,2.3,3.33,2.52,3.6,2.79,3.93)	)
  val <- rbind(val,	c(1.98,2.97,2.21,3.25,2.42,3.52,2.68,3.84)	)
  case4 <- data.frame( K=1:10, value=matrix(val,nrow=10,ncol=8))  
  colnames(case4)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")
  
  val <- NULL		
  val <- rbind(val,	c(5.59,6.26,6.56,7.3,7.46,8.27,8.74,9.63)	)
  val <- rbind(val,	c(4.19,5.06,4.87,5.85,5.49,6.59,6.34,7.52)	)
  val <- rbind(val,	c(3.47,4.45,4.01,5.07,4.52,5.62,5.17,6.36)	)
  val <- rbind(val,	c(3.03,4.06,3.47,4.57,3.89,5.07,4.4,5.72)	)
  val <- rbind(val,	c(2.75,3.79,3.12,4.25,3.47,4.67,3.93,5.23)	)
  val <- rbind(val,	c(2.53,3.59,2.87,4,3.19,4.38,3.6,4.9)	)
  val <- rbind(val,	c(2.38,3.45,2.69,3.83,2.98,4.16,3.34,4.63)	)
  val <- rbind(val,	c(2.26,3.34,2.55,3.68,2.82,4.02,3.15,4.43)	)
  val <- rbind(val,	c(2.16,3.24,2.43,3.56,2.67,3.87,2.97,4.24)	)
  val <- rbind(val,	c(2.07,3.16,2.33,3.46,2.56,3.76,2.84,4.1)	)
  case5 <- data.frame( K=1:10, value=matrix(val,nrow=10,ncol=8))  
  colnames(case5)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")

## -------------------------------------------------
# validate 
match.arg( tables,c("pss") )  # c("pss","narayan")
if (tables=="pss")
  table <- switch( obj$case, case1, case2, case3, case4, case5 )
#else 
#  table <- switch( case, n_case1, n_case2, n_case3, n_case4, n_case5 )

if (!inherits(obj,"ardl")) stop("Class of the argument must be ardl.")

K <- length(obj$variableTerms)
if (K<1 || K>10) stop("Number of regressors must be between 1 and 10")

case_desc <- switch(obj$case, "no intercept, no trend", 
                              "restricted intercert, no trend (not supported)",
                              "unrestricted intercert, no trend",
                              "unrestricted intercept, restricted trend (not supported)",
                              "unrestricted intercept, unrestricted trend")

# TODO: test sample size, recommend narayan if less than 80 obs ?

if (!is.null(file) && is.character(file)) sink(file)

cat("\nBounds Test:\n")
cat(deparse(formula(obj)),"\n")
cat("\nPSS case",obj$case," (",case_desc,")")
cat("\nRegressors (K)",K," \n\n")

## document the null hypothesis or NO LR relation 
cat("d(y_t) = alpha + pi (y_t-1,x_t)' + phi (d(y_t),d(x_t))' + epsilon_t \n")
cat("Null hypothesis (H0): No long-run relation exist, ie H0:pi=0\n\n")

cat(sprintf("         I(0)   I(1)\n"))
cat(sprintf("  10%%   %3.2f  %3.2f\n", table[K,"90.0"],table[K,"90.1"] ))
cat(sprintf("   5%%   %3.2f  %3.2f\n", table[K,"95.0"],table[K,"95.1"] ))    
cat(sprintf(" 2.5%%   %3.2f  %3.2f\n", table[K,"97.0"],table[K,"97.1"] ))
cat(sprintf("   1%%   %3.2f  %3.2f\n", table[K,"99.0"],table[K,"99.1"] ))

# F = Wald/(K+2) by PSS eq.21
#K <- length(obj$variableTerms)
#print(Fstat)
#p <- max(obj$ylag,obj$xlag)
#m <- (K+1)*(p+1)+1
#wuu <- obj$sr_var_residuals/(T-m)  
#wuu <- obj$sr_var_residuals/obj$sr_fstatistic3
#yx <- obj$model # matrix( c(get(obj$lhs),get(obj$variableTerms)), ncol=K+1 )
#T <- dim(yx)[1]
#P <- diag(T) # for (i in 1:T) P[i,i] <- T 
# P <- P - ...
# Wstat <- t(obj$coeff_sr) %*% t(yx) %*% P %*% yx  %*% obj$coeff_sr) 
# Wstat <- Wstat/wuu
# Fstat <- Wstat/(obj$sr_fstatistic2+2) # (K+2) 

## compare models with(m1) and without(m0) regressores in levels with the Waldtest 

core_split   <- obj$variableTerms
suffix_split <- obj$fixedTerms

lhs  <- obj$lhs
ylag <- obj$ylag
xlag <- obj$xlag
data <- obj$data 
case <- obj$case

K  <- length(core_split)
KX <- length(suffix_split)

## build the formula WITHOUT regressors in levels 
## this could be changed to a two estimations of the same model with constraints =0 on the levels
# fm0 <- d(mpr) ~ +1+L(d(mpr)) + d(cpi)+d(ner)+d(prod) + d_lula

if (case==1) fm0 <- paste0( "d(",lhs,") ~ -1+L(d(",lhs,")) ")
if (case==3) fm0 <- paste0( "d(",lhs,") ~ +1+L(d(",lhs,")) ")
if (case==5) fm0 <- paste0( "d(",lhs,") ~ +1+trend(",lhs,")+L(d(",lhs,")) ")
fm1 <- fm0 

for (i in 1:K)  fm0 <- paste0(fm0,"+d(",core_split[i],")")
if (KX>0) for (i in 1:KX) fm0 <- paste0(fm0,"+",suffix_split[i],"")
#print(fm0)

# fm1 <- d(mpr) ~ +1 + L(d(mpr)) + L(mpr, 1) + cpi+ner+prod+ d(cpi)+d(ner)+d(prod) + d_lula
for (i in 1:1)  fm1 <- paste0(fm1,"+L(",lhs,",",i,")") # 1:ylag
for (i in 1:K)  fm1 <- paste0(fm1,"+",core_split[i],"")
for (i in 1:K)  fm1 <- paste0(fm1,"+d(",core_split[i],")")
if (KX>0) for (i in 1:KX) fm1 <- paste0(fm1,"+",suffix_split[i],"")
#print(fm1)

cat("\nWald test to compare the models:\n")
cat(fm0,"\n")
cat(fm1,"\n")
m0 <- dynlm( formula( fm0 ), data=data )
m1 <- dynlm( formula( fm1 ), data=data )
m01 <- lmtest::waldtest(m0,m1)
Fstat <- m01$F[2]
cat("\nF statistic ",Fstat,"\n\n")  # espero algo entre 4 e 5 

## diagnostic 
diagn <- "Existence of a Long Term relation is"
if (Fstat>table[K,"95.1"]) 
  cat(diagn,"not rejected at 5%") 
if (Fstat<table[K,"95.0"]) 
  cat(diagn,"rejected at 5% (even assumming all regressors I(0))") 
if (Fstat<=table[K,"95.1"] && Fstat>=table[K,"95.0"]) 
  cat(diagn,"rejected at 5% with I(1) regressors but not with I(0) regressors ") 

#cat("\n\n Long-term coefficients:\n")   #TODO improve layout 
#print(obj$coeff_lr[1:K])
#print(obj$coeff_lr_sd)
#cat("AR test:")

if (!is.null(file) && is.character(file)) sink()

}


#eof
