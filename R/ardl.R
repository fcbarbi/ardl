#'
#' @name ardl
#' @title Estimate an Auto Regressive Distributed Lag (ARDL) model.
#' @description Generates an Auto Regressive Distributed Lag (ARDL) model based on the number of lags of y and x.
#' @details Saves an \code{ardl} object with all results to be \code{print()}, \code{summary()} or \code{coint()}.
#' 
#' @param formula Formula as in \code{y ~ x1 + x2 | x3 } where \code{x1} and \code{x2} may be lagged to different orders according to the vector \code{xlag=c(1,2)}. Note that \code{x3} is fixed so it is not lagged, it is generally used for dummies.  
#' @param data A dataframe or time referenced object with data in columns.
#' @param subset (optional) Filter rows from the dataframe. Defaults to \code{NULL}.
#' @param case (optional) Defaults to 3 (intercept + no trend). We use the same table of cases as \cite{Pesaran2001} the options are: 
#' \code{case = 1} model with no intercept, no trend; 
#' \code{case = 3} model with unrestricted intercept, no trend; 
#' \code{case = 5} model with unrestricted intercept, unrestricted trend.
#' @param ylag (optional) Defaults to 1. Maximum lag of the dependent variable. Must be 1 or more.
#' @param xlag (optional) Defaults to 1. Vector of the maximum lag for each of the lagging component. Must be 0 or more. Note that if omitted the regressors have lag 1.
#' @param quiet (optional) Defaults to \code{FALSE}. If set to \code{TRUE} the routine genaerates no output, as required when called by \code{auto.ardl()}.
#' @return An object of class \code{ardl}.
#' 
#' @export ardl 
#' @importFrom dynlm dynlm
#' @importFrom zoo zooreg
#' @importFrom zoo merge.zoo
#' @importFrom zoo index2char
#' @importFrom zoo na.trim 
# @importFrom zoo start 
# @importFrom zoo end 
# @importFrom("stats", "AIC", "BIC", "as.formula", "coefficients", "formula", "frequency", "logLik", "pnorm", "printCoefmat", "residuals", "start", "terms.formula", "var", "vcov")
#' @seealso \code{\link{auto.ardl}}
#' 
#' @references 
#' 
#'          Pesaran, M.H. and Shin, Yongcheol (1999) An Autoregressive Distributed-Lag Modelling Approach to Cointegration Analysis. \emph{Econometrics and Economic Theory in the 20th Century}. Cambridge University Press.
#'    
#'          Pesaran, M.H. and Shin, Yongcheol and Smith, Richard (2001) Bounds testing approaches to the analysis of level relationships. \emph{Journal of Applied Econometrics}.
#'
#'          Hassler, Uwe and Wolters, Jürgen (2005) Autoregressive distributed lag models and cointegration. Discussion Papers.
#' 
#' @examples
#'  
#' data(br_month)
#' m1 <- ardl( mpr~cpi+reer, data=br_month )
#' m2 <- ardl( mpr~cpi+reer|d_lula, data=br_month, ylag=2, xlag=c(1,1), case=5, quiet=TRUE )  
#' m3 <- ardl( mpr~cpi+prod+reer|d_lula, data=br_month, ylag=2, xlag=c(1,2,2), case=1 )
#
ardl <- function( formula, data, subset=NULL, ylag=1, xlag=1, case=3, quiet=FALSE ){  
  #DEBUG <- TRUE  
  
  # if parameters are passed by position, name them 
  #ardl.call <- match.call(expand.dots = FALSE)
  #m <- match(c("formula", "data", "subset", "ylag", "xlag", "case", "quiet"), names(ardl.model), 0)
  #print(m)
  #ardl.call[]
  
  ## validations
  if (!inherits(formula,"formula")) stop("First argument must be a formula (not a string...)")

  if (missing(data)) stop("Data must be supplied")
  if (!is.data.frame(data) && !is.matrix(data)) stop("Data must be data frame or matrix.")

  #if (!inherits(data,"zoo") && !inherits(data,"ts")) data <- ts( data, freq=1 )
  if (inherits(data,"zoo") || inherits(data,"ts")) {
    data_start <- start(data[, 1])     
    data_freq  <- frequency(data[, 1])
  } else {
    data_start <- data_freq <- 1
  }
  #print(data_start)
  #print(data_freq)
  
  if (quiet==FALSE) cat("\nDataset adjustment to the common sample of all regressors:\n")
  if (quiet==FALSE) {
    x <- data[,1] 
    cat( "Original dataset from ", zoo::index2char(zoo::index(x)[1], frequency(x)),
         "to", zoo::index2char(zoo::index(x)[length(x)], frequency(x)),"\n" )
  }
  
  # convert to df so diagnostic functions work better 
  if (!is.data.frame(data)) data <- as.data.frame(data); #print("converted to data.frame") }  
  
  if (!is.numeric(ylag)|!is.numeric(xlag)) stop("ylag and xlag must be numeric")
  if (ylag<0) stop("ylag must be 0 or more") else ylag <- floor(ylag)
  if (any(xlag<0)) stop("xlag must be an integer equal of above 0")
  xlag <- floor(xlag) # ensure all lags are integers
  
  if (is.na(match( case, c(1,3,5) ))) stop("case must be 1, 3 or 5")
      
  ## This function has 3 parts: build the formula, calculate LR coeffs, estimate SR coeff  
  ##
  ## ##################################################################
  ## Part 1: build the formula to be estimated by dynlm()
  ## The original formula must be decomposed and rearranged into:
  ## formula <- prefix + core + suffix 
  ## prefix has de dependent variable lagged and constant + trend terms (when required)
  ## suffix are the terms that do not lag ("fixed terms") such as dummies 
  ## To test ARDL with 1 lag, intercept and trend for "y ~ x2+x3|x1"
  ## the formula is rewritten as "y ~ +1+trend(y) + L(y)+x2+L(x2)+x3+L(x3) + x1"
  
  atoms <- ardl:::formulaExplode( formula )
  lhs           <- atoms[[1]]
  core_split    <- atoms[[2]]
  suffix_split  <- atoms[[3]]
  
  K  <- length(core_split)   ## number of variable lag regressors 
  KX <- length(suffix_split) ## number of fixed regressors 

  if (K<1) stop("At least one regressor X must be supplied in y~X ")
  
  ## select only the necessary columns to build a new dataset 
  columns <- c(lhs,core_split,suffix_split)
  #print(columns)
  
  ## check that all regressors are in the dataset 
  badcol <-NULL
  for (cc in columns) 
    if (!(cc %in% colnames(data))) badcol <- c(badcol,cc)
  if (length(badcol)>0)
   stop( cat("The column(s): ",badcol," was(were) not found in the dataset. Please check before proceeding.") )
  
  ## restrict dataset to regressors required by formula 
  data_s <- data[,columns] # selected data 
  
  ## check if selected data is available (not all NA), is numeric and varies enough to be usable   
  exclude <- NULL 
  for (i in 1:dim(data_s)[2]) 
    if (all(is.na(data_s[,i]))) {
      cat("No data available in column",colnames(data_s)[i],"(all NA) \n" )
      exclude <- c(exclude,i)
    } else if (!is.numeric(data_s[,i])) {
      cat("Data in column",colnames(data_s)[i],"is not numeric\n")
      exclude <- c(exclude,i)
    } else if (var(data_s[,i],na.rm = TRUE)<1e-6) {  
      cat("Data in column",colnames(data_s)[i],"seems to be constant\n")
      exclude <- c(exclude,i)
    }  
  if (length(exclude)>1) 
    stop(paste("Some column(s) in data was(were) not acceptable. Adjust dataset or formula before proceeding."))
  
  ## adjust dataset to eliminate NA at the begining and/or end of the sample 
  data_s <- zoo::zooreg( data_s, start=data_start, frequency=data_freq )
  data <- zoo::na.trim(data_s)  
  if (any(is.na(data))) 
    warning("dataset still contains NA's: consider doing some inputation before proceeding.") 
  ## data <- AdjustDataset( data, lag=max(ymax,xmax), lead=0 ) # compare models with the same nbr of obs 
  
  if (quiet==FALSE) {
    x <- data[,1] 
    cat("Adjusted dataset from ",zoo::index2char(zoo::index(x)[1], frequency(x)),"to",
        zoo::index2char(zoo::index(x)[length(x)], frequency(x)),"\n" )
  }
  #print(class(data))
  
  for (name in colnames(data)) assign( name, data[,name] ) 
  T <- length(get(core_split[1])) # n.obs in time

  ## adjust xlag if smaller than K
  if (K>length(xlag)) {
   xlag <- c(xlag,rep( 1, K-length(xlag) ))
   if (quiet==FALSE) warning("xlag was adjusted")
  }
  
  ## case using PSS classification, so far only unrestricted cases estimated by OLS
  prefix <- ""
  bInt <- bTrend <- NA
  if (case==1) { ## no intercept, no trend
    prefix <- "-1"
    bInt <- 0
    bTrend <- 0 
  }
  if (case==3) { ## unrestricted intercept, no trend
    prefix <- "+1"
    bInt <- 1
    bTrend <- 0
  }  
  if (case==5) { ## unrestricted intercept, unrestricted trend
    prefix <- paste0("+1+trend(",lhs,")")
    bInt <- 1
    bTrend <- 1
  }
  
  ## To map each regressor of the canonical form into the extended form we use the coeff_map vector 
  ## where coeff_map is "0" for the lhs (y) and "1" for the first element of rhs and so on until K+KX.
  ## Ex: The canononical form y~x1+x2|x3 with ylag=2 and xlag=(3,2) generates the extended form  
  ## y~+1+L(y,1)+L(y,2)+x1+L(x1,1)+L(x1,2)+L(x1,3)+x2+L(x2,1)+L(x2,2)+x3 with mapping 
  ## coeff_map == "-1" "0" "0" "1" "1" "1" "1" "2" "2" "2" "3"
  ## Note that Intercept and Trend are marked with "-1" as a placeholder only. 

  coeff_map <- NULL
  if (bInt) coeff_map <- "-1"  ## -1 is just a placeholder 
  if (bTrend) coeff_map <- c(coeff_map,"-1") 

  for (i in 1:ylag) { 
    prefix <- paste0( prefix,"+L(",lhs,",",i,")" )
    coeff_map <- c(coeff_map,"0")
  }
  
  ## build core string with the proper lags for each variable   
  core <- ""
  for (i in 1:K) {
   core <- paste0(core,core_split[i]) 
   coeff_map <- c(coeff_map,i)
   if (xlag[i]>0)
    for (j in 1:xlag[i]) {
      if (j==1) core <- paste0(core,"+")
      core <- paste0(core,"L(",core_split[i],",",j,")")
      if (j<xlag[i]) core <- paste0(core,"+")
      coeff_map <- c(coeff_map,i)
    }
   if (i<K) core <- paste0(core,"+")
  }

  suffix <- NULL 
  if (KX>0)
  for (i in 1:KX) {
    suffix <- paste0(suffix,suffix_split[i])
    if (i<KX) suffix <- paste0(suffix,"+")
    coeff_map <- c(coeff_map,i+K)
  }
  #print("coeff_map");print(coeff_map)
  
  if (!is.null(suffix)){
    fm <- paste(lhs,"~",prefix,"+",core,"+",suffix)
  } else { 
    fm <- paste(lhs,"~",prefix,"+",core)
  }
  #print(fm)

  # force data into ts because dynlm() works best with it 
  if (!inherits(data,"ts")) data <- as.ts( data, start=data_start, frequency=data_freq ); #print("converted to ts") }
  #write.csv(data,file="230_data.csv")
  
  res <- dynlm::dynlm( formula(fm), data=data, subset=subset )

  res$case <- case 
  res$lhs <- lhs 
  res$variableTerms <- core_split
  res$fixedTerms <- suffix_split
  res$ylag <- ylag 
  res$xlag <- xlag 
  res$data <- data 
  x <- data[,1]
  res$start_adj <- zoo::index2char(zoo::index(x)[1], frequency(x))
  res$end_adj   <- zoo::index2char(zoo::index(x)[length(x)], frequency(x))
  
  ## ##################################################################
  ## part 2: estimate LR coeffs
  ## generate Long Run coefficient estimates and SE using delta
  ##
  ## y_t = theta y_t-1 + beta_1 x_t + beta_2 x_t-1 + eps_t 
  ## y_t = coeff_lr x_t where coeff_lr = (beta_1 + beta_2)/(1 - theta)
  
  coeff_lr <- rep(NA,K+KX)
  
  ## terms associated to the lagged dependent variable   
  theta <- sum( res$coefficients[ which( coeff_map == 0) ] )
  for (i in 1:K) { ## terms that can be lagged 
    temp <- sum( res$coefficients[ which( coeff_map == i) ] )
    coeff_lr[i] <- temp/(1-theta) 
  }
  if (KX>0) ## fixed (not laged) terms
    for (i in (K+1):(K+KX)) {
      temp <- res$coefficients[ which( coeff_map == i) ] 
      coeff_lr[i] <- temp/(1-theta) 
    }
  
  badcoeff <- NULL
  if (any(is.na(coeff_lr))) badcoeff <- which(is.na(coeff_lr))
  if (length(badcoeff)>0) {
    #print(colnames(coeff_lr[badcoeff]))  # paste("Could not estimate LR coefficients of",coeff_lr[badcoeff]))
    stop("Some Long Run coefficients could not be estimated. Check model before proceeding.")
  }
  
  attr(coeff_lr,"names") <- c(core_split,suffix_split)
  res$coeff_lr <- coeff_lr
  #print(coeff_lr)

  coeff_lr_sd <- rep(NA,length(coeff_lr))  

  ## Delta method (see eq.2.20 in Pesaran and Shin (1999))
  ## do not include Intercept or Trend in the LR vector of coefficients 
  removeNA <- TRUE  # should it be set by the user?
  demean <- function(x)   { x-mean(x, na.rm = removeNA) }    
  y <- get(lhs)
  sigma_u <- var(residuals(res)) 
  #if (DEBUG) print("sigma_u");print(sigma_u)
  for (i in 1:(K+KX)) { 
    x <- get(attr(coeff_lr, "names")[i])
    DT <- sum(demean(x)^2, na.rm = removeNA) * sum(demean(y)^2) - sum(demean(y)*demean(x), na.rm = removeNA)^2
    temp1 <- (sigma_u/(1 - theta)^2) * 1/DT
    temp2 <- -sum(demean(x) * demean(y), na.rm = removeNA)
    temp2 <- matrix( c( sum(demean(y)^2, na.rm = removeNA), temp2, temp2, sum(demean(x)^2, na.rm = removeNA) ), ncol = 2 )
    temp3 <- matrix( c(1, coeff_lr[i]), ncol = 2) %*% temp2 %*% matrix(c(1, coeff_lr[i]), ncol = 1 )
    coeff_lr_sd[i] <- sqrt( temp1*temp3 )
  }  
  res$coeff_lr_sd <- coeff_lr_sd 
  
  ## ##################################################################
  ## part 3: estimate the model again with variables in first diff 
  ## and controlling for the cointegration relation to get the SR coeffs  

  X <- matrix( rep(NA,(K+KX)*T), ncol=K+KX ) 
  for (i in 1:K) X[,i] <- get(core_split[i])
  if (KX>0) for (i in 1:KX) X[,i+K] <- get(suffix_split[i])

  coeff_lr <- matrix( coeff_lr, ncol=1 )  
  #print("lhs");print(lhs)
  #print("coeff_lr");print(coeff_lr)
  #print("X");print(X)
  coint <- get(lhs) - X %*% coeff_lr  ## control for the LR relation   
  #print(coint)
  coint <- ts( coint, start=data_start, freq=data_freq ) 
  res$coint <- coint
  colnames <- colnames(data)
  data <- cbind(data,coint)
  colnames(data) <- c(colnames,"coint")
  
  ## build list of regressors to estimate the short term coefficients 
  core_str <- ""
  for (i in 1:K) {
    core_str <- paste0(core_str,"d(",core_split[i],")")
    if (i<K) core_str <- paste0(core_str,"+")
  } 
  ## do NOT diff the fixed regressors that are usually dummies 
  if (!is.null(suffix)) {
    fm_sr <- paste0("d(",lhs,") ~ L(d(",lhs,")) +",core_str," + ", suffix," + L(coint)")
  } else {
    fm_sr <- paste0("d(",lhs,") ~ L(d(",lhs,")) +",core_str," + L(coint)")
  }  
  #print(fm_sr)
  #write.csv(data,file="324_data.csv")
  res_sr <- dynlm::dynlm( formula(fm_sr), data=data, subset=subset )

  res$coeff_sr <- coefficients(res_sr)  # copies data and attr "names"
  res$coeff_sr_sd <- sqrt(diag(vcov(res_sr)))
  attr(res$coeff_sr_sd,"names") <- NULL
  
  res$sr_fstatistic1 <- summary(res_sr)$fstatistic[1]
  names(res$sr_fstatistic1) <- NULL
  res$sr_fstatistic2 <- summary(res_sr)$fstatistic[2]
  names(res$sr_fstatistic2) <- NULL
  res$sr_fstatistic3 <- summary(res_sr)$fstatistic[3]
  names(res$sr_fstatistic3) <- NULL
  
  #res$sr_residuals <- residuals(res_sr)
  res$sr_var_residuals <- var(residuals(res_sr))
  
  class(res) <- c("ardl",class(res))  
  #if (quiet==FALSE) print(res) 
  return(res) 
}

## eof 


