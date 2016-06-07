
## ------------------------------------------------
## utilities.R 
## Utility functions not exported, the functions are:
##
## dataframe <- AdjustDataset( dataframe, lag=0, lead=0 )
## list <- formulaExplode( formula )
## boolean <- isBetterModel( mod, ic, ic_best )
## float <- valIC( mod, ic ) 
## ------------------------------------------------

## Adjust the dataset to eliminate NA at the top and bottom of the data but 
## don't touch eventual NA's inside the dataset after stripping top and/or bottom
## this is why ts.intersect does not solve the problem, as it will affect elements 
## inside that could be better treated by interpolation.
##
## Ex:To cutoff the first 3 and the last 5 rows call AdjustDataset( data, lag=3, lead=5 )
## Internal use only, do not export 
AdjustDataset <- function( data, lag=0, lead=0 ){
  if (is.null(data)) stop("Dataset not provided")
  
  T <- dim(data)[1]
  K <- dim(data)[2]
  
  if (lag>0)  data <- data[-seq(1,lag),] 
  if (lead>0) data <- data[-seq(T-lead,T),] 
  
  ## maximum and calculated lag and lead 
  ## clead is the last of the top rows to cutt off (same as lag)
  ## note that clead is the first of the bottom rows to cut off (diff than lead)
  lagmax <- clag <- 0
  leadmax <- clead <- T
  for (i in 1:K) {
    if (any(is.na(data[,i]))) {
      L <- length(data[,i])
      if (L>0) {
        j <- 1   
        while (j<L) if (is.na(data[j,i])) { clag <- j;  j<-j+1 } else { j <- L }
        j <- L
        while (j>1) if (is.na(data[j,i])) { clead <- j; j<-j-1 } else { j <- 1 }   
      }
    }
    lagmax  <- max(clag, lagmax)
    leadmax <- min(clead,leadmax)
    #if (DEBUG) print(cat("lagmax=",lagmax,"leadmax=",leadmax))
  }
  
  #if (DEBUG) print(cat("lagmax=",lagmax,"leadmax=",leadmax))
#   if (is.timeseries(data[,1])) 
#     new_data <- window( data, start=lagmax+1,end=leadmax-1)
#   else {
    new_data <- data
    if (clag>0)  new_data <- new_data[-seq(1,lagmax),] 
    T <- T-lagmax
    leadmax <- leadmax-lagmax
    if (clead>0) new_data <- new_data[-seq(leadmax,T),] 
#  }
  ## check if there is any NA remaining in the final dataset, if so return an empty dataset 
  final <- sum(which(is.na(new_data)))    
  
  start_adj <- start(data)+(lag+clag)/12  # TODO: test with zoo 
#   if (start_adj[2]>12) { 
#     years <- start_adj[2] %/% 12
#     months <- start_adj[2]-years*12-1
#     temp <- start(data)+c(years,months)
#     start_adj <- temp
#   }  
  
  if (final==0) 
    res <- zoo::zooreg( new_data, start=start_adj, frequency=frequency(data) ) 
  else 
    res <- NULL   ## see na.fail()          
}

## ------------------------------------------
## formula.explode() decompose the canonical formula into core (variable regressors) 
## and suffix (fixed regressors)  
formulaExplode <- function( formula ){
  
  ## tokenize the formula into charater vectors
  fm <- terms.formula(formula)
  
  rhs <- NULL
  suffix <- NULL
  
  ## split the formula into left (lhs) and right (rhs) hand side 
  lhs <- attr(fm,"variables")[[2]]
  lhs <- as.character(lhs)
  if (lhs=="") stop("A dependent variable y must be explicit in y~X")
  rhs <- attributes(fm)$term.labels
  
  ## rhs is further decomposed into core and suffix according to five cases 
  ## A. y~x2       => rhs = "x2" 
  ## B. y~x2+x3    => rhs = "x2" "x3" 
  ## C. y~x2|x1    => rhs = "x2|x1"
  ## D. y~x2+x3|x1 => rhs = "x2+x3|x1"
  ## E. y~x2|x1+x3 => rhs = "x2|x1+x3"
  ## if rhs has only 1 element with a "|" parse rhs can be either C, D or E
  
  if (length(rhs)>1) { ## B 
    core_split <- rhs 
    #if (DEBUG) print(paste("B",rhs))
  } else if (regexpr("[|]",rhs)[1]==-1) { ## A as there is no |   
    core_split <- rhs 
  } else {  ## C or D or E
    rhs_split <- unlist(strsplit(rhs,"[|]"))  
    core_exp <- rhs_split[1]
    if (length(rhs_split)==2) suffix <- trimws(rhs_split[2])
    core_split <- trimws(unlist(strsplit(core_exp,"[+]")))   # split the core in the '+' sign(s)
  }
  
  suffix_split <- NULL
  if (!is.null(suffix)) suffix_split <- trimws(unlist(strsplit(suffix,"[+]")))
  
  res <- list( dependentVar=lhs, variableTerms=core_split, fixedTerms=suffix_split )
  return( res ) 
}

## --------------------------------------------
## Compare this model information criteria to the best so far   
## Internal use only, not to export 
isBetterModel <- function( mod, ic, ic_best )
{
  match.arg(ic,c("bic","aic","r2","ll")) 
  ret <- FALSE
  if (ic=="bic" || ic=="aic") if (valIC(mod,ic)<=ic_best) ret <- TRUE
  if (ic=="r2"  || ic=="ll")  if (valIC(mod,ic)>=ic_best) ret <- TRUE
  ret  
}  

## --------------------------------------------
## Value of the information criteria 'ic' for model 'mod' 
## Internal use only, not to export 
valIC <- function( mod, ic ) 
{
  ret <- NA
  if (ic=='bic') ret <- BIC(mod)
  if (ic=='aic') ret <- AIC(mod)
  if (ic=='r2')  ret <- summary(mod)$adj.r.squared
  if (ic=='ll')  ret <- as.numeric(logLik(mod))
  ret 
}


## eof

