#' Monthly macroeconomic indicators for Brazil.
#'
#' The BCB code in parenthesis corresponds to the series number in the BCB Time Series system.
#' 
#' @format A data frame with 170 rows and 15 variables in \code{zoo} format: 
#' \describe{
#'   \item{year}{observation year}
#'   \item{month}{observation month}
#'   \item{cpi}{broad national consumer price index (ipca) \% variation (433)}
#'   \item{mpr}{monetary policy rate (selic) \% per month (4390)}
#'   \item{intbk}{interbank rate \% p.y. (4392)}
#'   \item{asset}{change of ibovespa index \% (7832)}
#'   \item{unemp}{unemployment rate \% (10777)}
#'   \item{reer}{real effective exchange rate Jun/1994=101 (11752)}
#'   \item{prod}{economic activity index (ibc-br) seasonally adjusted (17632)}
#'   \item{comm}{commodity price index (20048)}
#'   \item{ner}{nominal exchange rate BRL/USD \% (7831)}
#'   \item{m1}{M1 monetary aggregate c.m.u (thousand) (1827)}
#'   \item{m2}{M2 monetary aggregate c.m.u (thousand) (1837)}
#'   \item{wti}{WTI crude oil price (source:EIA.GOV)}
#'   \item{brent}{Brent crude oil price (source:EIA.GOV)}
#' }
#'
#' @source \url{http://www4.bcb.gov.br/pec/series/ingl/}
"br_month"

