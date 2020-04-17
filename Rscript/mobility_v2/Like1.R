#' get likelihood  
#'
#' get Poisson likelihood  
#' internal to the MCMC
#' 
#' @param lambda: 'force of infection' matrix (incidence weighted by serial interval),
#'                  column number of days, row: number of locations   
#'                  
#' @param I matrix of observed incidence, same dimension as lambda
#' 
#' @param R0 vector of reproduction numbers per locations
#'
#' @details  L log likelihood
#' @export
#' 

Like1 <- function(theta){
  #mobility
  R_daily <- Rt_fun(theta = theta, x = mat_mob )
  Rt <- m_w_delay %*% R_daily

  logL <- colSums(dpois(x = mD, lambda = Rt*mOt, log = TRUE) ,na.rm=TRUE)
  # logL <- (dpois(x = mD, lambda = Rt*mOt, log = TRUE) )
  
  return(logL)
}
