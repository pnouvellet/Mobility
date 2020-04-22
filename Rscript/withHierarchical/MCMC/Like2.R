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

Like2 <- function(theta){
  #mobility
  R_daily <- Rt_fun(theta = theta, x = M )
  logL <- rep(0,N_geo)
  std <- theta[(N_geo*2)+(1:N_geo)]
  Std<- matrix(std, nrow = N_days, ncol = N_geo, byrow = TRUE)
  n <-1e2
  for (i in 1:n){
    epsilon <- matrix(rnorm(n = N_days* N_geo, mean = 0, sd = Std),
                      nrow = N_days, ncol = N_geo, byrow = FALSE)
    Rt <- H %*% (R_daily * exp(epsilon) )
    
    logL <- logL+colSums(dpois(x = mD, lambda = Rt*Ot, log = TRUE) ,na.rm=TRUE)
  }
  logL <- logL/n
  sum(logL)
  # logL <- (dpois(x = mD, lambda = Rt*Ot, log = TRUE) )
  
  return(logL)
}
