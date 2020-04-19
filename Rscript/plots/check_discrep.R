# # check
# N_days=100
# N_geo=2
# x <- matrix(c(seq(1,0,length.out = N_days),seq(0,1,length.out = N_days)),N_days,2,byrow = FALSE)
# y <- pnorm(q = x,mean = .5,sd = .08)
# plot(x[,1],y[,1])
# plot(x[,1],y[,2])
# 
# r_day <- Rt_fun(theta = c(5,5,2,2),x = y)
# 
# H <- matrix(0,nrow = N_days, ncol = N_days)
# for (i in 1:N_days){
#   f <- max(c(1,i-delta_id$SItrunc))
#   H[i,f:i] <- rev(delta_id$dist)[((delta_id$SItrunc+1)-(i-f)):(delta_id$SItrunc+1)]
#   if (i>1) H[i,f:i] <-  H[i,f:i]/sum( H[i,f:i])
# }
# 
# RD <- H %*% r_day
# yD <- H %*% y
# 
# plot(1-y[,1],r_day[,1],ylim=c(0,5))
# lines(1-yD[,1],RD[,1],type='p',pch=16,col='blue')
# 
# plot(1-y[,2],r_day[,2],ylim=c(0,5))
# lines(1-yD[,2],RD[,2],type='p',pch=16,col='blue')
