niceplot <- function(i){
  
  xlim <- range(D$dates)
  layout(matrix(c(1,1,3,3,3,2,2,3,3,3),2,5,byrow = TRUE))
  # plot mobility
  plot(mobility$mob_raw$dates,mobility$mob_raw[,i+1],#main = country[i],
       ylim = c(0,max(c(1,mobility$mob_raw[,i+1]))),
       xlab = '',ylab = 'prop. mobility',pch=16,bty = 'n')
  lines(mobility$mob_combined_smooth$dates,mobility$mob_combined_smooth[,i+1],col='blue',lwd=2)
  # lines(mob3[,i+1],col='red')
  abline(h=1,lwd=2,col='red',lty=2)
  
  # plot Rt
  f <- which(results_baseline$CV_below.2[,i+1])
  x <- results_baseline$median_R$dates[f] # (epi_res$t_end2 )[f]
  plot(x,results_baseline$median_R[f,i+1],
       xlim=xlim,
       ylim = c(0,8),
       type = 'l',col='black',lwd=2,bty = 'n',
       # main =country[i], 
       xlab = '',ylab = 'Rt')
  
  polygon(c(x,rev(x)),
          c(results_baseline$low_R[f,i+1],rev(results_baseline$up_R[f,i+1])), 
          col = rgb(0,0,0,.2), border = FALSE )
  abline(h=(1),lwd=2,col='red',lty=2)
  
  # plot new estimates
  f <- which(results_full_Rt_D2$median_R[,i+1] != 0)
  lines(results_full_Rt_D2$median_R$dates[f],results_full_Rt_D2$median_R[f ,i+1],
        type='l',#ylim = c(0,10),
        # xlim=c(0,N_days),
        col='blue',lwd=2,
        main = country[i],ylab = 'R_t')
  
  polygon(c(results_full_Rt_D2$median_R$dates[f],rev(results_full_Rt_D2$median_R$dates[f])),
          c(results_full_Rt_D2$low_R[f,i+1],rev(results_full_Rt_D2$up_R[f,i+1])),
          col=rgb(0,0,1,.2), border = NA)
  
  lines(results_full_Rt_daily$median_R$dates[f],results_full_Rt_daily$median_R[f ,i+1],
        type='l',#ylim = c(0,10),
        # xlim=c(0,N_days),
        col='red',lwd=2,
        main = country[i],ylab = 'R_t')
  
  polygon(c(results_full_Rt_daily$median_R$dates[f],rev(results_full_Rt_daily$median_R$dates[f])),
          c(results_full_Rt_daily$low_R[f,i+1],rev(results_full_Rt_daily$up_R[f,i+1])),
          col=rgb(1,0,0,.2), border = NA)
  legend('topleft',legend = c(expression('R_t^{D,1}'),expression('R_t^{D,2}'),expression('R_t')), 
         col = c(rgb(0,0,0),rgb(0,0,1),rgb(1,0,0)),lwd=2,bty='n')
  
  
  
  # plot Rt vs mob
  f <- which(results_baseline$CV_below.2[,i+1])
  errbar(x = 1-results_meff$median_meff[f,i+1]-.5e-2,
         y = results_baseline$median_R[f,i+1],
         yplus = results_baseline$up_R[f,i+1],
         yminus = results_baseline$low_R[f,i+1],
         xlab = 'Prop. reduction in movement',
         ylab = 'Rt',yaxt="n",
         xlim = c(0,1),bty = 'n',
         ylim = c((0.5),(8)),
         col = rgb(1,0,0))
  axis(side =2, at = (c(0,1,3,6)), labels = c(0,1,3,6))
  
  f <- which(D[,i+1] > 0)
  errbar(x = 1-results_meff$median_meff[f,i+1]+.5e-2,
         y = results_full_Rt_D2$median_R[f,i+1],
         yplus = results_full_Rt_D2$up_R[f,i+1],
         yminus = results_full_Rt_D2$low_R[f,i+1],
         col = rgb(0,0,1),
         add=TRUE)
  # errbar(x = 1-M[f,i],
  #        y = results_full_Rt_daily$median_R[f,i+1],
  #        yplus = results_full_Rt_daily$up_R[f,i+1],
  #        yminus = results_full_Rt_daily$low_R[f,i+1],
  #        col = rgb(0,0,1),
  #        add=TRUE)
  
  abline(h=(1),lwd=2,col='red',lty=2)
  
  lines(results_full_Rt_assumed_mob$median_R$mobility,(results_full_Rt_assumed_mob$median_R[ ,i+1]),
        type='l',#ylim = c(0,10),
        # xlim=c(0,N_days),
        col='blue',lwd=2)
  
  polygon(c(results_full_Rt_assumed_mob$median_R$mobility,
            rev(results_full_Rt_assumed_mob$median_R$mobility)),
          (c(results_full_Rt_assumed_mob$low_R[,i+1],
             rev(results_full_Rt_assumed_mob$up_R[,i+1]))),
          col=rgb(0,0,1,.2),border=NA)
  mtext(country[i], side = 3,  outer = FALSE,adj = -.15,font=2)
  
}