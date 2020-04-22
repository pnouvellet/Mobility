plot_proj <- function(i,threshold,log){
    n_pred <- nrow(temp$median) 
  ylim <- c(0,max(c(D[,i+1],temp$up[,i+1])))
  
  
  if (log == FALSE){
  plot(D$dates,D[,i+1],pch=16,
       col='black',bty = 'n',
       xlim = c(D$dates[which(D[,i+1]>0)[1]], 
                D$dates[1]+nrow(D)+n_pred+1),
       main = country[i],
       ylim = ylim)
  
  }else{
    ylim <- c(1,max(c(D[,i+1],temp$up[,i+1])))
    plot(D$dates,D[,i+1],pch=16,
         col='black',bty = 'n',
         xlim = c(D$dates[which(D[,i+1]>0)[1]], 
                  D$dates[1]+nrow(D)+n_pred+1),
         main = country[i],
         ylim = ylim,log='y')
    
    temp$median[which(temp$median[,i+1]==0),i+1]=.5
    temp$low[which(temp$low[,i+1]==0),i+1]=.5
    temp$up[which(temp$up[,i+1]==0),i+1]=.5
    
  }
  lines(temp$median$dates,temp$median[,i+1],
        lty = 1,lwd = 2,col = rgb(0,0,1))
  
  polygon(c(temp$median$dates,rev(temp$median$dates)),
          c(temp$low[,i+1],rev(temp$up[,i+1])),
          border = NA,col = rgb(0,0,1,.2))
  
  if ( threshold ) abline(h=100,col='red',lty = 2)
}