plot_obs_vs_pred <- function(p_wdth, p_hgt,
                     col_scheme, 
                     plt_fname, # including full path
                     obs_sum_dat, # observed summary data
                     fig_font){
  # plot the observed vs predicted data (by group)
  
  #### for manuscripts
  pdf(paste(plt_fname, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family=fig_font, mfrow = c(1,1), mar = c(5, 4, 2, 1), las=2, cex=1)
  get_obs_vs_pred(obs_sum_dat, col_scheme)
  dev.off()
  
  svg(paste(plt_fname, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family=fig_font, mfrow = c(1,1), mar = c(5, 4, 2, 1), las=2, cex=1)
  get_obs_vs_pred(obs_sum_dat, col_scheme)
  dev.off()
  
  ## for talks
  pdf(paste(plt_fname, '_4tlks', '.pdf', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family=fig_font, mfrow = c(1,1), mar = c(5, 4, 2, 1), las=2, cex=2)
  get_obs_vs_pred(obs_sum_dat, col_scheme)
  dev.off()
  
  svg(paste(plt_fname, '_4tlks', '.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family=fig_font, mfrow = c(1,1), mar = c(5, 4, 2, 1), las=2, cex=2)
  get_obs_vs_pred(obs_sum_dat, col_scheme)
  dev.off()
  
}


get_obs_vs_pred <- function(dat_sum, col_scheme){
  with(dat_sum, plot(x=c(1,1.5), y=prop_resp,
                     ylim=c(0,0.4), 
                     xlim=c(0.75, 1.75),
                     pch=17, cex=2, type="p",
                     axes=F,
                     col=col_scheme[3], ylab=expression(p(Task["¬"*LT])),
                     xlab="Group"))
  arrows(x0=c(1,1.5), y0=dat_sum$prop_resp - dat_sum$se,
         x1=c(1,1.5), y1=dat_sum$prop_resp + dat_sum$se,
         angle=90, code=3, length=0.1, col=col_scheme[3])
  axis(1, at=c(1,1.5), labels=c("Stable", "Variable"))
  axis(2, at=seq(0,0.4,0.2), labels=seq(0,0.4,0.2), las=2)
  points(x=c(1.05,1.55), y=dat_sum$prop_pred, pch=16, cex=2, 
         col=adjustcolor(col_scheme[2], alpha.f=0.5))
  legend("topright", legend=c("Observed", "Predicted"), pch=c(17,16), 
         col=c(col_scheme[3], adjustcolor(col_scheme[2], alpha.f=0.5)),
         bty="n")
}

