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
  
  # ## for talks
  # pdf(paste(plt_fname, '_4tlks', '.pdf', sep=''), # for talks
  #     width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  # par(family=fig_font, mfrow = c(1,1), mar = c(5, 4, 2, 1), las=2, cex=2)
  # get_obs_vs_pred(obs_sum_dat, col_scheme)
  # dev.off()
  
  svg(paste(plt_fname, '_4tlks', '.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family=fig_font, mfrow = c(1,1), mar = c(5, 4, 2, 1), las=2, cex=3)
  get_obs_vs_pred(obs_sum_dat, col_scheme)
  dev.off()
  
}


get_obs_vs_pred <- function(dat_sum, col_scheme){
  with(dat_sum, plot(x=c(1,1.5), y=prop_resp,
                     ylim=c(0,0.4), 
                     xlim=c(0.75, 1.75),
                     pch=17, cex=2, type="p",
                     axes=F,
                     col=col_scheme[3], ylab="p(Response)",
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

prnt_plt_3way <- function(p_wdth, p_hgt,
                     col_scheme, 
                     plt_fname, # including full path
                     fx_dat, # predicted summary data
                     fig_font){
  # print the 3-way interaction plot
  
  pdf(paste(plt_fname, '.pdf', sep=''),
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family=fig_font, mfrow = c(2,2), mar = c(5, 4, 2, 1), las=2, cex=1)
  plot_fx_3way(fx_dat, col_scheme)
  dev.off()
  
  svg(paste(plt_fname, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family=fig_font, mfrow = c(2,2), mar = c(5, 4, 2, 1), las=2, cex=1)
  plot_fx_3way(fx_dat, col_scheme)
  dev.off()
  
  svg(paste(plt_fname, '_4tlks', '.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family=fig_font, mfrow = c(2,2), mar = c(5, 4, 2, 1), las=2, cex=3)
  plot_fx_3way(fx_dat, col_scheme)
  dev.off()
}

plot_fx_3way <- function(df, col_scheme){
  # this will plot the predicted door_m responses and error bars for each group, 
  # at each level of success, for each level of task odds
  ylims = c(0, .7)
  plt_cols = c(rep(col_scheme[1],3), rep(col_scheme[2],3))
  xs = c(1:3, 5:7)
  plot(0, 0, xlim=c(0,8), ylim=ylims, axes=F, xlab="", ylab="")
  for(g in c(".75", ".5", ".25")){
    ylab_text <- if (g ==".5" | g==".75") "p(Response)" else ""
    xlab_text <- if (g == ".5" | g==".25") "Success Odds Quantile" else ""
    title_text <- paste("Task Odds Quantile = ", g, sep="")
    with(df, plot(xs, predicted[facet==g],
                  pch=19, cex=2,
                  ylim=ylims,
                  frame.plot=FALSE,
                  xlab=xlab_text,
                  ylab=ylab_text,
                  axes=F,
                  col=plt_cols,
                  main=title_text))
    with(df, arrows(x0=xs, y0=conf.low[facet==g],
                    x1=xs, y1=conf.high[facet==g],
                    angle=90, code=3, length=0.1, 
                    col=plt_cols))
    x_tick_txt <- if (g==".25" | g==".5")  rep(c(".25", ".5", ".75"), times=2) else rep("", times=6)
    with(df, axis(1, at=xs, labels=x_tick_txt))
    with(df, axis(2, at=seq(0.1, ylims[2], by=.2), las=2))
  }
  legend("topright", legend=c("Stable", "Variable"), pch=19,
         col=unique(plt_cols), bty="n")
  
}

