plot_rel <- function(p_wdth, p_hgt,
                     r_col, fig_lab,
                     rel_data_save_name,
                     rel_plt_fname){
  # plot the correlation between dopamine and placebo sessions
  # aka plot the reliability data
  # save both an svg and a pdf file, for manuscripts (at p_wdt, p_hgt)
  # and for talks
  
  # first, get the data
  rel_dat <- read.csv(rel_data_save_name)
  
  #### for manuscripts
  pdf(paste(rel_plt_fname, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(mfrow = c(1,1), mar = c(4, 4, 2, 1), las=2, cex=2/3)
  do_cor_plot(rel_dat, fig_lab)
  dev.off()
  
  svg(paste(rel_plt_fname, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(mfrow = c(1,1), mar = c(4, 4, 2, 1), las=2, cex=2/3)
  do_cor_plot(rel_dat, fig_lab)
  dev.off()
  
  ## for talks
  pdf(paste(rel_plt_fname, '_4tlks', '.pdf', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(mfrow = c(1,1), mar = c(4, 4, 2, 1), las=2, cex=1.5)
  do_cor_plot(rel_dat, fig_lab)
  dev.off()
  
  svg(paste(rel_plt_fname, '_4tlks', '.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(mfrow = c(1,1), mar = c(4, 4, 2, 1), las=2, cex=1.5)
  do_cor_plot(rel_dat, fig_lab)
  dev.off()
  
}

do_cor_plot <- function(rel_dat, fig_lab){
  # actually do the plot
  min_x = round(min(apply(rel_dat[,c('levodopa', 'placebo')],2,min))-2,0)
  max_x = round(max(apply(rel_dat[,c('levodopa', 'placebo')],2,max))+2,0)

  with(rel_dat, plot(x=levodopa, y=placebo,
                     col=r_col, pch=19,
                     bty='n',
                     xlim=c(min_x, max_x),
                     ylim=c(min_x, max_x),
                     xlab='SA',
                     ylab='SB',
                     xaxt = 'n',
                     yaxt = 'n'))
  axis(1, at = seq(min_x, max_x, by = 8))
  axis(2, at = seq(min_x, max_x, by = 8))
  fig_label(fig_lab)
  with(rel_dat, abline(lsfit(placebo, levodopa),
                       col='#666666'))
}
