plt_tran_bp_4paper_andtlks <- function(plt_sv_nm,
                                       p_wdth, p_hgt,
                                       dat,
                                       this_form,
                                       col_scheme,
                                       ylabel,
                                       ylim,
                                       fig_lab,
                                       fig_font){
  
  # for paper
  ###### make the coefs plots for paper 
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1)
  tran_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1)
  fig_label(fig_lab)
  dev.off()
  
  pdf(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1)
  tran_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1)
  fig_label(fig_lab)
  dev.off()
  
  # for talks
  tlk_scl = 2
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1.5)
  tran_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1.5)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1.5)
  tran_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1.5)
  fig_label(fig_lab)
  dev.off()
}

tran_grp_bp <- function(dat, this_form, col_scheme, ylabel, ylim, xlab_cex){
  
  with(dat, 
       boxplot(as.formula(this_form),
               frame=F,
               at=c(1:2, 3.5:4.5),
               col=col_scheme,
               ylab=ylabel,
               ylim=ylim,
               yaxt='n',
               xaxt='n',
               xlab='',
               notch=FALSE))
  axis(1, at=c(1.5, 4), labels=c('Stable', 'Variable'), las=1)
  axis(2, at=seq(0, max(ylim), by=50), labels=paste(seq(0, max(ylim), by=50)))
  mtext('Group', side=1, line=2, las=1, cex=xlab_cex)
  legend(0.25, 200, c('I','M'), fill=col_scheme, bty='n')
}

plt_bias_by_grp_4paper_andtlks <- function(plt_sv_nm,
                                           p_wdth, p_hgt,
                                           dat,
                                           this_form,
                                           col_scheme,
                                           ylabel,
                                           fig_lab,
                                           fig_font){
  # for the manuscript
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=2/3)
  trn_bias(dat, this_form, col_scheme, ylabel)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=2/3)
  trn_bias(dat, this_form, col_scheme, ylabel)
  fig_label(fig_lab)
  dev.off()
  
  # and for talks
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1.5)
  trn_bias(dat, this_form, col_scheme, ylabel)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1.5)
  trn_bias(dat, this_form, col_scheme, ylabel)
  fig_label(fig_lab)
  dev.off()
}

trn_bias <- function(dat,
                     this_form,
                     col_scheme,
                     ylabel){
  
  with(dat, 
       boxplot(as.formula(this_form),
               frame=F,
               col=col_scheme,
               ylab=ylabel,
               yaxt='n',
               xaxt='n',
               xlab='Group',
               ylim=c(0,1)))
  axis(1, at=c(1,2), las=1, labels=c('Stable', 'Variable'))
  axis(2, at=seq(0,1, by=0.25), labels=paste(seq(0,1, by=0.25)))
  abline(h=0.5, lty=2, col='darkgrey')
}

plt_r_bias_cor_4paper_andtlks <- function(plt_sv_nm,
                                          p_wdth, p_hgt,
                                          fig_lab,
                                          fig_font,
                                          x,y, # data for x and y
                                          grp, # grouping variable
                                          xlabel, # what we would like on the x-axis
                                          ylabel, # what we would like on the y-axis
                                          x_seq, # sequence for the x-tick labels
                                          y_seq, # same for y
                                          cor_text, # what to write on the plot - e.g. beta = blah, p<.001 etc
                                          xtext, # x co-ord of where the cor_text should go
                                          ytext){ # y co-ord of where the cor_text should go
  # for the manuscript
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=2/3)
  plot_r_bias_cor(x,y, # data for x and y
                  grp, # grouping variable
                  xlabel, # what we would like on the x-axis
                  ylabel, # what we would like on the y-axis
                  x_seq, # sequence for the x-tick labels
                  y_seq, # same for y
                  cor_text, # what to write on the plot - e.g. beta = blah, p<.001 etc
                  xtext, # x co-ord of where the cor_text should go
                  ytext) # y co-ord of where the cor_text should go
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=2/3)
  plot_r_bias_cor(x,y, # data for x and y
                  grp, # grouping variable
                  xlabel, # what we would like on the x-axis
                  ylabel, # what we would like on the y-axis
                  x_seq, # sequence for the x-tick labels
                  y_seq, # same for y
                  cor_text, # what to write on the plot - e.g. beta = blah, p<.001 etc
                  xtext, # x co-ord of where the cor_text should go
                  ytext)
  fig_label(fig_lab)
  dev.off()
  
  # and for talks
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1.5)
  plot_r_bias_cor(x,y, # data for x and y
                  grp, # grouping variable
                  xlabel, # what we would like on the x-axis
                  ylabel, # what we would like on the y-axis
                  x_seq, # sequence for the x-tick labels
                  y_seq, # same for y
                  cor_text, # what to write on the plot - e.g. beta = blah, p<.001 etc
                  xtext, # x co-ord of where the cor_text should go
                  ytext)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family=fig_font, mar=c(4,4,2,1), las=2, cex=1.5)
  plot_r_bias_cor(x,y, # data for x and y
                  grp, # grouping variable
                  xlabel, # what we would like on the x-axis
                  ylabel, # what we would like on the y-axis
                  x_seq, # sequence for the x-tick labels
                  y_seq, # same for y
                  cor_text, # what to write on the plot - e.g. beta = blah, p<.001 etc
                  xtext, # x co-ord of where the cor_text should go
                  ytext)
  fig_label(fig_lab)
  dev.off()
}

plot_r_bias_cor <- function(x,y, # data for x and y
                            grp, # grouping variable
                            xlabel, # what we would like on the x-axis
                            ylabel, # what we would like on the y-axis
                            x_seq, # sequence for the x-tick labels
                            y_seq, # same for y
                            cor_text, # what to write on the plot - e.g. beta = blah, p<.001 etc
                            xtext, # x co-ord of where the cor_text should go
                            ytext){ # y co-ord of where the cor_text should go
  
  colours = c("#9986A5", "#79402E")
  # x is what you want on the x-axis, and y is what you want on the y-axis
  x_dat = x
  y_dat = y
  
  plot(x=x_dat, y=y_dat, pch=19,
       frame.plot=F, 
       ylim=c(min(y_seq), max(y_seq)),
       xlim=c(min(x_seq), max(x_seq)),
       xlab = xlabel,
       ylab = ylabel,
       col =  colours[factor(grp)],
       main = "",
       xaxt = "n",
       yaxt = "n")
  
  axis(1, at = x_seq,
       labels = paste(x_seq))
  axis(2, at = y_seq, 
       labels=paste(y_seq))
  points(x=x_dat, y=y_dat, pch=1)
  # draw regression line
  mod <- lm(y_dat ~ x_dat, data=tibble(y_dat = y_dat, 
                                       x_dat = x_dat))
  abline(mod, col = "darkgrey", lwd = 1)
  text(x=xtext, y=ytext, labels=cor_text, cex = 1)
  
  legend("topleft",
         legend = c("Stable", "Variable"),
         pch = 19,
         col = colours,
         bty="n")
}
