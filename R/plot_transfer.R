plt_tran_bp_4paper_andtlks <- function(plt_sv_nm,
                                       p_wdth, p_hgt,
                                       dat,
                                       this_form,
                                       col_scheme,
                                       ylabel,
                                       ylim,
                                       fig_lab){
  
  # for paper
  ###### make the coefs plots for paper 
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1)
  tran_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1)
  fig_label(fig_lab)
  dev.off()
  
  pdf(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1)
  tran_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1)
  fig_label(fig_lab)
  dev.off()
  
  # for talks
  tlk_scl = 2
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
  tran_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1.5)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
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
  axis(1, at=c(1.5, 4), labels=c('Sta', 'Var'))
  axis(2, at=seq(0, max(ylim), by=50), labels=paste(seq(0, max(ylim), by=50)))
  mtext('Group', side=1, line=2, las=1, cex=xlab_cex)
  legend(0.25, 230, c('I','M'), fill=col_scheme, bty='n')
}

plt_bias_by_grp_4paper_andtlks <- function(plt_sv_nm,
                                           p_wdth, p_hgt,
                                           dat,
                                           this_form,
                                           col_scheme,
                                           ylabel,
                                           fig_lab){
  # for the manuscript
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1)
  trn_bias(dat, this_form, col_scheme, ylabel)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1)
  trn_bias(dat, this_form, col_scheme, ylabel)
  fig_label(fig_lab)
  dev.off()
  
  # and for talks
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
  trn_bias(dat, this_form, col_scheme, ylabel)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
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
  axis(1, at=c(1,2), labels=c('Sta', 'Var'))
  axis(2, at=seq(0,1, by=0.25), labels=paste(seq(0,1, by=0.25)))
  abline(h=0.5, lty=2, col='darkgrey')
}

plt_r_bias_cor_4paper_andtlks <- function(plt_sv_nm,
                                           p_wdth, p_hgt,
                                           bias,
                                           fig_lab){
  # for the manuscript
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=2/3)
  plot_r_bias_cor(bias)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=2/3)
  plot_r_bias_cor(bias)
  fig_label(fig_lab)
  dev.off()
  
  # and for talks
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
  plot_r_bias_cor(bias)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
  plot_r_bias_cor(bias)
  fig_label(fig_lab)
  dev.off()
}

plot_r_bias_cor <- function(bias){
  
  with(bias, plot(x=log_r_flt, y=k4_flt, pch=19,
                     frame.plot=F, 
                     ylim=c(0.1, 0.9),
                     xlim=c(0.8, 3.6),
                     xlab = "log r",
                     ylab = "transfer bias",
                     col = '#7570b3',
                     main = "",
                     xaxt = "n",
                     yaxt = "n"))
#                     cex = 2,
#                     cex.lab = 2))
  axis(1, at = seq(0.8, 3.6, by = 0.8),
       labels = paste(seq(0.8, 3.6, by = 0.8)))
  axis(2, at = seq(0.1, 0.9, by = 0.2), 
       labels=paste(seq(0.1, 0.9, by = 0.2)))
  with(bias, points(x=log_r_flt, y=k4_flt, pch=1))
  # draw regression line
  mod <- lm(k4_flt ~ log_r_flt, data=bias)
  abline(mod, col = "darkgrey", lwd = 1)
  text(x=1.6, y=0.15, labels="r(78)=-0.23**", cex = 1)
}
