plt_ts_bp_4paper_andtlks <- function(plt_sv_nm,
                                     p_wdth, p_hgt,
                                     dat,
                                     this_form,
                                     col_scheme,
                                     ylabel,
                                     ylims,
                                     yticks,
                                     fig_lab){
  
  # for paper
  ###### make the coefs plots for paper 
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(mar=c(4,4,2,1), las=2, cex=2/3)
  ts_grp_bp(dat, this_form, col_scheme, 
            ylims, ylabel, yticks,
            xlab_cex=2/3)
  fig_label(fig_lab)
  dev.off()
  
  pdf(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(mar=c(4,4,2,1), las=2, cex=2/3)
  ts_grp_bp(dat, this_form, col_scheme, 
            ylims, ylabel, yticks,
            xlab_cex=2/3)
  fig_label(fig_lab)
  dev.off()
  
  # for talks
  tlk_scl = 2
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(mar=c(4,4,2,1), las=2, cex=1.5)
  ts_grp_bp(dat, this_form, col_scheme, 
            ylims, ylabel, yticks,
            xlab_cex=1.5)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(mar=c(4,4,2,1), las=2, cex=1.5)
  ts_grp_bp(dat, this_form, col_scheme, 
            ylims, ylabel, yticks,
            xlab_cex=1.5)
  fig_label(fig_lab)
  dev.off()
}

ts_grp_bp <- function(dat, this_form, col_scheme, 
                      ylims,
                      ylabel,
                      yticks,
                      xlab_cex){
  
  with(dat, 
       boxplot(as.formula(this_form),
               frame=F,
               at=c(1:2, 3.5:4.5),
               col=col_scheme,
               ylab=ylabel,
               ylim=ylims,
               yaxt='n',
               xaxt='n',
               xlab=''))
  axis(1, at=c(1.5, 4), labels=c('Sta', 'Var'))
  axis(2, at=yticks, labels=paste(yticks))
  mtext('Group', side=1, line=2, las=1, cex=xlab_cex)
  legend('topleft', c('St','Sw'), fill=col_scheme, bty='n')
}

plt_r_se_bias_cor_4paper_andtlks <- function(plt_sv_nm,
                                             p_wdth, p_hgt,
                                             bias_se,
                                             fig_lab){
  # for the manuscript
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(mar=c(4,4,2,1), las=2, cex=2/3)
  plot_r_se_bias_cor(bias_se)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(mar=c(4,4,2,1), las=2, cex=2/3)
  plot_r_se_bias_cor(bias_se)
  fig_label(fig_lab)
  dev.off()
  
  # and for talks
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(mar=c(4,4,2,1), las=2, cex=1.5)
  plot_r_se_bias_cor(bias_se)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(mar=c(4,4,2,1), las=2, cex=1.5)
  plot_r_se_bias_cor(bias_se)
  fig_label(fig_lab)
  dev.off()
}

plot_r_se_bias_cor <- function(bias_se){
  
  with(bias_se, plot(x=log_r_flt, y=se_bias, pch=19,
                     frame.plot=F, 
                     ylim=c(0.1, 0.9),
                     xlim=c(0.8, 3.6),
                     xlab = "log r",
                     ylab = "switch bias",
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
  with(bias_se, points(x=log_r_flt, y=se_bias, pch=1))
  # draw regression line
  #mod <- lm(k4_flt ~ log_r_flt, data=bias)
  #abline(mod, col = "darkgrey", lwd = 1)
  text(x=2, y=0.15, labels="r(96)=-0.53***", cex = 1)
}
