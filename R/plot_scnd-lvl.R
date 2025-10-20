################################################################
## plot scnd lvl coefficients as boxplots
################################################################
make_coefs_plts_4paper_andtlks <- function(data_path,
                                           plt_sv_nm,
                                           p_wdth,
                                           p_hgt){
  
  ###### make the coefs plots for paper 
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(mfrow = c(2,1), las=2, cex=2/3)
  get_coefs_plts_acrss_exps(data_path)
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(mfrow = c(2,1), las=2, cex=2/3)
  get_coefs_plts_acrss_exps(data_path)
  dev.off()
  
  ###### and for talks
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(mfrow = c(2,1), las=2, cex=1.5)
  get_coefs_plts_acrss_exps(data_path)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(mfrow = c(2,1), las=2, cex=1.5)
  get_coefs_plts_acrss_exps(data_path)
  dev.off()
}


get_coefs_plts_acrss_exps <- function(data_path){
  
  #### this function generates the plots for
  #### both experiments and arranges them nicely
  par(mar = c(1,4,3,1))
  gen_coefs_plts(paste(data_path,
                       'betas_lt_first-level_cln.csv',
                       sep=''),
                 plot_formula = 'est ~ parameter + train_type',
                 leg = TRUE,
                 x_on = FALSE)
  fig_label('A')
  par(mar = c(3,4,1,1))
  gen_coefs_plts(paste(data_path,
                       'betas_ts_first-level_cln.csv',
                       sep=''),
                 plot_formula = 'est ~ parameter + train_type',
                 leg = FALSE,
                 x_on = TRUE)
  fig_label('B')
}

gen_coefs_plts <- function(fname,
                           plot_formula,
                           leg,
                           x_on){
  
  # generate coefficient plots for one experiment
  dat <- read.csv(fname)
  # select the variables required, and turn data from wideform
  # to longform
  dat <- dat %>% select(c(sub, train_type, ends_with('flt')))
  dat <- dat %>% pivot_longer(cols=ends_with('flt'),
                              names_to = 'parameter',
                              values_to = 'est')
  dat$train_type <- as.factor(dat$train_type)
  levels(dat$train_type) = c('stable', 'variable')
  dat$parameter <- factor(dat$parameter, 
                          levels=c('mu_flt',
                                   'sw_flt',
                                   'swr_flt',
                                   'cntx_flt',
                                   'scs_flt'))

  # set colours and other aesthetics here
  cols <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99')
  ylabel = 'log odds'
  
  plot_coefs_scnd_lvl(dat = dat, 
                      plot_formula = 'est ~ parameter + train_type',
                      cols = cols,
                      leg = leg,
                      ylab = ylabel,
                      x_on = x_on)
}

plot_coefs_scnd_lvl <- function(dat,
                                plot_formula,
                                cols,
                                leg,
                                ylabel,
                                x_on){
  
  with(dat,
       boxplot(as.formula(plot_formula),
               frame=F,
               at = c(1:5, 7:11),
               col = cols,
               ylim=c(-7,3),
               yaxt = 'n',
               xaxt='n',
               ylab=ylabel,
               xlab=''))
  abline(h=0, lty=2, col='darkgrey')

  if (leg){
    text(3, 3, adj=0.5, 'Stable', cex=1.5)
    text(9, 3, adj=0.5, 'Variable', cex=1.5)
    text(1, 2, '*', cex=1.5)
    text(5, 2, '*', cex=1.5)
    text(7, 2, '*', cex=1.5)
    text(11, 2, '*', cex=1.5)
  } else {
    text(1, 2, '*', cex=1.5)
    text(5, 2, '.', cex=1.5)
    text(7, 2, '*', cex=1.5)
    text(11, 2, '.', cex=1.5)
  }
  if (x_on){
    axis(1, at=c(1:5), labels=c(expression(B[0]), 'Sw', expression(Sw[r]), expression(O[LT]), expression(O[S])))    
  }
  axis(2, at=seq(-6, 2, by=2), labels=paste(seq(-6, 2, by=2)))
}
