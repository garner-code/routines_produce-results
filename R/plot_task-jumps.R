gen_jumps_plot <- function(data,
                           plot_formula,
                            exp_strs,
                            cols,
                            p_wdth, p_hgt,
                            plt_sv_nm,
                            fig_labs,
                            ylabel){
  
#  fig_labs = c("C", "D")
  ###########################################################
  # plot task jumps box plots
  ##########################################################
  # for manuscripts
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=2/3)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab)
    fig_label(fig_labs[i])
  }
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=2/3)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab)
    fig_label(fig_labs[i])
  }
  dev.off()
  
  ##########################################################
  # for talks
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=1.5)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab)
    fig_label(fig_labs[i])
  }
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=1.5)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab)
    fig_label(fig_labs[i])
  }
  dev.off()
}


plot_task_jumps <- function(data,
                            plot_formula,
                            exp_str,
                            cols,
                            leg = TRUE,
                            ylab){
  # use this function to create the appropriate box plot
  data$train_type <- as.factor(data$train_type)
  levels(data$train_type) <- c('stable', 'variable')
  data$switch <- as.factor(data$switch)
  levels(data$switch) <- c('stay', 'switch')
  with(data %>% filter(exp == 'ts'),
       boxplot(as.formula(plot_formula),
               frame=F,
               ylim=c(0,5),
               yaxt = 'n',
               xaxt='n',
               col=rep(cols,2),
               ylab=ylab,
               xlab='group'))
  axis(1, at = c(1.5, 3.5), labels=c('stab','var'))
  axis(2, at = c(0, 5))
  if (leg){
    legend('topleft', c('st','sw'), fill=col_scheme, bty='n')
  }
}

