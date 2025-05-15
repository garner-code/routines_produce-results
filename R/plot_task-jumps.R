gen_jumps_plot <- function(task_jumps,
                            exp_strs,
                            cols,
                            p_wdth, p_hgt,
                            plt_sv_nm){
  
  fig_labs = c("C", "D")
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
      ylab = 'jumps'
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(task_jumps, exp_strs[i], cols, leg, ylab)
    fig_label(fig_labs[i])
  }
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=2/3)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = 'jumps'
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(task_jumps, exp_strs[i], cols, leg, ylab)
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
      ylab = 'jumps'
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(task_jumps, exp_strs[i], cols, leg, ylab)
    fig_label(fig_labs[i])
  }
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=1.5)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = 'jumps'
    } else {
      leg = FALSE
      ylab = ''
    }
    plot_task_jumps(task_jumps, exp_strs[i], cols, leg, ylab)
    fig_label(fig_labs[i])
  }
  dev.off()
}


plot_task_jumps <- function(task_jumps,
                            exp_str,
                            cols,
                            leg = TRUE,
                            ylab){
  # use this function to create the appropriate box plot
  jumps$train_type <- as.factor(jumps$train_type)
  levels(jumps$train_type) <- c('stable', 'variable')
  jumps$switch <- as.factor(jumps$switch)
  levels(jumps$switch) <- c('stay', 'switch')
  with(jumps %>% filter(exp == 'ts'),
       boxplot(jumps~switch*train_type,
               frame=F,
               ylim=c(0,5),
               yaxt = 'n',
               xaxt='n',
               col=rep(cols,2),
               ylab=ylab,
               xlab='group'))
  axis(1, at = c(1.5,3.5), labels=c('stab','var'))
  axis(2, at = c(0, 5))
  if (leg){
    legend('topleft', c('st','sw'), fill=col_scheme, bty='n')
  }
}

