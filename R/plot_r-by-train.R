plot_r_train <- function(p_wdth, p_hgt, 
                         cols, breaks = 10,  
                         x_rng, y_rng,
                         exp_strs, data_path,
                         r_by_g_fname){
  # this function takes the r scores from a given experiment,
  # combines them with a subject's training condition, 
  # and then plots a violin plot showing the group difference
  
  ###### first, some settings
  mar_set = c(4,4,2,1)
  las_set = 2
  cex_set = 2/3
  tlk_scl = 2.5
  
  ###### set up pdf and svg settings
  ###### for manuscripts
  pdf(paste(r_by_g_fname, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family="Source Sans Pro", mfrow = c(1,2), mar = mar_set, las=las_set, cex=cex_set)
  do_data_and_plot(exp_strs, data_path, x_rng, y_rng, cols, breaks)
  dev.off()
  
  svg(paste(r_by_g_fname, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family="Source Sans Pro", mfrow = c(1,2), mar = mar_set, las=las_set, cex=cex_set)
  do_data_and_plot(exp_strs, data_path, x_rng, y_rng, cols, breaks)
  dev.off()
  
  ###### for talks
  pdf(paste(r_by_g_fname, '_4tlks.pdf', sep=''), 
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl) 
  par(family="Source Sans Pro", mfrow = c(1,2), mar = mar_set, las=las_set, cex=1.5)
  do_data_and_plot(exp_strs, data_path, x_rng, y_rng, cols, breaks)
  dev.off()
  
  svg(paste(r_by_g_fname, '_4tlks.svg', sep=''), 
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl) 
  par(family="Source Sans Pro", mfrow = c(1,2), mar = mar_set, las=las_set, cex=1.5)
  do_data_and_plot(exp_strs, data_path, x_rng, y_rng, cols, breaks)
  dev.off()
}

plot_r_grp_hst <- function(rdat, x_rng, y_rng, cols,
                           fig_lab, leg, breaks, ylab){
  # do the actual plot
  # now set colours to be more alpha-ey
  col_scheme <- unlist(lapply(cols, adjustcolor, alpha.f=0.5))
  names(col_scheme) <- levels(rdat$train_type)
  
  hist(with(rdat, r[train_type == "Stable"]), 
       probability=FALSE,
       col = col_scheme['Stable'], 
       xlim = x_rng,
       ylim = y_rng,
       xlab = "TE", ylab = ylab,
       main = '',
       xaxt = "n",
       yaxt = "n",
       breaks = breaks)
  axis(1, at = seq(x_rng[1], x_rng[2], by = 20))
  axis(2, at = seq(y_rng[1], y_rng[2], by = 5))
  fig_label(fig_lab)
  if (leg){
    legend('topright', levels(rdat$train_type), 
           fill = c(col_scheme[levels(rdat$train_type)[1]],
                    col_scheme[levels(rdat$train_type)[2]]),
           bty='n') 
  }
  hist(with(rdat, r[train_type == "Variable"]), probability=FALSE, 
       col=col_scheme['Variable'], add=T, breaks = breaks)
}

get_dat <- function(exp_str, data_path){
  # get the data
  rdat <- read.csv(paste(data_path, 'exp_', exp_str,
                         '_r-by-grp.csv', sep=''))
  rdat$train_type <- as.factor(rdat$train_type)
  levels(rdat$train_type) <- c("Stable", "Variable")
  rdat
}

do_data_and_plot <- function(exp_strs, data_path, x_rng, y_rng, cols, breaks){
  # need to run this code for each plot generated, so may as well make into a function
  rdat <- get_dat(exp_strs[1], data_path)
  plot_r_grp_hst(rdat, x_rng, y_rng, cols, 'A', leg = TRUE, breaks, ylab='freq')
  rdat <- get_dat(exp_strs[2], data_path)
  plot_r_grp_hst(rdat, x_rng, y_rng, cols, 'B', leg = FALSE, breaks, ylab='')
}
