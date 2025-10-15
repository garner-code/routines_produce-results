produce_z_plts <- function(exp_strs, col_scheme, 
                           p_wdth, p_hgt,
                           data_path,
                           sims_fname, ms_z_plt_fname,
                           tlk_z_plt_fname){
###########################################################
# K. Garner, 2025. 
# For each experiment, plot a histogram of the cleaned
# z-scores, along side that of the random agent.
###########################################################
# NOTES: col_scheme [colour scheme] must be named
  ## exp_strs [2] - c('lt', 'ts') # for referencing data from each exp
  ## p_wdth, p_hgt [1 num] - how tall and high you want the ms to be, in cm
  ## data_path [1, s] - is used under assumptions of running R from .proj
  ## sims_fname [1, s] - contains the folder within the proj e.g. sims/name.Rds
  ## ms_z_plt_fname, tlk_z_plt_fname, as above BUT WITHOUT EXT but for saving plots

  # make things alpha-ey
  col_scheme <- unlist(lapply(col_scheme, adjustcolor, alpha.f=0.5))

  ###########################################################
  # load data from each exp
  zdat <- do.call(rbind, lapply(exp_strs, function(x) {
    dat <- read.csv(file=paste(data_path, '/exp_', x, '_zs_cl.csv', sep=""))
    dat$exp <- x
    dat
  }))
  
  # load agent data
  load(sims_fname)
  
  ###########################################################
  # define histogram function
  ##########################################################
  main_titles = c("humans", "agent")
  do_z_hists <- function(main_titles){
    
    hist(with(zdat, mu_z[exp == exp_strs[1]]), 
         probability=TRUE,
         col = col_scheme[exp_strs[1]], 
         xlim = c(min(zdat$mu_z), max(zs)),
         ylim = c(0,0.08),
         xlab = expression(TE[z]), ylab = "freq",
         main = main_titles[1],
         xaxt = "n",
         yaxt = "n")
    axis(1, at = seq(-100, 0, by = 50))
    axis(2, at = seq(0, 0.08, by = 0.04))
    fig_label("A")
    legend(-110, 0.08, c("E1", "R"), fill = c(col_scheme[exp_strs[1]],
                                            col_scheme[exp_strs[2]]),
           bty='n') 
    hist(with(zdat, mu_z[exp == exp_strs[2]]), probability=TRUE, 
         col=col_scheme[exp_strs[2]], add=T)

    
    hist(zs, probability = TRUE,
         col = col_scheme["agent"],
         xlim = c(min(zdat$mu_z), max(zs)),
         xlab = expression(TE[z]), ylab = "",
         main=main_titles[2],
         xaxt = "n",
         yaxt = "n")
    axis(1, at = seq(-100, 0, by = 50))
    axis(2, at = seq(0, 0.4, by = 0.2))
    fig_label("B")
  }
  ###########################################################
  # plot histograms
  ##########################################################
  # for manuscripts
  pdf(paste(ms_z_plt_fname, '.pdf', sep=''), 
            width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family="Source Sans Pro", mfrow = c(1,2), mar = c(5, 4, 2, 1), las=2, cex=2/3)
  do_z_hists(main_titles = c('humans','agent')) #main_titles = c('','')
  dev.off()
  
  svg(paste(ms_z_plt_fname, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family="Source Sans Pro", mfrow = c(1,2), mar = c(5, 4, 2, 1), las=2, cex=2/3)
  do_z_hists(main_titles = c('humans','agent')) # main_titles = c('','')
  dev.off()

##########################################################
  # for talks
  pdf(paste(tlk_z_plt_fname, '.pdf', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family="Source Sans Pro", mfrow = c(1,2), mar = c(5, 4, 2, 1), las=2, cex=1.5)
  do_z_hists(main_titles = c('humans','agents'))
  dev.off()
  
  svg(paste(tlk_z_plt_fname, '.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family="Source Sans Pro", mfrow = c(1,2), mar = c(5, 4, 2, 1), las=2, cex=1.5)
  do_z_hists(main_titles = c('humans','agents'))
  dev.off()
##########################################################
}