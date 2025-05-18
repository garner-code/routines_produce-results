plot_ind_fits <- function(exp_str, subN, data_path){
  # this function plots individual logistic regression fits,
  # given subject data, and co-efficients from the model, output by the
  # functions contained in 'ent_apply-impact-models.R'
  # K. Garner, 2025
  ####################################################################
  # load the subject data, and beta co-efficients
  sub_dat <- read.csv(paste(data_path, 'evt-dat_4log-reg.csv', sep='')) %>%
     filter(exp == exp_str) %>% filter(sub == subN)
  sub_betas <- read.csv(paste(data_path, 'betas_', exp_str, '_first-level.csv', 
                              sep='')) %>% filter(sub == subN)
  
  # use the sub betas to predict the probability of a door_m response,
  # given the scaled inputs to the model (sw, swr, scs, cntx)
  tmp <- sub_dat %>% select(door_m, Sw, Swr, succss_odds, cntxt_odds) %>%
    mutate(Sw = scale(Sw),
           Swr = scale(Swr),
           succss_odds = scale(succss_odds),
           cntxt_odds = scale(cntxt_odds))
  
  tmp$log_odds = with(tmp, sub_betas$mu + sub_betas$sw*Sw + sub_betas$swr*Swr +
                    sub_betas$scs*succss_odds + sub_betas$cntx*cntxt_odds)
  tmp$p = 1/(1+exp(-tmp$log_odds))
  # ok, will plot the following:
  # door_m and p against each regressor
  
  # now link this info with a key grouping variable that I can use for plotting
  sub_dat$p = tmp$p
  
  # make the plots of guesses vs the regressors
  ps <- lapply(c('Sw', 'Swr', 'succss_odds', 'cntxt_odds'), get_scat_plt, sub_dat=sub_dat)
  ps
}

get_scat_plt <- function(iv, sub_dat){
  
  plot(sub_dat[,iv], sub_dat[,'door_m'], xlab=iv, ylab='door_m')
  points(sub_dat[,iv], sub_dat[,'p'], col="blue")
  p = recordPlot()
  plot.new()
  p
}
