## K. Garner, 2025 ######################################
#########################################################
# apply logistic regression models to subjects, and extract beta
# values
#################################################
get_logist_mods_and_betas <- function(fname, exp_str, 
                                      data_path, res_path){

  # try using a message function here to print the subject number
  
  dat <- read.csv(fname) %>% filter(exp == exp_str)
  subs <- unique(dat$sub)
  results <- lapply(subs, run_logist, dat=dat, exp_str=exp_str)
  betas <- do.call(rbind, lapply(results, "[[", 1))
  write.csv(betas, paste(data_path, 'betas_', exp_str, '_first-level.csv', 
                         sep=''), row.names = FALSE)
  warnings <- do.call(rbind, lapply(results, "[[", 2))
  write.csv(warnings, paste(res_path, 'log-betas_', exp_str, '_first-level.csv', 
                         sep=''), row.names = FALSE)
}


#################################################
# write a function that runs the logistic regression
# and returns beta coefficients, for each subject
run_logist <- function(dat, subN, exp_str){

  do_fit <- quietly(glm)
  
  tmp <- dat %>% filter(sub == subN)
  fit <- do_fit(door_m ~ scale(Sw) + scale(Swr) + scale(succss_odds) +
               scale(cntxt_odds),
             data=tmp,
             family=binomial(link="logit"))
  coef <- fit$result$coefficients
  
  dat <- tibble(sub = subN,
                train_type = tmp$train_type[1],
                mu = coef['(Intercept)'],
                sw = coef['scale(Sw)'],
                swr = coef['scale(Swr)'],
                scs = coef['scale(succss_odds)'],
                cntx = coef['scale(cntxt_odds)'])
  warn <- tibble(sub = subN,
                 w = fit$warnings)
  list(dat, warn)
}



