## K. Garner, 2025 ######################################
#########################################################
# apply logistic regression models to subjects, and extract beta
# values
#################################################
get_logist_mods_and_betas <- function(fname, exp_str, data_path){

  dat <- read.csv(fname) %>% filter(exp == exp_str)
  subs <- unique(dat$sub)
  betas <- do.call(rbind, lapply(subs, run_logist, dat=dat, exp_str=exp_str))
  write.csv(betas, paste(data_path, 'betas_', exp_str, '_first-level.csv', 
                         sep=''))
}


#################################################
# write a function that runs the logistic regression
# and returns beta coefficients, for each subject
run_logist <- function(dat, subN, exp_str){

  sprintf('running sub-%d of exp %s', subN, exp_str)
  tmp <- dat %>% filter(sub == subN)
  fit <- glm(door_m ~ scale(Sw) + scale(Swr) + scale(succss_odds) +
               scale(cntxt_odds),
             data=tmp,
             family=binomial(link="logit"))
  coef <- fit$coefficients
  tibble(sub = subN,
         train_type = tmp$train_type[1],
         mu = coef['(Intercept)'],
         sw = coef['scale(Sw)'],
         swr = coef['scale(Swr)'],
         scs = coef['scale(succss_odds)'],
         cntx = coef['scale(cntxt_odds)'])
}



