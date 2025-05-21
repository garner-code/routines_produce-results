## K. Garner, 2025 ######################################
#########################################################
# apply second level analysis to the beta values estimated
# at the first level
remove_outliers <- function(betas, dv){
  # this function takes the betas dataframe, and replaces
  # any outliers with NA values
  p <- boxplot(betas[, dv])
  nu_dv = paste(dv, "flt", sep='_')
  betas_flt <- betas %>% select(sub, dv) %>%
                 mutate( !!nu_dv := if_else(.data[[dv]] > min(p$stats) &
                                         .data[[dv]] < max(p$stats),
                                         .data[[dv]], NA))
  betas_flt %>% select(nu_dv)
}


apply_outlier_filter_to_all_vars <- function(betas){

  vars <- names(betas)[names(betas) != 'sub' & names(betas) != 'train_type']
  betas <- cbind(betas, 
                 do.call(cbind, lapply(vars, remove_outliers, betas=betas)))
  # note that I manually plotted histograms of the distributions at this 
  # point and they all looked reasonably normal
  betas
}

apply_t_tests_to_all_vars <- function(betas, exp_str, res_path){
  
  # first, get the variables of interest
  vars_2_test <- names(betas)[grepl('_flt', names(betas))]
  
  # now apply the t test across variables
  # first compare to zero
  against_zero <- lapply(vars_2_test, function(x) t.test(betas[,x], mu=0, na.rm=T))
  against_zero_d <- do.call(rbind,
                            lapply(vars_2_test, function(x) cohens_d(betas,
                                                          formula=as.formula(paste(x,'~1',sep='')), 
                                                          mu=0,
                                                          ci=TRUE)))
  btwn_grp <- lapply(vars_2_test, function(x){
                    this_form = paste(x, '~train_type', sep='')
                    with(betas, t.test(as.formula(this_form),
                    na.rm=T))})
  
  btwn_grp_d <- do.call(rbind, lapply(vars_2_test, function(x){
                          this_form = paste(x, '~train_type', sep='')
                          cohens_d(betas,
                                  formula=as.formula(this_form),
                                  ci=T)}))
  # put ts together and save
  ####### first, one sample data
  ts <- do.call(rbind, lapply(against_zero, turnt2tib_onesamp))
  ts$fx <- vars_2_test
  ts <- inner_join(ts, turnd2tib(against_zero_d), by='fx')
  write.csv(ts, paste(res_path, 'exp_', exp_str,
                      '_betas-scnd-lvl_inf-ag-0.csv', sep=''),
            row.names=FALSE)
  
  ####### now put the between grp ts together and save
  twosamp_ts <- do.call(rbind, lapply(btwn_grp, turnt2tib_twosamp))
  twosamp_ts$fx <- vars_2_test
  twosamp_ts <- inner_join(twosamp_ts, turnd2tib_twosamp(btwn_grp_d),
                           by='fx')
  write.csv(twosamp_ts, paste(res_path, 'exp_', exp_str,
                        '_betas-scnd-lvl_inf-grp.csv', sep=''),
            row.names=FALSE)
}

turnt2tib_onesamp <- function(t_res){
  # function to compile t.test results into a csv file
  tibble(t = t_res$statistic,
         df = t_res$parameter,
         p = t_res$p.value,
         mu = t_res$estimate,
         l = t_res$conf.int[1],
         u = t_res$conf.int[2]) 
}

turnt2tib_twosamp <- function(t_res){
  # function to compile between group t.test results into a csv file
  tibble(t = t_res$statistic,
         df = t_res$parameter,
         p = t_res$p.value,
         mu_stab = t_res$estimate[1],
         mu_frq = t_res$estimate[2],
         l = t_res$conf.int[1],
         u = t_res$conf.int[2]) 
}

turnd2tib <- function(d_res){
  # make the tibble I want from the output of cohens_d()
  tibble(fx = d_res$.y.,
         d = d_res$effsize,
         n = d_res$n,
         dl = d_res$conf.low,
         du = d_res$conf.high)
}

turnd2tib_twosamp <- function(d_res){
  # make the tibble I want from the output of cohens_d()
  tibble(fx = d_res$.y.,
         d = d_res$effsize,
         n_stab = d_res$n1,
         n_frq = d_res$n2,
         dl = d_res$conf.low,
         du = d_res$conf.high)
}

# #################################################
# # now, correlate the regressors with each other
# # and with the routine score
# # first get the routine score
# R <- read.csv(file=paste("../doors-data/data-wrangled/exp", exp_str, 
#                             "rscore-full.csv", sep="_"))
# # first get an average routine score
# R <- R %>% group_by(sub) %>% summarise(mur = mean(r))
# flt <- inner_join(flt, R, by="sub")
# # remove rows with nas
# flt <- flt %>% na.omit()
# 
# # first, check for multivariate outliers
# mhl.mat <- as.matrix(flt[,c("mu_flt", "scs_flt", "cntx_flt", "mur")]) 
# mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
# mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist
# hist(mhl.dist, breaks = 20)
# sprintf("For a Mahalanobis to be less that .1 per cent likely to have occured by chance, given our degrees of feedom (%f), it has to be a value greater than %f", length(mhl.dist)-1, qchisq(.001, df=length(mhl.dist)-1))
# 
# #### learning transfer - we therefore remove the participants with a mahalanobis distance
# #### greater than 56.47
# rm_sbs <- as.numeric(names(mhl.dist)[which(mhl.dist > qchisq(.001, df=length(mhl.dist)-1))])
# flt <- flt %>% filter(!sub %in% rm_sbs)
# flt <- flt %>% mutate(mur_l = log(mur))
# ggpairs(flt %>% select(mu_flt, scs_flt, cntx_flt, mur_l),
#         mapping=ggplot2::aes(colour = flt$train_type),
#         upper = list(continuous = wrap("cor", method = "spearman")))
### now do a pairs plot
