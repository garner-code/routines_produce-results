###########################################################################
## load likelihood stats from the model comparisons based on simulated data
## given the null model. Compute the likelihood of the observed LRT
###########################################################################
rm(list=ls())
library(DHARMa)
library(tidyverse)

res_nm <- 'katana'
##########################
## functions

load_data <- function(f){
  e <- new.env()
  obj_name <- load(f, envir = e)
  df <- e[[obj_name]]
  list(sim=df$simulatedLR, obs=df$observedLRT)
}

getP <- function(simulated, observed, alternative){
  if(alternative == "greater") p = mean(simulated >= observed, na.rm=T)
  if(alternative == "less") p = mean(simulated <= observed, na.rm=T)
  if(alternative == "two.sided") p = min(min(mean(simulated <= observed, na.rm=T), 
                                             mean(simulated >= observed, na.rm=T)))
  
  return(p)
}

get_sim_results <- function(fnames){
  dat <- lapply(fnames, load_data)
  LK_observed <- dat[[1]]$obs
  LK_sims <- as.vector(do.call(rbind, lapply(dat, function(x) return(x$sim))))
  p_LK <- getP(LK_sims, LK_observed, alternative="greater")
  mu_sims <- mean(LK_sims, na.rm = T)
  sd_sims <- sd(LK_sims, na.rm = T)
  
  return(tibble(obs=LK_observed, 
                p=p_LK,
                mu_sim=mu_sims,
                sd_sim=sd_sims))
  
}

##########################################################################
## LEARNING TRANSFER EXP
## Main effect of group
exp <- 'lt'
lt_me_grp_flst <- list.files(paste(res_nm, exp, 'lt_grp', sep='/'), 
                             pattern = "\\.RData$", full.names = TRUE)
lt_me_grp_res <- get_sim_results(lt_me_grp_flst)
lt_me_grp_res$fx <- "me_grp"
lt_me_grp_res$exp <- exp

## 3 way interaction
lt_3way_flst <- list.files(paste(res_nm, exp, 'lt_3way', sep='/'), 
                             pattern = "\\.RData$", full.names = TRUE)
lt_3way_res <- get_sim_results(lt_3way_flst)
lt_3way_res$fx <- "3way"
lt_3way_res$exp <- exp
##########################################################################
## TASK SWITCHING EXP
exp <- 'ts'
## Main effect of group
ts_me_grp_flst <- list.files(paste(res_nm, exp, 'ts_grp', sep='/'),
                             pattern = "\\.RData$", full.names = TRUE)
ts_me_grp_res <- get_sim_results(ts_me_grp_flst)
ts_me_grp_res$fx <- "me_grp"
ts_me_grp_res$exp <- exp

## Main effect of group

## 2 way interaction
ts_2way_flst <- list.files(paste(res_nm, exp, 'ts_2way', sep='/'),
                            pattern = "\\.RData$", full.names = TRUE)
ts_2way_res <- get_sim_results(ts_2way_flst)
ts_2way_res$fx <- "2way"
ts_2way_res$exp <- exp

##########################################################################
## SAVE RESULTS
sim_results <- rbind(lt_me_grp_res, lt_3way_res,
                     ts_me_grp_res, ts_2way_res)
apply(sim_results[, c("obs", "mu_sim", "sd_sim", "p")], 2, function(x) round(x, 2)) -> sim_results[, c("obs", "mu_sim", "sd_sim", "p")]
sim_results$id <- paste(sim_results$exp, sim_results$fx, sep="_")
write.csv(sim_results, paste("res", "glmm_sims.csv", sep="/"), row.names=F)

#########################################################################
## NOW LOAD THE MODELS AND GET THE RELEVANT BETA CO-EFFICIENTS
load(paste('res', 'lt_mod.Rdata', sep='/'))
lt_summary <- summary(lt_mod)
lt_summary$coefficients %>% as.data.frame() %>% rownames_to_column(var = "term") %>%
  filter(term != "(Intercept)") %>%
  mutate(exp = "lt") -> lt_coefs
lt_coefs <- lt_coefs %>% select(term, Estimate, `Std. Error`)
names(lt_coefs) <- c("term", "beta", "se")
apply(lt_coefs[, c("beta", "se")], 2, function(x) round(x, 2)) -> lt_coefs[, c("beta", "se")]
write.csv(lt_coefs, paste("res", "lt_coefs.csv", sep="/"), row.names = F)

load(paste('res', 'ts_mod.Rdata', sep='/'))
ts_summary <- summary(ts_mod)
ts_summary$coefficients %>% as.data.frame() %>% rownames_to_column(var = "term") %>%
  filter(term != "(Intercept)") %>%
  mutate(exp = "ts") -> ts_coefs
ts_coefs <- ts_coefs %>% select(term, Estimate, `Std. Error`)
names(ts_coefs) <- c("term", "beta", "se")
apply(ts_coefs[, c("beta", "se")], 2, function(x) round(x, 2)) -> ts_coefs[, c("beta", "se")]
write.csv(ts_coefs, paste("res", "ts_coefs.csv", sep="/"), row.names = F)
