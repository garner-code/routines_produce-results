###############################################################
## Here, I plot the key findings from the GLMM models
## its a 4 panel plot. The first shows the predicted against
## observed probabilities of a response, by group.
## the second shows the key group x soodd interaction from the 
## model.
###############################################################
rm(list=ls())
library(tidyverse)
library(ggeffects)
library(merTools)
library(extrafont)
source("R/plot_glmm.R")
#font_import() # run this once only, comment out after first time

res_path = 'res'

loadfonts(device='pdf')
fig_font <- grep("source", fonts(), value = TRUE, ignore.case = TRUE)
fig_font <- fig_font[3]
# the below are relevant to the z-score plot but are also the base for many other plot dims and colour schemes so will put these here
g_p_wdth <- 8 # plot width of ms plot, in cm
g_p_hgt <- g_p_wdth
col_scheme <- c('#1b9e77','#d95f02', '#7570b3')
cols_4_fx <- c('#0868ac','#43a2ca', '#7bccc4')
################################################################
## lt model
load(paste(res_path, 'lt_mod.Rdata', sep='/'))
eff_3way <- ggpredict(lt_mod, terms=c("train_type", "soddr_c", "coddr_c"))
## U2H - change the levels of eff_3way$facet to go from high to low instead of low
## to high
eff_3way$facet <- factor(eff_3way$facet, levels = rev(levels(eff_3way$facet)),
                         labels=c(".75", ".5", ".25"))
# eff_3way is now just a dataframe with the predicted values and confidence 
# intervals for each combination of train_type, soddr_c, and coddr_c. 
# We can use this to plot the interaction effects.

################################################################
## load the data
load(paste(res_path, 'lt_dat_4_model.Rdata', sep='/'))

## first, I want to plot predicted against observed data, so we
## can check how far off the model was in its predictions
lt_dat$pred <- predict(lt_mod, type="response") # returns probabilities

# lets convert the data into probabilities
lt_dat_sum <- lt_dat %>% group_by(sub, train_type) %>%
  summarise(p=mean(door_m),
            pp=mean(pred)) %>%
  ungroup() %>%
  group_by(train_type) %>%
  summarise(n = n(),
            prop_resp = mean(p),
            prop_pred = mean(pp),
            sd = sd(p),
            se = sd/sqrt(n))

save(lt_dat_sum, file = paste(res_path, 'lt_dat_sum.RData', sep='/'))

# now for this new dataframe, I want to predict the proportion of 
# responses, using the model
plot_obs_vs_pred(g_p_wdth, g_p_hgt,
                 col_scheme,
                 paste('figs', 'lt_mod_obs_vs_pred', sep='/'),
                 lt_dat_sum,
                 fig_font)

# now plot the predicted effects from the model
prnt_plt_3way(g_p_wdth*2, g_p_hgt*2,
             col_scheme, 
             plt_fname=paste('figs', 'lt_mod_3way', sep='/'),
             fx_dat=eff_3way,
             fig_font=fig_font)


################################################################
## ts model
load(paste(res_path, 'ts_mod.Rdata', sep='/'))
grp_by_soddr_c <- ggpredict(ts_mod, terms=c("train_type", "soddr_c"))
soddr_c_by_coddr_c <- ggpredict(ts_mod, terms=c("soddr_c", "coddr_c"))

## load the data
load(paste(res_path, 'ts_dat_4_model.Rdata', sep='/'))

## first, I want to plot predicted against observed data, so we
## can check how far off the model was in its predictions
ts_dat$pred <- predict(ts_mod, type="response") # returns probabilities

# lets convert the data into probabilities
ts_dat_sum <- ts_dat %>% group_by(sub, train_type) %>%
  summarise(p=mean(door_m),
            pp=mean(pred)) %>%
  ungroup() %>%
  group_by(train_type) %>%
  summarise(n = n(),
            prop_resp = mean(p),
            prop_pred = mean(pp),
            sd = sd(p),
            se = sd/sqrt(n))

save(ts_dat_sum, file = paste(res_path, 'ts_dat_sum.RData', sep='/'))

# plot obs vs pred
plot_obs_vs_pred(g_p_wdth, g_p_hgt,
                 col_scheme,
                 paste('figs', 'ts_mod_obs_vs_pred', sep='/'),
                 ts_dat_sum,
                 fig_font)

prnt_plt_2way(g_p_wdth, g_p_hgt,
             col_scheme, 
             plt_fname=paste('figs', 'ts_mod_2way', sep='/'),
             fx_dat=grp_by_soddr_c,
             fig_font=fig_font)
