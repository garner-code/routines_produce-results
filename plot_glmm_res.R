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
#font_import() # run this once only, comment out after first time

res_path = 'res'

loadfonts(device='pdf')
fig_font <- grep("source", fonts(), value = TRUE, ignore.case = TRUE)

# the below are relevant to the z-score plot but are also the base for many other plot dims and colour schemes so will put these here
g_p_wdth <- 10 # plot width of ms plot, in cm
g_p_hgt <- g_p_wdth
col_scheme <- c('#1b9e77','#d95f02', '#7570b3')
################################################################
## lt model
load(paste(res_path, 'lt_mod.RData', sep='/'))
eff_grp <- ggpredict(lt_mod, terms=c("train_type"))
eff_3way <- ggpredict(lt_mod, terms=c("train_type", "soddr_c", "coddr_c"))

################################################################
## load the data
load(paste(res_path, 'lt_dat_4_model.RData', sep='/'))

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
plot_obs_vs_pred(g_p_wdth*0.8, g_p_hgt,
                 col_scheme,
                 paste('figs', 'lt_mod_obs_vs_pred', sep='/'),
                 lt_dat_sum,
                 fig_font)

# and now plot the key interaction from the model
lt_3way <- plot(eff_3way, grid=FALSE) +
  theme_classic() +
  labs(title = "",
       y = expression(p(Task["¬"*LT])),
       x = "Group") +
  scale_x_discrete(
    labels = c("Stable", "Variable")
  )
# save that plot as a base for subsequent edits
ggsave(lt_3way, filename = paste('figs', 'lt_3way.pdf', sep='/'),
       width=g_p_wdth*1.25, height=g_p_hgt*0.75, units="cm",
       dpi=300)

################################################################
## ts model
load(paste(res_path, 'ts_mod.RData', sep='/'))
grp_by_soddr_c <- ggpredict(ts_mod, terms=c("train_type", "soddr_c"))
soddr_c_by_coddr_c <- ggpredict(ts_mod, terms=c("soddr_c", "coddr_c"))

## load the data
load(paste(res_path, 'ts_dat_4_model.RData', sep='/'))

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
plot_obs_vs_pred(g_p_wdth*0.8, g_p_hgt,
                 col_scheme,
                 paste('figs', 'ts_mod_obs_vs_pred', sep='/'),
                 ts_dat_sum,
                 fig_font)

# now plot interactions
ts_2way_grp <- plot(grp_by_soddr_c, grid=FALSE) +
  theme_classic() +
  labs(title = "",
       y = expression(p(Task["¬"*LT])),
       x = "Group") +
  scale_x_discrete(
    labels = c("Stable", "Variable")
  )
ggsave(ts_2way_grp, filename = paste('figs', 'ts_2way_grp.pdf', sep='/'),
       width=g_p_wdth*1.25, height=g_p_hgt*0.75, units="cm",
       dpi=300)

ts_2way_odds <- plot(soddr_c_by_coddr_c, grid=FALSE) +
  theme_classic() +
  labs(title = "",
       y = expression(p(Task["¬"*LT])),
       x = "Success odds") +
  scale_x_discrete(
    labels = c("Stable", "Variable")
  )

ggsave(ts_2way_odds, filename = paste('figs', 'ts_2way_odds.pdf', sep='/'),
       width=g_p_wdth*1.25, height=g_p_hgt*0.75, units="cm",
       dpi=300)
