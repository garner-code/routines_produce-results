rm(list=ls())
library(lme4)
library(performance)
library(ggeffects)
library(DHARMa)
library(tidyverse)
##############################################################################
## Plan for multi-level modelling of LT experiment
## Things I have learned so far from visually inspecting the data -
## First, the switch rate predictor is pretty much redundant if we include the
## training group variable in the model, as the switch rate pretty much converges
## on the group factor. It is also highly correlated with the number of switches,
## so I am going to drop it.
## By plotting the covariates against %door_m selections at the level of each subject,
## I have learned that there are no obvious non-linearities that we are missing
## in the model (good news). However, it does seem that some participants encountered
## some extreme values of odds of success and odds of context, so for each participant,
## I will remove any observations that are above or below 2.5 x the IQR for that participant.
## I will then re-plot to check what removal of these outliers does.
#########################################################################################

##############################################################################
## Sort the data, and make sure the key variables are scaled and factored correctly. I will need to do this separately for the two experiments, because they have different ranges of values for the key predictors.  
# load the data and make sure the factors are defined correctly
data_path = 'data-wrangled/'
res_path = 'res/'
dat <- read.csv(paste(data_path, 'evt-dat_4log-reg.csv', sep=''))
dat <- dat %>% 
  mutate(train_type = factor(train_type, 
                             levels=c(1,2),
                             labels=c("stable", "variable")),
         sub = if_else(exp == 'ts', sub + 100, sub),
         sub = factor(sub),
         exp = factor(exp, levels = c('lt', 'ts')))
# now I will scale the key variables, for keeping the model calls neater
lt_dat <- dat %>% filter(exp == 'lt') %>%
  select(door_m, train_type, sub, Sw, succss_odds, cntxt_odds, idx) 


lt_dat <- lt_dat %>%
  group_by(sub) %>%
  filter(if_all(where(is.numeric), ~ 
                  . >= mean(., na.rm = TRUE) - 3 * sd(., na.rm = TRUE) &
                  . <= mean(., na.rm = TRUE) + 3 * sd(., na.rm = TRUE)
  )) %>%
  ungroup() # this keeps .95% of data, so I am happy with that I will check distributions
# again, and also check the relationships

lt_dat <- lt_dat %>%
  mutate(coddr=round(cntxt_odds, 2))

lt_dat <- lt_dat %>%
  mutate(soddr=round(succss_odds, 2)) 


# now, I will do some visual checks of the relationship between the predictors and
# the outcome variable
# with the switch predictor, I think things are good enough. We might be missing some nuances
# around people who don't switch most the time but switch from time to time, but
# overall, I don't think there are any key non-linear trends that we are missing. 
sw_p <- lt_dat %>%
  group_by(Sw, train_type, sub) %>% # group by these bins
  summarise(
    door_m_mean = mean(door_m) # calculate the proportion of observed values here
  ) %>%
  ggplot(aes(x = Sw, y = door_m_mean, colour=train_type)) +
  geom_point() +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial")
  ) +
  facet_wrap(~sub, scales="free") +
  theme_classic()
ggsave(sw_p, filename = paste(res_path, 'sw_p.svg', sep='/'))

# for some participants, there does seem to be a u-shaped function that we're not fitting, but its only
# a subset. For the rest, we're kinda doing ok.
cntxt_p <- lt_dat %>%
  group_by(coddr, train_type, sub) %>% # group by these bins
  summarise(
    door_m_mean = mean(door_m) # calculate the proportion of observed values here
  ) %>%
  ggplot(aes(x = coddr, y = door_m_mean, colour=train_type)) +
  geom_point() +
  facet_wrap(~sub, scales="free") + 
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial")) +
  theme_classic()
ggsave(cntxt_p, filename = paste(res_path, 'cntxt_p.svg', sep='/'))

# same as above
scss_odds_p <- lt_dat %>%
  group_by(sub, soddr, train_type) %>% # group by these bins
  summarise(
    door_m_mean = mean(door_m) # calculate the proportion of observed values here
  ) %>%
  ggplot(aes(x = soddr, y = door_m_mean, colour=train_type)) +
  geom_point() +
  facet_wrap(~sub, scales="free") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial")) +
  theme_classic()
ggsave(scss_odds_p, filename = paste(res_path, 'scss_odds_p.svg', sep='/'))

idx_p <- lt_dat %>%
  group_by(sub, idx, train_type) %>%
  summarise(
    door_m_mean = mean(door_m) # calculate the proportion of observed values here
  ) %>%
  ggplot(aes(x = idx, y = door_m_mean, colour=train_type)) +
  geom_point() +
  facet_wrap(~sub, scales="free") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial")) +
  theme_classic()
ggsave(idx_p, filename = paste(res_path, 'idx_p.svg', sep='/'))

pairs(lt_dat %>% select(Sw, coddr, soddr, idx))
######################################################################################
## overall, what these visual checks tell me is that trimming the predictor variables
## helps a lot.


#################################################################################
# first, centre variables
lt_dat <- lt_dat %>%
  mutate(Sw_c = Sw - mean(Sw, na.rm=TRUE),
         soddr_c = soddr - mean(soddr, na.rm=TRUE),
         coddr_c = coddr - mean(coddr, na.rm=TRUE),
         idx_c = idx - mean(idx, na.rm=TRUE))

# now I'll code the most complex
# gonna run the full model of interest on the lt data, and then do some model checks
# if this is not good, then I will think about how to address the poor fits.
# lt_mod <- glmer(door_m ~ train_type +
#                   Sw_c +
#                   soddr_c +
#                   coddr_c + 
#                   train_type*Sw_c +
#                   train_type*soddr_c +
#                   train_type*coddr_c +
#                   train_type*coddr_c*soddr_c +
#                   (1 + Sw_c + soddr_c + coddr_c |sub),
#                 data = lt_dat, family = binomial)
# # save the model!
# save(lt_mod, file = paste(res_path, 'lt_mod.Rdata', sep=''))
# check_model(lt_mod)

# actually, thinking about it, we don't need the train x Sw_c interaction, 
# as we're only controlling for that
lt_mod_red <- glmer(door_m ~ train_type +
                  Sw_c +
                  soddr_c +
                  coddr_c + 
                  train_type*soddr_c +
                  train_type*coddr_c +
                  train_type*coddr_c*soddr_c +
                  (1 + Sw_c + soddr_c + coddr_c |sub),
                data = lt_dat, family = binomial)
save(lt_mod_red, file = paste(res_path, 'lt_mod_red.Rdata'))
check_model(lt_mod_red)

### now I am checking simulated residuals, instead of binned
sim_res_lt_mod_red <- simulateResiduals(lt_mod_red, 
                                        n=1000,
                                        refit=TRUE,
                                        plot = TRUE)

lt_mod_idx <- glmer(door_m ~ train_type + # gonna start with simplest rfx
                      Sw_c +
                      soddr_c +
                      coddr_c +
                      idx_c +
                      train_type*soddr_c +
                      train_type*coddr_c +
                      train_type*coddr_c*soddr_c +
                      (1|sub),
                    data = lt_dat, family = binomial)
save(lt_mod_idx, file = paste(res_path, 'lt_mod_idx.Rdata'))

lt_mod_idx_rfx <- glmer(door_m ~ train_type + 
                      Sw_c +
                      soddr_c +
                      coddr_c +
                      idx_c +
                      train_type*soddr_c +
                      train_type*coddr_c +
                      train_type*coddr_c*soddr_c +
                      (1 + soddr_c + coddr_c |sub),
                    data = lt_dat, family = binomial)
save(lt_mod_idx_rfx, file = paste(res_path, 'lt_mod_idx_rfx.Rdata'))
check_model(lt_mod_idx)
check_model(lt_mod_idx_rfx)

##### 
### Next things to do:
### check the distribution of quantile residuals plot
### Triple check regressors - regressors are tripple checked and are doing what I think
### can confirm that the p(s|c) is working as assumed
### Now I will re-add the get N since last switch and see how that correlates with the predictors
### it does not. Going to add time since switch as a predictor in a model and leave overnight.
### will see what it does.

### Tomorrow! Must work through model diagnostics.




