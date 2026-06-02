rm(list=ls())
library(lme4)
library(performance)
library(ggeffects)
library(DHARMa)
library(tidyverse)
library(stringr)
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
## The other thing I have learned is that there can be a very up/down pattern along
## the odds measures - i.e. some of the levels of odds have zero observations of
## task jumps, and some have plenty. This tells me that the predictors may be too granular as
## they are, and that we may be better off binning the odds measures into 5 or 6 bins,
## to get more stable estimates at each level of odds.
## ok, so binning works. the questions that remain are:
## can I get as good results when not removing extreme odds/cntxt values?
## the visual inspection suggests this is a bad plan, as it greatly affects the binning
## should I log transform the odds measures?
## the answer to the log transform is no. The model performs better
## when the odds predictors are not log transformed.
## should I drop one of the random fx?
## no. model performs best w maximal rfx structure (according to bic and aic)
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
         exp = factor(exp, levels = c('lt', 'ts'))) %>%
  mutate(idx=if_else(door_n == 0 & door_m == 0, 1, 0)) %>%# these are general errors, so remove them
  filter(idx == 0)
# now I will select the key variables, for keeping the model calls neater
lt_dat <- dat %>% filter(exp == 'lt') %>%
  select(door_n, door_m, train_type, sub, Sw, succss_odds, cntxt_odds, t)

# remove extreme cases
lt_dat <- lt_dat %>%
  group_by(sub) %>%
  filter(if_all(where(is.numeric), ~
                  . >= mean(., na.rm = TRUE) - 3 * sd(., na.rm = TRUE) &
                  . <= mean(., na.rm = TRUE) + 3 * sd(., na.rm = TRUE)
  )) %>%
  ungroup() # this keeps .95% of data, so I am happy with that I will check distributions
# again, and also check the relationships

# round these two variables to 2 dp, don't need more granularity than that
# and will help with visual checks
lt_dat <- lt_dat %>%
  mutate(coddr=round(cntxt_odds, 2))
lt_dat <- lt_dat %>%
  mutate(soddr=round(succss_odds, 2)) 

# now bin the Sw, coddr and soddr measures into a coarser level of granularity
lt_dat <- lt_dat %>%
  group_by(train_type) %>%
  mutate(
    coddr_bin = cut(coddr, breaks = 10),
    soddr_bin = cut(soddr, breaks = 10),
    Sw_bin    = cut(Sw, breaks = 10)
  ) %>%
  mutate(across(ends_with("_bin"), as.character)) %>%
  mutate(
    across(
      ends_with("_bin"),
      ~ {
        parts <- strsplit(.x, ",")
        lower <- as.numeric(gsub("\\(|\\[", "", sapply(parts, `[`, 1)))
        upper <- as.numeric(gsub("\\)|\\]", "", sapply(parts, `[`, 2)))
        (lower + upper) / 2
      },
      .names = "{.col}_mid"
    )
  )

# now that I have smoothed the granularity of the predictors a little, 
# I will next visually check whether or not the relationship between the predictors
# and the outcome is linear enough, or whether a log transform of the predictors is needed.

# this one is not perfect, but I reckon it's good enough
sw_p <- lt_dat %>%
  group_by(Sw_bin_mid, train_type, sub) %>% # group by these bins
  summarise(
    door_m_mean = mean(door_m) + .0001,     # calculate the proportion of observed values here
    log_odds = log(door_m_mean / (1 - door_m_mean) ) # to check for linear relationship to predictor variables
  ) %>%
  ggplot(aes(x = Sw_bin_mid, y = log_odds, colour=train_type)) +
  geom_point() +
  geom_smooth(
    method = "lm"
  ) +
  facet_wrap(~sub, scales="free") +
  theme_classic()
ggsave(sw_p, filename = paste(res_path, 'sw_p.svg', sep='/'), width=12, height=12)

# I think the log is a bit better
cntxt_p <- lt_dat %>% 
  group_by(coddr_bin_mid, train_type, sub) %>% # group by these bins
  summarise(
    door_m_mean = mean(door_m) + .001, # calculate the proportion of observed values here
    log_odds = log(door_m_mean / (1 - door_m_mean))
  ) %>%
  ggplot(aes(x = coddr_bin_mid, y = log_odds, colour=train_type)) +
  geom_point() +
  facet_wrap(~sub, scales="free") + 
  geom_smooth(
    method = "lm"
    ) +
  theme_classic()
ggsave(cntxt_p, filename = paste(res_path, 'cntxt_p.svg', sep='/'), width=14, height=14)

# log will prob help a bit but the difference is neglible
scss_odds_p <- lt_dat %>%
  group_by(sub, soddr_bin_mid, train_type) %>% # group by these bins
  summarise(
    door_m_mean = mean(door_m) + .001,
    log_odds = log(door_m_mean / (1 - door_m_mean))
  ) %>%
  ggplot(aes(x = log(soddr_bin_mid), y = log_odds, colour=train_type)) +
  geom_point() +
  facet_wrap(~sub, scales="free") +
  geom_smooth(
    method = "lm",
  ) +
  theme_classic()
ggsave(scss_odds_p, filename = paste(res_path, 'scss_odds_p.svg', sep='/'), width=14, height=14)


# pairs(lt_dat %>% select(Sw_bin_mid, coddr_bin_mid, soddr_bin_mid))
######################################################################################
## overall, what these visual checks tell me is that trimming the predictor variables
## helps a lot.


#################################################################################
# first, centre variables
lt_dat <- lt_dat %>%
  mutate(Sw_c = Sw_bin_mid - mean(Sw_bin_mid, na.rm=TRUE),
         soddr_c = soddr_bin_mid - mean(soddr_bin_mid, na.rm=TRUE),
         coddr_c = coddr_bin_mid - mean(coddr_bin_mid, na.rm=TRUE))

# save this data
save(lt_dat, file = paste(res_path, 'lt_dat_4_model.Rdata', sep=""))

# okies, first model, with all the interactions and the maximal random effects structure. 
# actually, thinking about it, we don't need the train x Sw_c interaction, 
# as we're only controlling for that
lt_mod <- glmer(door_m ~ train_type +
                      Sw_c +
                      soddr_c +
                      coddr_c + 
                      train_type*soddr_c +
                      train_type*coddr_c +
                      train_type*coddr_c*soddr_c +
                      (1 + Sw_c + soddr_c + coddr_c |sub),
                data = lt_dat, family = binomial)
save(lt_mod, file = paste(res_path, 'lt_mod.Rdata', sep=""))

### now I am checking simulated residuals, instead of binned
lt_mod_res <- simulateResiduals(lt_mod, plot = TRUE)
plotResiduals(lt_mod_res, form = "Sw_c")
# testUniformity(lt_mod_res)
testOutliers(lt_mod_res, type = "bootstrap")
# data:  lt_mod_res
# outliers at both margin(s) = 0, observations = 70197, p-value = 1
# alternative hypothesis: two.sided
#  percent confidence interval:
#  0.0000000000 0.0001656054
# sample estimates:
# outlier frequency (expected: 2.15108907788082e-05 ) 
testDispersion(lt_mod_res)
check_model(lt_mod) # use performance package to check assumptions re:
# normality of random fx

# now make the required null models
lt_mod_no_soddr <- update(lt_mod, . ~ . - soddr_c)
save(lt_mod_no_soddr, file = paste(res_path, 'lt_mod_no_soddr.Rdata', sep=""))
lt_mod_no_3way <- update(lt_mod, . ~ . - train_type:soddr_c:coddr_c)
save(lt_mod_no_3way, file = paste(res_path, 'lt_mod_no_3way.Rdata', sep=""))
lt_mod_no_grp <- update(lt_mod, . ~ . - train_type)
save(lt_mod_no_grp, file = paste(res_path, 'lt_mod_no_grp.Rdata'))
