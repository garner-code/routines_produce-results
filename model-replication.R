rm(list=ls())
library(lme4)
library(performance)
library(ggeffects)
library(DHARMa)
library(tidyverse)
library(stringr)

######### load the data
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

ts_dat <- dat %>% filter(exp == 'ts') %>%
  select(door_n, door_m, train_type, sub, Sw, succss_odds, cntxt_odds, t)

# remove extreme cases
ts_dat <- ts_dat %>%
  group_by(sub) %>%
  filter(if_all(where(is.numeric), ~
                  . >= mean(., na.rm = TRUE) - 3 * sd(., na.rm = TRUE) &
                  . <= mean(., na.rm = TRUE) + 3 * sd(., na.rm = TRUE)
  )) %>%
  ungroup() # this keeps .95% of data, so I am happy with that I will check distributions
# again, and also check the relationships

ts_dat <- ts_dat %>%
  mutate(coddr=round(cntxt_odds, 2))
ts_dat <- ts_dat %>%
  mutate(soddr=round(succss_odds, 2)) 

ts_dat <- ts_dat %>%
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

ts_dat <- ts_dat %>%
  mutate(Sw_c = Sw_bin_mid - mean(Sw_bin_mid, na.rm=TRUE),
         soddr_c = soddr_bin_mid - mean(soddr_bin_mid, na.rm=TRUE),
         coddr_c = coddr_bin_mid - mean(coddr_bin_mid, na.rm=TRUE))

# save this data
save(ts_dat, file = paste(res_path, 'ts_dat_4_model.Rdata', sep=""))

# does replicate but no convergence. will try with reduced rfx
ts_mod <- glmer(door_m ~ train_type +
                  Sw_c +
                  soddr_c +
                  coddr_c + 
                  train_type*soddr_c +
                  train_type*coddr_c +
                  train_type*coddr_c*soddr_c +
                  (1 + Sw_c + soddr_c + coddr_c |sub),
                data = ts_dat, family = binomial)
save(ts_mod, file = paste(res_path, 'ts_mod.Rdata', sep=""))

# model checks
### now I am checking simulated residuals, instead of binned
ts_mod_res <- simulateResiduals(ts_mod, plot = TRUE)
plotResiduals(ts_mod_res)
# testUniformity(lt_mod_res)
testOutliers(ts_mod_res, type = "bootstrap")
# data:  ts_mod_res
# outliers at both margin(s) = 6, observations = 71337, p-value = 0.62
# alternative hypothesis: two.sided
# percent confidence interval:
#   0.00000000 0.00063186
# sample estimates:
#   outlier frequency (expected: 0.000102050829162987 ) 
# 8.410783e-05 

testDispersion(ts_mod_res)
check_model(ts_mod)

# ts_mod_red_rfx <- glmer(door_m ~ train_type +
#                   Sw_c +
#                   soddr_c +
#                   coddr_c + 
#                   train_type*soddr_c +
#                   train_type*coddr_c +
#                   train_type*coddr_c*soddr_c +
#                   (1 + soddr_c + coddr_c |sub),
#                 data = ts_dat, family = binomial)
# save(ts_mod_red_rfx, file = paste(res_path, 'ts_mod_red_rfx.Rdata', sep=""))


ts_mod_no_ttbys <- update(ts_mod, . ~ . - train_type:soddr_c)
save(ts_mod_no_ttbys, file = paste(res_path, 'ts_mod_no_ttbys.Rdata', sep=""))
# ts_mod_no_sc <- update(ts_mod, . ~ . - coddr_c:soddr_c)
# save(ts_mod_no_sc, file = paste(res_path, 'ts_mod_no_sc.Rdata', sep=""))
ts_mod_no_me_grp <- update(ts_mod, . ~ . - train_type)
save(ts_mod_no_me_grp, file = paste(res_path, 'ts_mod_no_me_grp.Rdata', sep=""))

