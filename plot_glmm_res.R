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

res_path = 'res'
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
col_scheme <- c('#1b9e77','#d95f02', '#7570b3')



# now plot this data!
