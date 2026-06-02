rm(list = ls())
library(ggeffects)

### quick model plots
res_path = 'res/'
load(paste(res_path, 'lt_mod.Rdata', sep=""))
lt_pred <- ggpredict(lt_mod, terms=c("train_type", "soddr_c", "coddr_c"))
plot(lt_pred)

load(paste(res_path, 'ts_mod.Rdata', sep=""))

ts_pred <- ggpredict(ts_mod, terms=c("train_type", "soddr_c"))
ts_pred <- ggpredict(ts_mod, terms=c("train_type", "soddr_c", "coddr_c"))
plot(ts_pred)
