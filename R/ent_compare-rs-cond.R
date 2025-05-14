#######################################################################
# this code contains the functions required to wrangle and analyse
# the routines data for the group comparison in the training stage
#######################################################################
get_r_info_and_save <- function(exp_str, data_path){
  
  r_dat <- read.csv(paste(data_path, 'exp_', exp_str, '_rscore.csv',
                          sep='')) # this is r scores
  info_dat <- read.csv(paste(data_path, 'exp_', exp_str, 
                             '_avg.csv', sep=''))
  info_dat <- info_dat %>% filter(ses==2) %>%
    select(sub, train_type) %>% unique() # get only the required info
  
  r_dat <- inner_join(r_dat, info_dat, by='sub')
  write.csv(r_dat, paste(data_path, 'exp_', exp_str, '_r-by-grp.csv', 
                         sep=''),
            row.names=FALSE)
}