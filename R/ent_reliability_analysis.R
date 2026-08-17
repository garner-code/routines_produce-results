###########################################################
# K. Garner, 2025. 
# set of functions to get routine scores from the dopamine
# experiment, and correlate reliability across the
# sessions
###########################################################

sum_counts_across_trials <- function(dat, subN, drugN, condN, n_doors = 16){
  # run this function to count transitions for each trial and sum
  # up at the end
  tmp <- dat %>% filter(sub == subN & drug == drugN & cond == condN )
  n_trials <- length(unique(tmp$t))
  trials <- unique(tmp$t)
  counts <- array(0, c(n_doors, n_doors, n_trials))
  
  for (i in 1:n_trials){
    counts[,,i] <- data_2_counts_matrix(tmp$door[tmp$t == trials[i]], n_doors = 16)
  }
  counts <- apply(counts, c(1,2), sum)
  counts
}

get_Rs <- function(dat, subN, drugN, condN){
  # for each subject, get routine scores by condition and session
  
  counts_mat <- sum_counts_across_trials(dat=dat,
                                         subN=subN,
                                         drugN=drugN,
                                         condN=condN)
  p_mat <- p_st1_gs(counts_mat, n_doors = 16)
  TE <- get_wMTE(counts_matrix = counts_mat, p_mat = p_mat)
  tibble(sub=subN, cond=condN, drug=drugN, TE=TE)
}

reliability_analysis <- function(rel_data_fname, 
                                 rel_data_save_name, 
                                 rel_analysis_save_name){
  
  # ----------------------------------------------------------------------------
  # load data
  # ---------------------------------------------------------------------------
  load(rel_data_fname) # this will load a dataframe called 'blocked_dat'

  # ----------------------------------------------------------------------------
  # compute R's
  # ---------------------------------------------------------------------------
  total_subs <- length(unique(blocked_dat$sub))
  subNs <- rep(unique(blocked_dat$sub), times=4)
  drugNs <- rep(unique(blocked_dat$drug), each=total_subs*2)
  condNs <- rep(unique(blocked_dat$cond), each=total_subs, times=2)
  r_dat <- do.call(rbind, mapply(get_Rs, subN=subNs, drugN=drugNs, condN=condNs, 
                                 MoreArgs=list(dat=blocked_dat), SIMPLIFY=FALSE)) 
  
  # now pivot wider
  r_dat_w <- r_dat %>% group_by(sub, drug) %>% 
    summarise(TE=mean(TE),,
              .groups="drop") %>%
    pivot_wider(id_cols=sub, names_from=drug,
                values_from=c(TE), 
                names_sep="_")
  write.csv(r_dat_w, rel_data_save_name, row.names=FALSE)
  measures = c("TE")
  
  cor_results <- cor.test(r_dat_w$levodopa, r_dat_w$placebo) %>%
    tidy() %>%
    mutate(measure = "TE")
  names(cor_results) <- c("r","t","p","df","l","u","method","alt","measure")
  write.csv(cor_results, rel_analysis_save_name, row.names=FALSE)
}

