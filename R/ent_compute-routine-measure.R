ent_compute_routine_measure <- function(data_fname,
                                        save_new_data_fname,
                                        save_sum_data_fname){
  ######################################################################
  ## K. Garner (2025) - use initially wrangled data to compute transition
  ## matrices and entropy scores
  ######################################################################
  # note that anything commented was used before this was turned into
  # a function
  # DEPENDENCIES
  # -------- src-ent/ent_functions.R
  # ARGS
  # -------- data_fname - file name/full path to feed in data file
  # -------- save_new_data_fname - filename/full path for routine scores not 
  # averaged across context
  # -------- save_sum_data_fname - as above but averaged across context
  
  dat <- read.csv(data_fname)
  
  ### compute entropy score/routine measure for each subject
  subs <- unique(dat$sub)
  cntxts <- unique(dat$context_assign_ent)
  subs <- rep(subs, times=length(cntxts))
  cntxts <- rep(cntxts, each = max(subs))
  
  compute_ent_by_sub <- function(subN, cntxN, dat, n_doors = 16){
    # compute routine score and create a mini tibble for a given subject
    # and context
    tmp <- dat %>% filter(sub == subN & context_assign_ent == cntxN)
    door_dat <- tmp$door
    count_mat <- data_2_counts_matrix(data = door_dat, n_doors)
    probs <- p_st1_gs(counts_matrix = count_mat, n_doors = n_doors)
    # now that we have the Hs, we can get the occupancy weights
    TE <- get_wMTE(counts_matrix = count_mat, p_mat = probs)
    tibble(sub = subN, context = cntxN, TE=TE)
  }
  
  r_dat <- do.call(rbind, mapply(compute_ent_by_sub, subs, cntxts, MoreArgs = list(dat=dat),
                                 SIMPLIFY = FALSE))

  
  write.csv(r_dat, file=save_new_data_fname,
            row.names=FALSE)

  r_dat <- r_dat %>% group_by(sub) %>% summarise(TE=mean(TE)) %>% ungroup()
  write.csv(r_dat, save_sum_data_fname,
            row.names=FALSE)
}