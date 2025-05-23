remove_outliers <- function(betas, dv){
  # this function takes the betas dataframe, and replaces
  # any outliers with NA values
  p <- boxplot(betas[, dv])
  nu_dv = paste(dv, "flt", sep='_')
  betas_flt <- betas %>% select(sub, dv) %>%
    mutate( !!nu_dv := if_else(.data[[dv]] > min(p$stats) &
                                 .data[[dv]] < max(p$stats),
                               .data[[dv]], NA))
  betas_flt %>% select(nu_dv)
}