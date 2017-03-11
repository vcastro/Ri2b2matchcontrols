cem_match <- function(d, cohort_variable = "cohort", match_variables = c(), patientid_variable = "patient_num", controls_to_match = 1)
{

  #controls_to_match = 1
  
  d <- d[,c(patientid_variable, cohort_variable, match_variables)]
  
  cem_result <- cem::cem(data = d, treatment = "cohort", drop=patientid_variable, eval.imbalance = TRUE)
  
  d.matched <- cbind(d, match_strata = cem_result$strata, matched = cem_result$matched)
  
  strata_summary <- d.matched %>%
    group_by(cohort, match_strata) %>%
    summarise(n = n()) %>%
    spread(cohort, n) %>%
    mutate(controls_needed = Case*controls_to_match)
  
  
  #insufficient_controls <- filter(strata_summary, controls_needed>Control)$match_strata

  
  matched_controls <- d.matched %>%
    filter(cohort == "Control") %>%
    left_join(dplyr::select(strata_summary, match_strata, controls_needed), by="match_strata") %>%
    group_by(match_strata) %>%
    sample_frac(1) %>%      #select random controls from each strata
    filter(row_number() <= controls_needed)

  
  final_match_data <- d.matched %>% mutate(k2k_control = patient_num %in% matched_controls$patient_num)
  
  
  list(cem_result = cem_result,
       match_data = final_match_data,
       strata_summary = strata_summary,
       controls_to_match = controls_to_match,
       match_variables = match_variables)
  
}
