matchcontrols <- function(d, case_patientset_name = "None Given",
  controlpool_patientset_name = "None Given", controls_to_match = 1,
  match_variables = c("age", "gender", "race")) {

  
  if(length(levels(d$cohort)) != 2) 
    stop(paste0("The dataset includes", length(levels(d$cohort)), "cohorts. Only 2 cohorts are allowed (usually Case and Control"))
  
  
  # remove cases from controls
  
  
  if(nrow(d[d$cohort == "Control",]) / nrow(d[d$cohort == "Case",]) < controls_to_match)
     stop(paste0("There are insufficient control pool patients to run the match."))
  
  
  
  # run the matching procedure 
  m <- Ri2b2matchcontrols::cem_match(d, match_variables = match_variables, drop_variables = "patient_num", controls_to_match = controls_to_match)
  
  #generate the report
  a <- rmarkdown::render(system.file("rmd/matchcontrols_report.Rmd", package="Ri2b2matchcontrols"), 
                         output_file = tempfile(fileext = ".html"), 
                         params = list(match_data = m,
                                       case_patientset_name = case_patientset_name, 
                                       controlpool_patientset_name = controlpool_patientset_name)
  )
  
  #output results
  list(report_text = paste(readLines(a), collapse = "\n"),
       html_file = a,
       match_data = m$match_data,
       cem_data = m$cem_result,
       matched_controls = m$match_data[which(m$match_data$k2k_control==TRUE), "patient_num"])
  
  
  ##refactor: warnings
}




