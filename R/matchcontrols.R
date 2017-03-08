match_controls <- function(d, case_patientset_name = "None Given",
  controlpool_patientset_name = "None Given", controls_to_match = 1,
  match_variables = c("age", "gender", "race")) {

  a <- rmarkdown::render(system.file("rmd/match_report.Rmd", package="Ri2b2matchcontrols"), output_file = tempfile(fileext = ".html"), params = list(patient_data = df,
    case_patientset_name = case_patientset_name, controlpool_patientset_name = controlpool_patientset_name,
    controls_to_match = controls_to_match, match_variables = match_variables))

  #o <- read.csv("output.csv")

  list(report_text = paste(readLines(a), collapse = "\n"))
  ##, matching_results = o)
}




