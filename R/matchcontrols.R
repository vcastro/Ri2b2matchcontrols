#' matchcontrols
#' 
#' The main match controls function to match controls using cem and generate
#' an html report and a list of matched controls.
#'
#' @param d A data frame of patients with matching variables and cohort field
#' @param case_patientset_name Name of the case patient set (for report)
#' @param controlpool_patientset_name Name of the control pool patient set (for report)
#' @param controls_to_match Number of controls to match to each patient set
#' @param match_variables A string vector of variable names to use for matching
#'
#' @return A list with the elements
#' \item{report_text}{String of the html report}
#' \item{html_report}{Full path to html report file in tmp directory}
#' \item{match_data}{The full patient data frame with match strata and which 
#'    controls were matched}
#' \item{cem_data}{The cem object from the cem function.}
#' \item{matched_controls}{The final list of matched controls (can be used to 
#'    create an i2b2 patientset)}
#' @export
#'
#' @examples
#' #Not run
matchcontrols <- function(d, case_patientset_name = "None Given",
  controlpool_patientset_name = "None Given", controls_to_match = 1,
  match_variables = c("age", "gender", "race")) {

  
  if(length(levels(d$cohort)) != 2) 
    stop(paste0("The dataset includes ", length(levels(d$cohort)), " cohorts. Only 2 cohorts are allowed (usually 'Case' and 'Control')"))
  
  
  # remove cases from controls
  
  
  if(nrow(d[d$cohort == "Control",]) / nrow(d[d$cohort == "Case",]) < controls_to_match)
     stop(paste0("There are insufficient control pool patients to run the match."))
  
  
  
  # run the matching procedure 
  m <- Ri2b2matchcontrols::cem_match(d, match_variables = match_variables, patientid_variable = "patient_num", controls_to_match = controls_to_match)
  
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




