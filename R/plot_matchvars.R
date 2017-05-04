#' plot_matchvars 
#' 
#'
#' @param d A data frame of patients with matching variables and cohort field
#' @param match_variables A string vector of variable names to use for matching
#' @param grouping_variable A string to use as the grouping variable.
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' #Not run

plot_matchvars <- function(d, match_variables, grouping_variable="cohort") {
  
  factor_variables <- names(which(sapply(d[, match_variables], 
                                         is.factor) == TRUE))
  numeric_variables <- names(which(sapply(d[, match_variables], 
                                          is.numeric) == TRUE))
  
  if(length(match_variables) > 6) 
    stop("Too many matching variables")
  
  plot_cols <- ifelse(length(match_variables) %in% c(3,4,5,6), 3, length(match_variables))
  
  f_plots <- lapply(factor_variables, function(.x) plot_cat_histogram(d, .x, grouping_variable))
  n_plots <- lapply(numeric_variables, function(.x) plot_num_boxplot(d, .x, grouping_variable))
  
  g <- gridExtra::marrangeGrob(grobs=c(f_plots, n_plots), ncol=plot_cols, nrow=2)
  g
}


plot_cat_histogram <- function(d, plot_variable, grouping_variable = "cohort") {
  
  t <- data.frame(prop.table(table(d[, grouping_variable], d[, plot_variable]), 1))
  names(t) <- c(plot_variable, grouping_variable, "Freq")
  
  ggplot(t, aes(x = get(plot_variable), y = Freq)) + 
    geom_bar(aes(fill = get(grouping_variable)), position = "dodge", stat = "identity") + 
    xlab("cohort") + 
    theme_bw() + 
    guides(fill = guide_legend(title = grouping_variable))
  
}

plot_num_boxplot <- function(d, plot_variable, grouping_variable = "cohort") {
  
 ggplot(d, aes(x = get(grouping_variable), y = get(plot_variable))) + 
    geom_boxplot() + 
    theme_bw() + 
    ylab(plot_variable) + 
    xlab(grouping_variable)
  
}