#' Vectorize T-test for the Property data
#'
#' @param x features of the Property dataframe
#'
#' @return vectorized t-tests
#' @export
vectorize_ttest_for_property <- function(x, property_df) {
  t.test(unlist(property_df[, x]) ~ property_df$treat)
}

#' Vectorize Proportion z-test for the Property data
#'
#' @param var the variable of which we want to compare the proportions
#' @param df dataframe that contains the data
#' @param treatment_var name of the treatment variable
#'
#' @return vectorized z-tests
#' @export
vectorize_propztest_for_property <- function(var, df, treatment_var = 'treat') {
  
  control_sample <- df %>% 
    filter(!!as.name(treatment_var) == FALSE) %>% nrow()
  control_overspent <- df %>% 
    filter(!!as.name(treatment_var) == FALSE, !!as.name(var) == TRUE) %>% nrow()
  treatment_sample <- df %>% 
    filter(!!as.name(treatment_var) == TRUE) %>% nrow()
  treatment_overspent <- df %>% 
    filter(!!as.name(treatment_var) == TRUE, !!as.name(var) == TRUE) %>% nrow()
  
  prop.test(x = c(treatment_overspent, control_overspent), 
            n = c(treatment_sample, control_sample),
            alternative = "less")
}