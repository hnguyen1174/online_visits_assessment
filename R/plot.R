#' Visualize common support from a propensity_model
#'
#' @param propensity_model a propensity model
#'
#' @return a graph for common support
#' @export
visualize_common_support <- function(propensity_model) {
  
  # Get propensity score
  propensity_score <- predict(propensity_model, type = 'response')
  
  # Bind propensity score into a dataframe
  propensity_df <- data.frame(propensity_score = propensity_score, 
                              treat = propensity_model$model$treat) %>% 
    as_tibble()
  
  p <- propensity_df %>% 
    mutate(treat = as.character(treat)) %>% 
    ggplot(aes(x = propensity_score, group = treat, fill = treat)) +
    geom_density(alpha = 0.5) +
    theme_hc()
  
  p
}