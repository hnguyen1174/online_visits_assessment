#' Get Property and Metrics Data
#'
#' @return property and metrics data
#' @export
get_property_data <- function() {
  
  # Load Data -----------------------------------
  metrics <- readr::read_csv(file.path(here::here(), 'data/data.csv'))
  property <- readr::read_csv(file.path(here::here(), 'data/property_info.csv'))
  
  # Process Data --------------------------------
  metrics <- metrics %>% 
    clean_names() %>% 
    replace(is.na(.), 0)
  
  property <- property %>% 
    clean_names() %>% 
    dplyr::rename(online_visits = virtual_reality_launched) %>% 
    mutate(avg_review_rating_num = case_when(
      avg_review_rating == "< 2.5" ~ (0 + 2.5)/2,
      avg_review_rating == "2.5 - 3" ~ (2.5 + 3)/2,
      avg_review_rating == "3.1 - 3.5" ~ (3.1 + 3.5)/2,
      avg_review_rating == "3.6 - 4" ~ (3.6 + 4)/2,
      avg_review_rating == "4.1 - 4.25" ~ (4.1 + 4.25)/2,
      avg_review_rating == "4.26 - 4.50" ~ (4.26 + 4.5)/2,
      avg_review_rating == "4.51 - 4.75" ~ (4.51 + 4.75)/2,
      avg_review_rating == "4.76+" ~ (4.76 + 5)/2,
      avg_review_rating == "No Reviews" ~ 0 
    )) %>% 
    mutate(number_of_reviews_on_the_property_num = case_when(
      number_of_reviews_on_the_property == "20+"  ~ 20,
      number_of_reviews_on_the_property == "6 to 11" ~ (6 + 11)/2,
      number_of_reviews_on_the_property == "5" ~ 5, 
      number_of_reviews_on_the_property == "12 to 19" ~ (12 + 19)/2,
      number_of_reviews_on_the_property == "4" ~ 4,
      number_of_reviews_on_the_property == "2" ~ 2,
      number_of_reviews_on_the_property == "0" ~ 0,
      number_of_reviews_on_the_property == "3" ~ 3,
      number_of_reviews_on_the_property == "1" ~ 1
    )) %>% 
    mutate(treat = if_else(online_visits == 'Yes', 1, 0))
  
  common_property_id <- intersect(
    property %>% pull(property_id) %>% unique(),
    metrics %>% pull(property_id) %>% unique()
  )
  
  property <- property %>% 
    filter(property_id %in% c(common_property_id))
  
  list(
    'metrics' = metrics,
    'property' = property
  )
}