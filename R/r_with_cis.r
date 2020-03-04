#' r_with_cis
#'
#' Bootstrapped correlations between a single item and scale sum scores. 
#' @param data data frame with two columns, scale sum_scores and scores from a single item.
#' @examples 
#' r_with_cis(data)
#' @export

# pearson's r with 95% CIs
r_with_cis <- function(data){
  
  require(tidyverse)
  require(psych)
  
  temp <- data %>%
    select(sum_score, score) %>%
    cor.ci(x = ,
           use = "pairwise.complete.obs", 
           plot = FALSE, 
           n.iter = 5000)
  
  results <- temp$ci %>%
    select(low.e, up.e) %>%
    mutate(r = temp$means)
  
  return(results)
  
}

