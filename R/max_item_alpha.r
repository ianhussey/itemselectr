#' max_item_alpha
#'
#' Calculate the max Cronbach's alpha for each item given the scale's internal consistency and each item's correlation with the sum score.
#' @param data Item level data for a given scale. One item per column, one row per participant. No other columns other than the scale items.
#' @examples 
#' max_item_alpha(data)
#' @export

# attentution correction for each item with 95% CIs
max_item_alpha <- function(data) {
  
  require(psych)
  require(tidyverse)
  
  ic_alpha <- psych::alpha(data,
                           n.iter = 5000)
  
  estimates <- data %>%
    mutate(sum_score = rowSums(.)) %>%
    gather(item, score, c(-sum_score)) %>%
    group_by(item) %>%
    do(r_with_cis(.)) %>%
    ungroup() %>%
    mutate(estimate = r^2 / ic_alpha$boot.ci[2],
           ci_lower = low.e^2 / ic_alpha$boot.ci[1],
           ci_upper = up.e^2 / ic_alpha$boot.ci[3]) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    select(-low.e, -up.e, -r)
  
  plot <- 
    ggplot(estimates, aes(item, estimate)) +
    geom_point() +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
    coord_flip() +
    ylab(expression(alpha)) +
    xlab("Item") 
  
  return(results = list(estimates = estimates,
                        plot = plot))
  
}

