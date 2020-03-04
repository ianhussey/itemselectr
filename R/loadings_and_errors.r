#' loadings_and_errors
#'
#' Fit a CFA with all parameters free. Return a list with the lavaan fit object and a table of factor loadings and intercepts along with their 95% CI
#' @param data Item level data for a given scale. One item per column, one row per participant. No other columns other than the scale items.
#' @examples 
#' loadings_and_errors(data)
#' @export

# use CFA to find factor loadings and errors
loadings_and_errors <- function(data) {
  
  require(tidyverse)
  require(lavaan)
  require(semPlot)
  
  # model specification
  model <- paste("L =~", paste(colnames(data), collapse = ' + '))
  
  # fit model
  fit <- cfa(model, 
             data = data) 
  
  # extract estimates
  estimates <- as.data.frame(fit@ParTable) %>%
    select(lhs, op, rhs, est, se) %>%
    filter(!(op == "~~" & lhs == "L" & rhs == "L")) %>%
    rename(item = rhs,
           estimate = est) %>%
    mutate(ci_lower = estimate - se*1.96,
           ci_upper = estimate + se*1.96,
           type = ifelse(op == "=~", "factor_loading", 
                         ifelse(op == "~~", "error", NA))) %>%
    select(item, type, estimate, ci_lower, ci_upper) 
  
  plot <- 
    estimates %>%
    mutate(type = dplyr::recode(type, 
                                `error` = "Errors",
                                `factor_loading` = "Factor loadings"),
           type = fct_relevel(type, "Factor loadings", "Errors")) %>%
    ggplot(aes(item, estimate)) +
    geom_point() +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
    facet_wrap(~type, 
               scales = "free", 
               nrow = 2, 
               ncol = 1) +
    coord_flip() +
    ylab("Estimate") +
    xlab("Item")
  
  return(results = list(estimates = estimates,
                        model_fit = fit, 
                        plot = plot))
}

