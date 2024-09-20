library(tidyverse)

unlist_keep_na <- function(lst) {
  lst[sapply(lst, is.null)] <- NA
  result <- unlist(lst, use.names = FALSE)
  return(result)
}

parse_abroca_data <- function(abroca_matrix) {
  if ('matrix' %in% class(abroca_matrix)) {
    return(
      as.data.frame(t(abroca_matrix)) %>%
        mutate(abroca = unlist_keep_na(abroca), 
               auc0 = unlist_keep_na(auc0), 
               auc1 = unlist_keep_na(auc1)) %>%
        mutate(auc_delta = auc1-auc0)
    )
  }
  abroca_matrix <- abroca_matrix[!sapply(abroca_matrix, function(x) all(is.na(x)))]
  result_df <- data.frame(
    abroca = numeric(),
    auc0 = numeric(),
    auc1 = numeric(),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(abroca_matrix)) {
    element_data <- abroca_matrix[[i]]
    abroca_value <- NA
    auc0_value <- NA
    auc1_value <- NA
    if (!is.null(element_data$abroca)) {
      abroca_value <- element_data$abroca[[1]]
    }
    if (!is.null(element_data$auc0)) {
      auc0_value <- element_data$auc0[[1]]
    }
    if (!is.null(element_data$auc1)) {
      auc1_value <- element_data$auc1[[1]]
    }
    result_df <- rbind(result_df, data.frame(
      abroca = abroca_value,
      auc0 = auc0_value,
      auc1 = auc1_value
    ))
  }
  return(result_df)
}

process_auc_sim <- function(f='myoutput.rds') {
  ans <- readRDS(f) %>%
    imap_dfr(function(m, i) {
      m %>%
        parse_abroca_data() %>%
        mutate(ref=i)
    }) %>%
    separate(ref, into=c('ratio_minority', 'ratio_pos_case', 'auc_minority', 'n'), sep='-') %>%
    tibble() %>%
    mutate(auc_delta = auc1-auc0)
  return(ans)
}

d <- process_auc_sim('myoutput.rds')

# Compute skew
d %>%
  group_by(n) %>%
  summarize(skew = mean(abroca) - median(abroca))

# Plot ABROCA ~ AUC1-AUC2
ggplot(d, aes(x = auc_delta, y = abroca)) +
  geom_point(size = 1.5, alpha = 0.15) +  
  scale_color_brewer(palette = "Set2") +  
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  labs(x = expression(AUC[1] - AUC[2]), y = 'ABROCA') +
  scale_y_continuous(limits = c(0, 0.45)) +  
  theme_minimal(base_size = 12) +
  theme(legend.position = "top") + 
  facet_wrap(~n)

# Plot sampled CIs
ci_summary <- d %>%
  group_by(n) %>%
  summarize(
    ci_lower = quantile(abroca, 0.025, na.rm = TRUE),
    ci_median = quantile(abroca, 0.5, na.rm = TRUE),
    ci_upper = quantile(abroca, 0.975, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(ci_summary, aes(x = factor(n, levels=c('500', '1000', '1500')))) + # Adjust as needed
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "blue") + 
  geom_point(aes(y = ci_median), color = "blue") + 
  geom_line(aes(y = ci_median, group = 1), color = "blue") + 
  labs(
    title = "Sampled Confidence Intervals for abroca",
    x = "Sample Size (n)", # Total sample size. Multiply by test set ratio for test set N
    y = "abroca"
  ) +
  theme_minimal()
