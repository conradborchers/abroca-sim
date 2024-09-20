generate_data <- function(n=1000, auc=0.8, ratio_pos_case=0.5) {
  # Numbers based on proof in Salgado et al.
  # See also: https://stats.stackexchange.com/questions/422926/generate-synthetic-data-given-auc
  t <- sqrt(log(1/(1-auc)**2))
  z <- t-((2.515517 + 0.802853*t + 0.0103328*t**2) / 
            (1 + 1.432788*t + 0.189269*t**2 + 0.001308*t**3))
  d <- z*sqrt(2)
  x <- c(rnorm(n*(1-ratio_pos_case), mean = 0), rnorm(n*ratio_pos_case, mean = d))
  y <- c(rep(0, n*(1-ratio_pos_case)), rep(1, n*ratio_pos_case))
  return(data.frame(x=x, y=y))
}

generate_predictions <- function(n_majority, n_minority, auc_majority, auc_minority, ratio_pos_case, test_set_size=0.2) {
  data0 <- generate_data(n_minority, auc_minority, ratio_pos_case)
  data0$group <- 0
  data1 <- generate_data(n_majority, auc_majority, ratio_pos_case)
  data1$group <- 1
  data <- rbind(data0, data1)
  train_idx <- sample(1:nrow(data), (1-test_set_size) * nrow(data))
  train_data <- data[train_idx,]
  test_data <- data[-train_idx,]
  model <- glm(y ~ x, data = train_data, family = binomial)
  pred <- predict(model, test_data, type = "response")
  test_data['pred'] <- pred
  return(test_data)
}

calculate_abroca <- function(test_data, randomize_groups=FALSE, return_both_aucs=FALSE) {
  if (randomize_groups)
    test_data$group <- sample(test_data$group, nrow(test_data), replace=FALSE)
  roc_all <- pROC::roc(test_data$y, test_data$pred)
  roc_0 <- pROC::roc(test_data$y[test_data$group == 0], test_data$pred[test_data$group == 0])
  roc_1 <- pROC::roc(test_data$y[test_data$group == 1], test_data$pred[test_data$group == 1])
  tpr_0_interp <- stats::approx(roc_0$specificities, roc_0$sensitivities, xout = roc_all$specificities)$y
  tpr_1_interp <- stats::approx(roc_1$specificities, roc_1$sensitivities, xout = roc_all$specificities)$y
  abroca <- pracma::trapz(roc_all$specificities, abs(tpr_0_interp - tpr_1_interp))
  if (return_both_aucs) {
    return(list(abroca=abroca, auc0=as.numeric(roc_0$auc), auc1=as.numeric(roc_1$auc)))
  }
  return(abroca)
}

simulate_abroca <- function(n_majority, n_minority, auc_majority, auc_minority, ratio_pos_case, test_set_size=0.2, return_both_aucs=FALSE) {
  test_data <- generate_predictions(n_majority, n_minority, auc_majority, auc_minority, ratio_pos_case, test_set_size=0.2)
  abroca_obs <- calculate_abroca(test_data, return_both_aucs=return_both_aucs)
  return(abroca_obs)
}
