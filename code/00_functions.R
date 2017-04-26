#==============================================================================
# 00_funcitons.R
# Purpose: functions we use in other scripts of this project.
# Author: Andreu Casas & Nora Webb Williams
#==============================================================================

# 'get_standard_coef_nb()'
# A function that takes a dataset and a NB model, and calculates and returns
#   standardized coefficients and 95%CIs when each var moves from the mean
#   to +1sd, all else constant.
get_standard_coef_nb <- function(model) {
  dataset <- model.matrix(model)
  require(MASS)
  coef_names <- names(coefficients(model))
  pe <- coef(model)
  vc <- vcov(model)
  sims <- 100000
  simbetas <- mvrnorm(sims, pe, vc)
  var_names <- names(pe)
  #var_names <- coef_names[-which(substring(coef_names, first = 1, last = 1) == "(")]
  var_means <- sapply(var_names, function(x) mean(dataset[,x], na.rm = TRUE))
  var_sd <- sapply(var_names, function(x) sd(dataset[,x], na.rm = TRUE))
  newdata <- as.data.frame(matrix(ncol = 2,nrow = length(var_means),
                                  data = rep(var_means, 2)))
  newdata <- t(newdata)
  colnames(newdata) <- var_names
  output <- as.data.frame(matrix(ncol = 4, nrow = length(var_names)))
  colnames(output) <- c("var", "pe", "lwr", "upr")
  output$var <- var_names
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    var_newdata <- newdata
    var_newdata[2, var] <- var_means[i] + var_sd[i]
    pred_mean <- simbetas %*% var_newdata[1,]
    pred_1sd <- simbetas %*% var_newdata[2,]
    pred_effect <- exp(pred_1sd) - exp(pred_mean)
    output[output$var == var, "pe"] <- mean(pred_effect)
    output[output$var == var, "lwr"] <- quantile(pred_effect, probs = 0.025)
    output[output$var == var, "upr"] <- quantile(pred_effect, probs = 0.975)
  }
  return(output)
}

