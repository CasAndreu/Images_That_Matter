#==============================================================================
# 04_figure_4.R
# Purpose: reproduces Figure 3 of the paper, which shows the marginal effect of
#   an original tweet having an image versus not having it on the number of
#   retweets and the number of retweets by new A14 users.
# Author: Andreu Casas & Nora Webb Williams
#==============================================================================

# Packages
library(dplyr)
library(ggplot2)

# Models

# - loading the two basic models that are part of Figure 3
print(load("./models/basic_rt_model_att.RData"))
print(load("./models/basic_rt_model_diff.RData"))

# Main

# - initializing an empty scenarios matrix: 1,000 rows and 10 variables (the
#     variables in our basic models)
mmatrix <- as.data.frame(matrix(ncol = ncol(model.matrix(basic_rt_model_att)),
                                nrow = 1000))
# - adding the variable names
colnames(mmatrix) <- colnames(model.matrix(basic_rt_model_att))
# - filling the scenarios matrix with the mean values for each variable
for (var in colnames(mmatrix)){
  mmatrix[,var] <- mean(model.matrix(basic_rt_model_att)[,var], na.rm = FALSE)
}
# - varying the number of followers: we will calcualte the marginal effect
#     of a message having an image when the number of followers of
#     a user go from 1 to 100,000
mmatrix$followers_count <- seq(1, 100000, length.out = 1000)
# - initializing a results matrix with estimated marginal effects plus CIs
marf_res <- data.frame(
  followers_count = rep(mmatrix$followers_count, 2),
  model = c(rep("Attention: RTs of BLM & Shutdown14 tweets", 1000), 
            rep("Diffusion: RTs of ShutdownA14 tweets by new users", 1000)),
  pe = NA,
  lwr = NA,
  upr = NA
)
# - abstracting model Betas, vcov matrix, and SEs: and 1,000 simbetas
pe_att <- coef(basic_rt_model_att)
pe_diff <- coef(basic_rt_model_diff)
vc_att <- vcov(basic_rt_model_att)
vc_diff <- vcov(basic_rt_model_diff)
se_att <- sqrt(diag(vc_att))
se_diff <- sqrt(diag(vc_diff))
sim <- 1000
simbetas_att <- MASS::mvrnorm(sim, pe_att, vc_att)
simbetas_diff <- MASS::mvrnorm(sim, pe_diff, vc_diff)
# - looping through each scenario to calculate the marginal effect of 
#     having an image at each point (#followers)
for (i in 1:nrow(mmatrix)){
  scenario_generic <- mmatrix[i,]
  scenario0 <- scenario_generic %>% mutate(media_dummy = 0)
  scenario1 <- scenario_generic %>% mutate(media_dummy = 1)
  y0_att <- exp(as.matrix(simbetas_att) %*% as.matrix(as.numeric(scenario0)))
  y1_att <- exp(as.matrix(simbetas_att) %*% as.matrix(as.numeric(scenario1)))
  y0_diff <- exp(as.matrix(simbetas_diff) %*% as.matrix(as.numeric(scenario0)))
  y1_diff <- exp(as.matrix(simbetas_diff) %*% as.matrix(as.numeric(scenario1)))
  ydiff_att <- y1_att - y0_att
  ydiff_diff <- y1_diff - y0_diff
  marf_res$pe[marf_res$model == "Attention: RTs of BLM & Shutdown14 tweets"][i] <- mean(ydiff_att, na.rm = FALSE)
  marf_res$lwr[marf_res$model == "Attention: RTs of BLM & Shutdown14 tweets"][i] <- as.numeric(
    quantile(ydiff_att, probs = c(0.025)))
  marf_res$upr[marf_res$model == "Attention: RTs of BLM & Shutdown14 tweets"][i] <- as.numeric(
    quantile(ydiff_att, probs = c(0.975)))
  marf_res$pe[marf_res$model == "Diffusion: RTs of ShutdownA14 tweets by new users"][i] <- mean(ydiff_diff, na.rm = FALSE)
  marf_res$lwr[marf_res$model == "Diffusion: RTs of ShutdownA14 tweets by new users"][i] <- as.numeric(
    quantile(ydiff_diff, probs = c(0.025)))
  marf_res$upr[marf_res$model == "Diffusion: RTs of ShutdownA14 tweets by new users"][i] <- as.numeric(
    quantile(ydiff_diff, probs = c(0.975)))
}

# Plot

# - PDF: marginal effects plot (Figure 3)
pdf("./images_pdf/figure_3.pdf", width = 12, height = 5)
ggplot(marf_res, aes(x = followers_count, y = pe,
                     ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "gray90") +
  geom_line() +
  geom_line(aes(y = lwr), alpha = 0.3) +
  geom_line(aes(y = upr), alpha = 0.3) +
  xlab("Number of followers") +
  ylab("Marginal effect of having an image") +
  facet_wrap(~ model,scales = "free") +
  theme(axis.text.y= element_text(size = 14),
        axis.text.x= element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text.x = element_text(size = 16),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()                  


# - PNG: marginal effects plot (Figure 3)
png("./images_png/figure_3.png", width = 650, height = 250)
ggplot(marf_res, aes(x = followers_count, y = pe,
                     ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "gray90") +
  geom_line() +
  geom_line(aes(y = lwr), alpha = 0.3) +
  geom_line(aes(y = upr), alpha = 0.3) +
  xlab("Number of followers") +
  ylab("Marginal effect of having an image") +
  facet_wrap(~ model,scales = "free") +
  theme(axis.text.y= element_text(size = 10),
        axis.text.x= element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()                  
