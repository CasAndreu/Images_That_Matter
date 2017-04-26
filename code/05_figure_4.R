#==============================================================================
# 05_figure_4.R
# Purpose: reproduces Figure 4, marginal effect of an original message image 
#     having a protest or a symbol on the number of retweets and number of 
#     retweets by new A14 users.
# Author: Andreu Casas & Nora Webb Williams
#==============================================================================

# Packages
library(dplyr)
library(ggplot2)

# Models

# - loading the two mechanism models
print(load("./models/mech_rt_model_att.RData"))
print(load("./models/mech_rt_model_diff.RData"))

# Main

# - initializing an empty results matrix where we will add iteratively the 
#     marginal effect of an image having a protest or a symbol for users with
#     a varying number of followers
mar_res2 <- NULL

# - the two variables of interest in this plot
variables <- c("protest", "symbol")

# - calculating first the marginal effect on Attention (number of BLM and
#     A14 retweets)
model <- mech_rt_model_att
for (variable in variables) {
  mmatrix <- as.data.frame(matrix(ncol = ncol(model.matrix(model)),
                                  nrow = 1000))
  colnames(mmatrix) <- colnames(model.matrix(model))
  for (var in colnames(mmatrix)){
    mmatrix[,var] <- mean(model.matrix(model)[,var], na.rm = FALSE)
  }
  # ... varying the number of followers
  mmatrix$followers_count <- seq(1, 400000, length.out = 1000)
  co <- coef(model)
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  sim <- 1000
  simbetas <- MASS::mvrnorm(sim, co, vc)
  x0 <- mmatrix
  x0[,variable] <- 0
  x1 <- mmatrix
  x1[,variable] <- 1
  for (i in 1:nrow(x0)) {
    y0 <- exp(as.matrix(simbetas) %*% as.matrix(as.numeric(x0[i,])))
    y1 <- exp(as.matrix(simbetas) %*% as.matrix(as.numeric(x1[i,])))
    ydiff <- y1 - y0
    pe <- mean(ydiff)
    lwr <- quantile(ydiff, probs = 0.025)
    upr <- quantile(ydiff, probs = 0.975)
    newrow <- data.frame(
      model = names(model$model)[1],
      var = variable,
      followers_count = x0$followers_count[i],
      pe = pe,
      lwr = lwr, 
      upr = upr
    )
    mar_res2 <- rbind(mar_res2, newrow)
  }
}

# - calculating now the marginal effect on Diffusion (number of
#     A14 retweets by new A14 users)
model <- mech_rt_model_diff
for (variable in variables) {
  mmatrix <- as.data.frame(matrix(ncol = ncol(model.matrix(model)),
                                  nrow = 1000))
  colnames(mmatrix) <- colnames(model.matrix(model))
  for (var in colnames(mmatrix)){
    mmatrix[,var] <- mean(model.matrix(model)[,var], na.rm = FALSE)
  }
  # ... varying the number of followers
  mmatrix$followers_count <- seq(1, 400000, length.out = 1000)
  co <- coef(model)
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  sim <- 1000
  simbetas <- MASS::mvrnorm(sim, co, vc)
  x0 <- mmatrix
  x0[,variable] <- 0
  x1 <- mmatrix
  x1[,variable] <- 1
  for (i in 1:nrow(x0)) {
    y0 <- exp(as.matrix(simbetas) %*% as.matrix(as.numeric(x0[i,])))
    y1 <- exp(as.matrix(simbetas) %*% as.matrix(as.numeric(x1[i,])))
    ydiff <- y1 - y0
    pe <- mean(ydiff)
    lwr <- quantile(ydiff, probs = 0.025)
    upr <- quantile(ydiff, probs = 0.975)
    newrow <- data.frame(
      model = names(model$model)[1],
      var = variable,
      followers_count = x0$followers_count[i],
      pe = pe,
      lwr = lwr, 
      upr = upr
    )
    mar_res2 <- rbind(mar_res2, newrow)
  }
}

# - providing better variable/value labels
mar_res2$model <- ifelse("retweet_n" == mar_res2$model, "Attention: RTs of BLM & Shutdown14 tweets",
                         "Diffusion: RTs of ShutdownA14 tweets by new users")

# Plot

# - PDF: Figure 4
pdf("./images_pdf/figure_4.pdf", width = 14, height = 5)
ggplot(mar_res2, aes(x = followers_count, y = pe, ymin = lwr, ymax = upr,
                     fill = var)) +
  #geom_line(y = pe) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_line(aes(y = lwr), alpha = 0.3) +
  geom_line(aes(y = upr), alpha = 0.3) +
  xlab("Number of Followers") +
  ylab("Marginal effect of an image having a protest or a symbol") +
  scale_fill_manual("Explanatory Variable", values = c("gray30", "gray70")) +
  facet_wrap(~ model, scale = "free") +
  theme(axis.text.y= element_text(size = 14),
        axis.text.x= element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text.x = element_text(size = 16),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

# - PNG: Figure 4
png("./images_png/figure_4.png", width = 750, height = 250)
ggplot(mar_res2, aes(x = followers_count, y = pe, ymin = lwr, ymax = upr,
                     fill = var)) +
  #geom_line(y = pe) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_line(aes(y = lwr), alpha = 0.3) +
  geom_line(aes(y = upr), alpha = 0.3) +
  xlab("Number of Followers") +
  ylab("Marginal effect of an image having a protest or a symbol") +
  scale_fill_manual("Explanatory Variable", values = c("gray30", "gray70")) +
  facet_wrap(~ model, scale = "free") +
  theme(axis.text.y= element_text(size = 10),
        axis.text.x= element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()
