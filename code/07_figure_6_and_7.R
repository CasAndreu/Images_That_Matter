#==============================================================================
# 07_figure_6_and_7.R
# Purpose: reproduces Figure 6, predicted values of Attention and Diffusion
#   given different emotion scores
# Author: Andreu Casas & Nora Webb Williams
#==============================================================================

# Packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Data

# - dataset with info and covariates for the original messages (n=49,345)
or_covs <- read.csv("./data/or_with_covs.csv")

# - a version of the dataset including only original messages about ShutdownA14
or_shut_covs <- or_covs %>%
  filter(shutA14 == 1)


# Models

# - loading the two mechanism models
print(load("./models/mech_rt_model_att.RData"))
print(load("./models/mech_rt_model_diff.RData"))


# Main

# - a vector with the variables of interest (emotion variables) and 
#     another vector with all the mechansism model covariates
emotions <- c("anger", "fear", "disgust", "sadness", "enthusiasm")
model_vars <- c("followers_count", "friends_count", "time_control",
                "prev_tweets", "protest", "symbol", emotions)

# - subsetting the data to inlcude only original messages that have an image
or_covs2 <- or_covs %>% filter(media_dummy == 1)
or_shut_covs2 <- or_shut_covs %>% filter(media_dummy == 1)

# - two vectors with 'generic scenarios' for each mechanism model
#     (these are scenarios in which each variable is at its mean)
generic_att <- or_covs2[1, model_vars]# placeholder values
generic_diff <- or_shut_covs2[1, model_vars]
for (var in colnames(generic_att)) {
  if (var == "time_control") { # factor variable
    generic_att[1,var] <- "t5" # giving any value
    generic_diff[1,var] <- "t5"
  } else {
    mu_att <- mean(or_covs2[, var], na.rm = TRUE)
    mu_diff <- mean(or_shut_covs2[, var], na.rm = TRUE)
    generic_att[1,var] <- mu_att
    generic_diff[1,var] <- mu_diff
  }
}

# - a dataframe of scenarios for which we want to generate predicted values
# ... initializing the scenarios matrix
scenarios <- data.frame(
  emotion = as.character(sapply(emotions, function(x) rep(x, 11))),
  score = 0:10,
  pe_att = NA,
  lwr_att = NA,
  upr_att = NA,
  pe_diff = NA,
  lwr_diff = NA,
  upr_diff = NA
)
# ... filling the rest of the values in the scenarios matrix
for (i in 1:nrow(scenarios)) {
  emotion <- as.character(scenarios$emotion[i])
  score <- scenarios$score[i]
  newdata_att <- generic_att
  newdata_att[,emotion] <- score
  newdata_diff <- generic_diff
  newdata_diff[,emotion] <- score
  pred_att <- predict(mech_rt_model_att, newdata = newdata_att, se.fit = TRUE, 
                      type = "response")
  pred_diff <- predict(mech_rt_model_diff, newdata = newdata_diff, se.fit = TRUE,
                       type = "response")
  scenarios$pe_att[i] <- pred_att$fit
  scenarios$lwr_att[i] <- pred_att$fit - (1.96 * pred_att$se.fit)
  scenarios$upr_att[i] <- pred_att$fit + (1.96 * pred_att$se.fit)
  scenarios$pe_diff[i] <- pred_diff$fit
  scenarios$lwr_diff[i] <- pred_diff$fit - (1.96 * pred_diff$se.fit)
  scenarios$upr_diff[i] <- pred_diff$fit + (1.96 * pred_diff$se.fit)
}

# - giving a 'long' format to the scenarios matrix
scenarios_long <- scenarios %>%
  gather(estimate, value, -emotion, -score)

# - cleaning the 'long' scenarios matrix
scenarios_long$est <- as.character(sapply(as.character(scenarios_long$estimate), function(x)
  strsplit(x, split = "_")[[1]][1]))
scenarios_long$model <- as.character(sapply(as.character(scenarios_long$estimate), function(x)
  strsplit(x, split = "_")[[1]][2]))

# - giving it a 'wide' format again, so it's easier for plotting
scenarios_wide <- scenarios_long %>%
  dplyr::select(-estimate) %>%
  spread(est, value)

# Plots

# - PDF: Figure 6
pdf("./images_pdf/figure_6.pdf", width = 14, height = 4.5)
ggplot(scenarios_wide %>% filter(model == "att"), aes(x = score)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray90") +
  geom_line(aes(y = pe)) +
  geom_line(aes(y = lwr), alpha = 0.5) +
  geom_line(aes(y = upr), alpha = 0.5) +
  ylab("Number of retweets") +
  xlab("Evoked emotion (0 to 10)") +
  scale_x_continuous(breaks = seq(0, 10, length.out = 5),
                     labels = c("0", "2.5", "5", "7.5", "10")) +
  facet_wrap(~ emotion, nrow = 1, ncol = 5) +
  theme(axis.text.y= element_text(size = 16),
        axis.text.x= element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.x = element_text(size = 18),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

# - PNG: Figure 6
png("./images_png/figure_6.png", width = 600, height = 250)
ggplot(scenarios_wide %>% filter(model == "att"), aes(x = score)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray90") +
  geom_line(aes(y = pe)) +
  geom_line(aes(y = lwr), alpha = 0.5) +
  geom_line(aes(y = upr), alpha = 0.5) +
  ylab("Number of retweets") +
  xlab("Evoked emotion (0 to 10)") +
  scale_x_continuous(breaks = seq(0, 10, length.out = 5),
                     labels = c("0", "2.5", "5", "7.5", "10")) +
  facet_wrap(~ emotion, nrow = 1, ncol = 5) +
  theme(axis.text.y= element_text(size = 10),
        axis.text.x= element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

# - PDF: Figure 7
pdf("./images_pdf/figure_7.pdf", width = 14, height = 4.5)
ggplot(scenarios_wide %>% filter(model == "diff"), aes(x = score)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray90") +
  geom_line(aes(y = pe)) +
  geom_line(aes(y = lwr), alpha = 0.5) +
  geom_line(aes(y = upr), alpha = 0.5) +
  ylab("Number of retweets by new ShutdownA14 users") +
  xlab("Evoked emotion (0 to 10)") +
  scale_x_continuous(breaks = seq(0, 10, length.out = 5),
                     labels = c("0", "2.5", "5", "7.5", "10")) +
  facet_wrap(~ emotion, nrow = 1, ncol = 5) +
  theme(axis.text.y= element_text(size = 16),
        axis.text.x= element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.x = element_text(size = 18),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

# - PDF: Figure 7
pdf("./images_pdf/figure_7.pdf", width = 14, height = 4.5)
ggplot(scenarios_wide %>% filter(model == "diff"), aes(x = score)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray90") +
  geom_line(aes(y = pe)) +
  geom_line(aes(y = lwr), alpha = 0.5) +
  geom_line(aes(y = upr), alpha = 0.5) +
  ylab("Number of retweets by new ShutdownA14 users") +
  xlab("Evoked emotion (0 to 10)") +
  scale_x_continuous(breaks = seq(0, 10, length.out = 5),
                     labels = c("0", "2.5", "5", "7.5", "10")) +
  facet_wrap(~ emotion, nrow = 1, ncol = 5) +
  theme(axis.text.y= element_text(size = 16),
        axis.text.x= element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.x = element_text(size = 18),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

# - PNG: Figure 7
png("./images_png/figure_7.png", width = 600, height = 250)
ggplot(scenarios_wide %>% filter(model == "diff"), aes(x = score)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray90") +
  geom_line(aes(y = pe)) +
  geom_line(aes(y = lwr), alpha = 0.5) +
  geom_line(aes(y = upr), alpha = 0.5) +
  ylab("Number of retweets by new ShutdownA14 users") +
  xlab("Evoked emotion (0 to 10)") +
  scale_x_continuous(breaks = seq(0, 10, length.out = 5),
                     labels = c("0", "2.5", "5", "7.5", "10")) +
  facet_wrap(~ emotion, nrow = 1, ncol = 5) +
  theme(axis.text.y= element_text(size = 10),
        axis.text.x= element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()
