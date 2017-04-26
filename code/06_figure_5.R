#==============================================================================
# 06_figure_5.R
# Purpose: reproduces Figure 5, coefficients plot for the Mechanism Models
# Author: Andreu Casas & Nora Webb Williams
#==============================================================================

# Packages
library(dplyr)
library(ggplot2)
source("./code/00_functions.R")

# Models

# - loading the two mechanism models
print(load("./models/mech_rt_model_att.RData"))
print(load("./models/mech_rt_model_diff.RData"))

# Main

# - getting standrad coefficients: using a function 'get_standard_coef_nb()'
#     from our '00_functions.R' script
mech_att <- cbind(get_standard_coef_nb(mech_rt_model_att), 
                  data.frame(
                    model = "Attention: BLM + ShutdownA14 tweets (mechanisms)"))
mech_diff <- cbind(get_standard_coef_nb(mech_rt_model_diff), 
                   data.frame(
                     model = "Diffusion: ShutdownA14 new users (mechanisms)"))
# - merging the coefficients of the two models into the same data frame
all_std_models <- rbind(mech_att, mech_diff)

# - providing better labels to varaibles/values
all_std_models <- all_std_models %>%
  mutate(var = ifelse(var == "followers_count", "Number of followers", var),
         var = ifelse(var == "friends_count", "Number of friends", var),
         var = ifelse(var == "prev_tweets", "Previous Tweets", var),
         var = ifelse(var == "media_dummy", "Image", var),
         var = ifelse(var == "anger", "Anger", var),
         var = ifelse(var == "disgust", "Disgust", var),
         var = ifelse(var == "enthusiasm", "Enthusiasm", var),
         var = ifelse(var == "fear", "Fear", var),
         var = ifelse(var == "sadness", "Sadness", var),
         var = ifelse(var == "symbol", "Symbol", var),
         var = ifelse(var == "protest", "Protest", var))

# - sorting the coavraites so they are in the same order in all models
covs_levels <- unique(all_std_models$var)
all_std_models$var <- factor(all_std_models$var, levels = rev(covs_levels))

# Plot

# - PDF: coefficients plot (Figure 5)
pdf("./images_pdf/figure_5.pdf", width = 11.5, height = 4)
ggplot(all_std_models %>% filter(!grepl("basic", model),
                                 !grepl("Previous", var),
                                 !grepl("Intercept", var),
                                 !grepl("Number of followers", var),
                                 !grepl("time_", var), 
                                 !grepl("friends", var)),
       aes(x = var, y = pe, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ model) +
  xlab("") +
  ylab("Number of retweets") +
  theme(axis.text.y= element_text(size = 14),
        axis.text.x= element_text(size = 14),
        strip.text.x = element_text(size = 14),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

# - PNG: coefficients plot (Figure 5)
png("./images_png/figure_5.png", width = 800, height = 300)
ggplot(all_std_models %>% filter(!grepl("basic", model),
                                 !grepl("Previous", var),
                                 !grepl("Intercept", var),
                                 !grepl("Number of followers", var),
                                 !grepl("time_", var), 
                                 !grepl("friends", var)),
       aes(x = var, y = pe, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ model) +
  xlab("") +
  ylab("Number of retweets") +
  theme(axis.text.y= element_text(size = 14),
        axis.text.x= element_text(size = 14),
        strip.text.x = element_text(size = 14),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()
