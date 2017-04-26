#==============================================================================
# 02_figure_2.R
# Purpose: reproduces Figure 2 of the paper
# Author: Andreu Casas & Nora Webb Williams
#==============================================================================

# Packages
library(dplyr)
library(ggplot2)

# Data

# - dataset containing basic information for all the  messages (original tweets
#     and retweets)
db <- read.csv("./data/all_messages.csv")

# Data Wrangling

# - preparing the dataset for the plot
plot_db <- db %>%
  # ... selecting only variables of interest for the plot
  dplyr::select(media_dummy, retweet_dummy, shutA14) %>%
  # ... providing better variables/values labels
  mutate(media_dummy = ifelse(media_dummy == 1, "image", "no image"),
         retweet_dummy = ifelse(retweet_dummy == 1, "Retweets (RT)", "Original messages (OR)"),
         shutA14 = ifelse(shutA14 == 1, "ShutdownA14", "BLM"))

# Plot

# - PDF version
pdf("./images_pdf/figure_2.pdf", width = 12, height = 5)
ggplot(plot_db, aes(x = retweet_dummy, fill = media_dummy)) +
  geom_bar(position = "dodge") +
  xlab("") +
  ylab("Number of messages") +
  facet_wrap(~ shutA14) +
  scale_fill_manual("", values = c("gray40", "gray80")) +
  theme(axis.text.y= element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x= element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

# - PNG version
png("./images_png/figure_2.png", width = 600, height = 250)
ggplot(plot_db, aes(x = retweet_dummy, fill = media_dummy)) +
  geom_bar(position = "dodge") +
  xlab("") +
  ylab("Number of messages") +
  facet_wrap(~ shutA14) +
  scale_fill_manual("", values = c("gray40", "gray80")) +
  theme(axis.text.y= element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text.x= element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill = NA),
        strip.background = element_rect("white"))
dev.off()

