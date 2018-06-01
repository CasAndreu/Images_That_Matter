#==============================================================================
# 01_figure_1.R
# Purpose: reproduces Figure 1 of the paper.
# Instructions: Figure 1 is composed of 2 plots. Here we provide the code
#   to reproduce both, Figure 1a and Figure 1b.
# Author: Andreu Casas & Nora Webb Williams
#==============================================================================

# Data

# Load the main dataset containing information about each 30-minute period
results <- read.csv("./data/results.csv")

# A) Plot showing showing the ATTENTION (# of tweets) to BLM + A14:
colorpal <- c("gray90", "gray70", "gray50", "black")
pdf("./images_pdf/figure_1a.pdf", width = 10, height = 6)
#png("./images_png/figure_1a.png", width = 1000, height = 600)
par(mar = c(7,5,2,5))
plot(results$tweets, type = 'l', axes = FALSE,
     ylab = "Tweets (n)", xlab = "", ylim = c(0, 3500), 
     xlim = c(0, 333), cex.lab = 1.4)
axis(2,las=2,cex.axis=1.2, pos = 2)
axis(1, at = c(-1000, 1000), pos = 1)
ticks8am <- c(31, 78, 126, 173, 220, 268, 315)
labs <- c("Apr 14 (8am)", "Apr 15 (8am)", "Apr 16 (8am)", "Apr 17 (8am)",
          "Apr 18 (8am)", "Apr 19 (8am)", "Apr 20 (8am)")
axis(1, at = ticks8am, # These are the 8am break numbers
     labels = rep("",length(ticks8am)), pos = 2)
pimagesbr <- seq(min(results$pimages),max(results$pimages),length.out=5)
for (i in 1:nrow(results)) {
  color <- colorpal[length(which(pimagesbr < results$pimages[i]))]
  polygon(x=c(c(i,i+1),c(i+1,i)),
          y=c(c(0,0),c(results$tweets[i+1],results$tweets[i])),
          border=F,
          col=color)
}
text(ticks8am-15, y = -500, cex=1,
     labels = labs, srt = 55, pos = 1, xpd = TRUE)
legend(x=230,y=3400,bty='n', cex = 1.4, 
       title = "% tweets with an image",
       legend=c('(0% - 16%]',
                '(16% - 32%]',
                '(32% - 49%]',
                '(49% - 65%]'),
       col= colorpal,pch=15,pt.cex=2,y.intersp=2)
dev.off()

# B) Plot showing the DIFFUSION (# new unique A14 users) of the April14 protest:
par(mar = c(7,5,2,5))
pdf("./images_pdf/figure_1b.pdf", width = 10, height = 6)
#png("./images_png/figure_1b.png", width = 1000, height = 600)
plot(results$users_shut_accum, type = 'l', axes = FALSE, 
     ylab = "Unique users (n)", xlab = "", ylim = c(0, 25000), 
     xlim = c(0, 333), cex.lab = 1.4)
axis(2, at = seq(0,25000,5000), labels = c("",seq(5000, 25000,5000)),
     las = 2, cex.axis=1.3, pos = 1)
axis(1, at = c(-1000, 1000), pos = 2, cex.axis =1.4)
for (i in 1:nrow(results)) {
  color <- colorpal[length(which(pimagesbr < results$pimages[i]))]
  polygon(x=c(c(i,i+1),c(i+1,i)),
          y=c(c(0,0),c(results$users_shut_accum[i+1],results$users_shut_accum[i])),
          border=F,
          col=color)
}
ticks8am <- c(31, 78, 126, 173, 220, 268, 315)
labs <- c("Apr 14 (8am)", "Apr 15 (8am)", "Apr 16 (8am)", "Apr 17 (8am)",
          "Apr 18 (8am)", "Apr 19 (8am)", "Apr 20 (8am)")
axis(1, at = ticks8am, # These are the 8am break numbers
     labels = rep("",length(ticks8am)), pos = 2, cex.axis = 1.4)
text(ticks8am-15, y = -3500, cex=1,
     labels = labs, srt = 55, pos = 1, xpd = TRUE)
dev.off()
