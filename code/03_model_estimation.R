#===============================================================================
# 03_model_estimation.R
# Purpose: fitting the models of the paper (Model 1, 2, 3, and 4)
# Author: Andreu Casas & Nora Webb Williams
#===============================================================================

# Packages
library(dplyr)
library(ggplot2)
library(MASS)

# Data

# - dataset with info and covariates for the original messages (n=49,345)
or_covs <- read.csv("./data/or_with_covs.csv")

# - a version of the dataset including only original messages about ShutdownA14
or_shut_covs <- or_covs %>%
  filter(shutA14 == 1)

# Model Esimation

# - Model 1: Basic Attention Model
# ... outcome var: number of retweets of BLM and A14 original messages
basic_rt_model_att <- glm.nb(retweet_n ~ followers_count +
                               prev_tweets +
                               friends_count +
                               time_control +
                               media_dummy, control=glm.control(maxit=300),
                             data = or_covs)

# - Model 2: Basic Diffusion Model
# ... outcome var: number of retweets of A14-only original messages by 
#       new users (users who messages about A14 for the first time)
basic_rt_model_diff <- glm.nb(retweet_new_shut_n ~ followers_count + 
                                prev_tweets +
                                friends_count + 
                                time_control +
                                media_dummy, control=glm.control(maxit=300),
                              data = or_shut_covs)

# - Model 3: Mechanisms Attention Model
# ... outcome var: number of retweets of BLM and A14 original messages
mech_rt_model_att <- glm.nb(retweet_n ~ followers_count +
                              friends_count +
                              prev_tweets +
                              time_control +
                              protest +
                              symbol +
                              anger +
                              fear +
                              disgust +
                              sadness +
                              enthusiasm,
                            control=glm.control(maxit=200), # Need to increase ...
                            data = or_covs)   # ... iterations in order to converge

# - Model 4: Mechanisms Diffusion Model
# ... outcome var: number of retweets of A14-only original messages by 
#       new users (users who messages about A14 for the first time)
mech_rt_model_diff <- glm.nb(retweet_new_shut_n ~ followers_count +
                               friends_count +
                               prev_tweets +
                               time_control +
                               protest +
                               symbol +
                               anger +
                               fear +
                               disgust +
                               sadness +
                               enthusiasm, 
                             control=glm.control(maxit=200),
                             data = or_shut_covs)

# Saving the models
save(basic_rt_model_att, file = "./models/basic_rt_model_att.RData")
save(basic_rt_model_diff, file = "./models/basic_rt_model_diff.RData")
save(mech_rt_model_att, file = "./models/mech_rt_model_att.RData")
save(mech_rt_model_diff, file = "./models/mech_rt_model_diff.RData")
