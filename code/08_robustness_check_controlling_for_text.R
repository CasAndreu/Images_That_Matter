#===============================================================================
# 08_robustness_check_controlling_for_text.R
# Purpose: fitting an unsupervised topic model (LDA) to the text of the messages
#   and re-running the paper model to control for the effect of text
# Author: Andreu Casas & Nora Webb Williams
#===============================================================================

# PACKAGES
library(dplyr)
library(stringr)
library(quanteda)
library(qdap)
library(tm)
library(topicmodels)
library(tidytext)

# DATA
or_covs <- read.csv("./data/or_with_covs.csv")

# DATA WRANGLING

# - pulling the text messages and the hashtags in a separate vector
text <- as.character(or_covs$text)
hashtags <- as.character(or_covs$hashtags)
hashtags[which(hashtags == "")] <- NA

# - preprocessing:
    # - the text is already all lower case

    # - remove hashtags: I'll bring them back later 
text_nohash <- gsub("#[a-z]{1,}", "", text)

    # - removing usernames: not very likely these will have topic-related info
text_nousers <- gsub("@[a-z]{1,}", "", text_nohash)

    # - removing links: although they may be topic-related, it's possible that 
    #     the same final url has different links due to link shorteners and so
    #     adding links may add noise.
text_nolinks <- gsub("http\\S+\\s*", "", text_nousers)

    # - removing punctuation (but hashtags): punctuation signs may be useful to
    #     estiamte tone but are unlikely to provide topic information. Moreover,
    #     leaving punctuation adds noise by treating "token." and "token" as 
    #     different words.
text_nopunct <- gsub( "[^[:alnum:]#]", " ", text_nolinks)

    # - removing numbers: have no reason to believe numbers will provide relevant
    #     topic information.
text_nonum <- gsub( "[0-9]", "", text_nopunct)

# - removing stopwords: not topic related and add a lot of noise
stopw <- c(quanteda::stopwords("english"), "")
text_nostop <- as.character(sapply(text_nonum, function(x)
  paste0(strsplit(x, split = " ")[[1]][which(!(strsplit(x, split = " ")[[1]]) %in%
                                        stopw)], collapse = " ")))

    # - stemming: words with the same root are highly likely to belong to the 
    #     same topic. Transforming them to a single word in order to facilitate
    #     topic estimation.
text_stem <- qdap::stemmer(text_nostop, capitalize = FALSE, warn = FALSE)

    # - adding the hashtags back it: because they are likely to be topic-related
final_text <- as.character(sapply(1:length(text_stem), function(i) 
  paste(text_stem[i], paste0("#", strsplit(gsub(",", "", hashtags[i]), 
                                           split = " ")[[1]],
                              collapse = " "))))

final_text <- gsub(" #NA", "", final_text)
final_text <- gsub("NA?\\s", "", final_text)
final_text <- gsub("NA", "", final_text)
final_text[which(final_text == "")] <- NA
text_stem[which(text_stem == "NA")] <- NA

# - saving the pre-processed text in the main db
or_covs$text_preproc <- final_text
messages <- final_text
messages_nohash <- as.character(na.omit(text_stem))

#write.csv(or_covs, "./data/or_with_covs.csv", row.names = FALSE)

# - removing messages that have no text
messages_nohash <- as.character(na.omit(messages_nohash))

# - creating a Document-Term-Matrix with the pre-processed text
corpus <- Corpus(VectorSource(messages_nohash))
dtm <- DocumentTermMatrix(corpus)

# - checking for empty rows in the dtm
rows_sum <- as.numeric(apply(dtm, 1, sum))
final_messages <- messages_nohash[-c(which(rows_sum == 0))]
corpus <- Corpus(VectorSource(final_messages))
dtm <- DocumentTermMatrix(corpus)

# MAIN

# - fitting an LDA model to the text
k = 5
tmodel <- topicmodels::LDA(dtm, k = k, control = list(seed = 123))

# @beta: the word-topic probabilities
# @gamma: the doc-topic probabilities 

# - exporting a csv with the top word-topic probabilities by topic
words_vec <- tmodel@terms
wtpr_long <- tidy(tmodel, matrix = "beta")
wtpr_topterms <- NULL

for (i in 1:k) {
  # - selecting 1 topic
  wtpr_onetopic <- wtpr_long %>% 
    filter(topic == i)
  # - sort by word probability
  wtpr_onetopic <- wtpr_onetopic %>%
    arrange(desc(beta))
  # - only keeping the top 20 terms
  wtpr_onetopic_topterms <- as.data.frame(wtpr_onetopic[1:20,] %>%
                                            mutate(beta = round(beta, 3)) %>%
                                            rename(pr = beta))
  # - transform the output to a single column
  wtpr_onetopic_topterms_vec <- paste0(
    wtpr_onetopic_topterms$term, " (", wtpr_onetopic_topterms$pr, ")"
  )
  # - adding the column to the results df outside of the loop
  wtpr_topterms <- cbind(wtpr_topterms, wtpr_onetopic_topterms_vec)
}

colnames(wtpr_topterms) <- paste0("topic", 1:k)
View(wtpr_topterms)

# - saving a copy of this topic model
#save(tmodel, file = "./data/topicmodel_k5.Rdata")
#write.csv(wtpr_topterms, "./data/wtpr_topterms_k5.csv", row.names = FALSE)

# - the probaiblity of each document to belong to each topic
dtpr <- tidy(tmodel, matrix = "gamma") %>%
  mutate(document = as.numeric(as.character(document))) %>%
  arrange(document)

#===============================================================================
#===============================================================================
# Only-Hashtags topic model

# - removing main hashtags (not much topic-related information)
all_hash_string <- gsub(" NA,", "", paste0(hashtags, collapse = ", "))
all_hash_u <- data.frame(hash = strsplit(all_hash_string, split = ", ")[[1]])

hash_tab <- all_hash_u %>%
  filter(hash != "", hash != "_") %>%
  group_by(hash) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# - messages as a string of only hashtags
hash_mess <- as.character(sapply(hashtags, function(x)
  paste0(strsplit(x, split = ", ")[[1]], collapse = " ")))

# - removing the messages with no hashtags
hash_mess2 <- hash_mess[hash_mess != "NA"]

# - building the Document-Term-Matrix
corpus <- Corpus(VectorSource(hash_mess2))
dtm <- DocumentTermMatrix(corpus)

# - checking for empty rows in the dtm
rows_sum <- as.numeric(apply(dtm, 1, sum))

final_hash_mess2 <- hash_mess2[-c(which(rows_sum == 0))]
corpus <- Corpus(VectorSource(final_hash_mess2))
dtm <- DocumentTermMatrix(corpus)

# MAIN

# - fitting an LDA model to the text
k = 5
tmodel_hash <- topicmodels::LDA(dtm, k = k, control = list(seed = 123))

# @beta: the word-topic probabilities
# @gamma: the doc-topic probabilities 

# - exporting a csv with the top word-topic probabilities by topic
words_vec <- tmodel_hash@terms
wtpr_long <- tidy(tmodel_hash, matrix = "beta")
wtpr_topterms <- NULL

for (i in 1:k) {
  # - selecting 1 topic
  wtpr_onetopic <- wtpr_long %>% 
    filter(topic == i)
  # - sort by word probability
  wtpr_onetopic <- wtpr_onetopic %>%
    arrange(desc(beta))
  # - only keeping the top 20 terms
  wtpr_onetopic_topterms <- as.data.frame(wtpr_onetopic[1:20,] %>%
                                            mutate(beta = round(beta, 3)) %>%
                                            rename(pr = beta))
  # - transform the output to a single column
  wtpr_onetopic_topterms_vec <- paste0(
    wtpr_onetopic_topterms$term, " (", wtpr_onetopic_topterms$pr, ")"
  )
  # - adding the column to the results df outside of the loop
  wtpr_topterms <- cbind(wtpr_topterms, wtpr_onetopic_topterms_vec)
}

colnames(wtpr_topterms) <- paste0("topic", 1:k)
View(wtpr_topterms)

# - saving a copy of this topic model
#save(tmodel, file = "./data/topicmodel_hash_k20.Rdata")
#write.csv(wtpr_topterms, "./data/wtpr_topterms_hash_k20.csv", row.names = FALSE)






