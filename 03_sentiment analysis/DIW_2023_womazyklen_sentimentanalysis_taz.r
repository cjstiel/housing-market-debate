# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: SENTIMENT ANALYSIS (taz)
#
# 		OUTLINE: PART 1: Define stemming and tokenization functions
#				 PART 2: Load stopword list
#				 PART 3: Load polarity of words list (sentiment lexicon)
#				 PART 4: Prepare sentiment lexicon
#				 PART 5: Read-in text from articles
#				 PART 6: Pre-process text data using the tidyr-package
#				 PART 7: Run sentiment analysis: Calculate sentiment scores
#						7.1 Loop through all articles
#						7.2 Define sentiment measures
#				 PART 8: Explorative analysis of results
# 						8.1 Distribution of sentiment measures
# 						8.2 Most positive and negative articles
# 						8.3 Most important positive and negative words
#						8.4 Matches with dictionary
#
# -------------------------------------------------------------------------------
# code author: Konstantin Kholodilin, Felix Aubele, and Caroline Stiel (DIW Berlin)
#
# First created --- May 25, 2022									
# Last modified --- June 10, 2022 (cs)
# ------------------------------------------------------------------------------
# content: This code conducts the sentiment analysis for the housing articles 
# from 'taz' using the new strategy to identify housing articles.
# ==============================================================================

# ==============================================================================
# 0) Initial settings
# ==============================================================================

# clean
# -----
rm(list = ls())


# Use fixed notation instead of exponential notation
#---------------------------------------------------
options(scipen = 999)

# record process time
# -------------------
Begin = proc.time()


# load libraries
# --------------
library(tm)
library(openxlsx)
library(stringr) # To use function str_replace
library(XML)
library(udpipe) # To stem German words
library(tidyverse)
library(stopwords)
library(data.table)
library(splitstackshape)
library(dplyr)
library(tidyr)
library(tidytext)
library(quanteda)

# set R system language (including dates etc.) to German
# ------------------------------------------------------
Sys.setlocale('LC_ALL', 'german')

# choose dictionary
# -----------------
dict = "SentiMerge"

# choose lemmatization procedure
# -------------------------------
lemma = "with"

# define paths and data input/output names
# ----------------------------------------
sFolder1 = "" # path where data, drafts etc. are stored
sFolder2 = "" # path with local repo

sInFile1 = "data pre-processing/German_specific_letters.xlsx"
sInFile2 = "Data/SentiWS/SentiWS_v2.0_all_terms.xlsx"
sInFile3 = "Data/Rauh_GermanSentimentDict/Rauh_SentDictionaryGerman.xlsx"
sInFile4 = "Data/Rauh_GermanSentimentDict/1_Dictionaries/GermanPolarityClues-Positive-21042012.tsv"
sInFile5 = "Data/Rauh_GermanSentimentDict/1_Dictionaries/GermanPolarityClues-Negative-21042012.tsv"
sInFile6 = "Data/SentiMerge/sentimerge.xlsx"
sInFile7 = "Data/Sentistrength/EmotionLookupTable_v5_fullforms.txt"
sInFile8 = "Data/taz/taz_Housing_articles_unambiguous_withtopics_2022-06-09.rds"

sOutFile1 = paste0("Data/taz/articlelists_sentiment/Sentiment_taz_Housing_", lemma, "_lemma_",dict,".xlsx")
sOutFile2 = paste0("Data/taz/articlelists_sentiment/Words_positive_Sentiment_taz_Housing_", lemma, "_lemma_",dict,".txt")
sOutFile3 = paste0("Data/taz/articlelists_sentiment/Words_negative_Sentiment_taz_Housing_", lemma, "_lemma_",dict,".txt")
sOutFile4 = paste0("Data/taz/articlelists_sentiment/Words_positive_Sentiment_taz_Housing_", lemma, "_lemma_",dict,".rds")
sOutFile5 = paste0("Data/taz/articlelists_sentiment/Words_negative_Sentiment_taz_Housing_", lemma, "_lemma_",dict,".rds")


# ==============================================================================
# 1) Define stemming and tokenization functions
# ==============================================================================

# lemmatization function using udpipe
# -----------------------------------
mylem <- function(doc) {
# apply udpipe function (runs lemmatization)
  sdoc = udpipe_annotate(ud_german, as.character(doc))
  sdoc = as.data.frame(sdoc)
  # select lemmatization
  sDoc = paste(sdoc$lemma, collapse = " ")
  #attributes(sdoc) <- attributes(doc)
  sDoc
}

# ==============================================================================
# 2) Load stopword list
# ==============================================================================

# use stopword list from stopword package
# -----------------------------------------------------
# combine all dictionaries to obtain maximum of stopwords
Stopwords_pck = c(stopwords::stopwords("de", source="marimo")
                  , stopwords::stopwords("de", source="snowball")
                  ,stopwords::stopwords("de", source="nltk")
                  ,stopwords::stopwords("de", source="stopwords-iso"))

Stopwords_pck = unique(Stopwords_pck)

# Add user-specific stopwords
# ---------------------------
Stopwords_pck = c("prozent", "dr","st","nnen","taz",Stopwords_pck)
Stopwords_pck = sort(Stopwords_pck)
Stopwords_pck

# Replace Umlaute
# ---------------
Stopwords_pck = gsub("ä", "ae", Stopwords_pck)
Stopwords_pck = gsub("Ä", "Ae", Stopwords_pck)
Stopwords_pck = gsub("Ö", "Oe", Stopwords_pck)
Stopwords_pck = gsub("Ü", "Ue", Stopwords_pck)
Stopwords_pck = gsub("ö", "oe", Stopwords_pck)
Stopwords_pck = gsub("ü", "ue", Stopwords_pck)
Stopwords_pck = gsub("ß", "ss", Stopwords_pck)

# Convert to data frame
# ---------------------
Stopwords_pck = data.frame("word"=c(Stopwords_pck))
head(Stopwords_pck,5)


# ==============================================================================
# 3) Load polarity of words list (sentiment lexicon)
# ==============================================================================

# load dictionary based on initialization
# ---------------------------------------

  # SentiWS
  # -------
if (dict == "SentiWs"){
  PosNeg = read.xlsx(paste(sFolder1, sInFile2, sep=""))
  #use lemmatized words if articles text will be stemmed
  if (stemming == "with"){
    PosNeg$Word = PosNeg$Word_reduced
  }
  
  # Rauh
  # ----
} else if (dict == "Rauh"){
  PosNeg = read.xlsx(paste(sFolder1, sInFile3, sep = ""))
  PosNeg$Word = PosNeg$feature
  PosNeg$Value = as.integer(PosNeg$sentiment)
  
  # GPC
  # ---
} else if (dict == "GPC"){
  # load positive words list
  Pos.words= as.data.frame(fread(paste(sFolder1, sInFile4, sep=""), encoding="UTF-8", header=F))
  # define global variable 'polarity' which is 1 for all positive words
  Pos.words$polarity <- 1
  # summarize info on positive words in a data frame
  pos <- data.frame(Word = Pos.words$V1,Word_reduced = Pos.words$V2
                    ,Value = Pos.words$polarity)
  
  # load negative words
  Neg.words = as.data.frame(fread(paste(sFolder1, sInFile5, sep=""), encoding="UTF-8", header=F))
  # define global variable 'polarity' which is -1 for all negative words
  Neg.words$polarity <- -1
  # summarize info on negative words in a data frame
  neg <-  data.frame(Word = Neg.words$V1,Word_reduced = Neg.words$V2
                     ,Value = Neg.words$polarity)
  # stack positive and negative words together
  PosNeg <- rbind(pos, neg)

  # SentiMerge
  # ----------
} else if(dict == "SentiMerge"){
  PosNeg = read.xlsx(paste0(sFolder1,sInFile6))
  PosNeg$Word <-  PosNeg$lemma
  PosNeg$Value <- PosNeg$sentiment
  
  # SSG
  # ----
} else if(dict == "SSG"){
  PosNeg = read.delim(paste(sFolder1, sInFile7, sep=""), encoding="UTF-8", sep="\t", skip=46, header=F)
  PosNeg$Word <-  PosNeg$V1
  PosNeg$Value <- PosNeg$V2
  
}

# ==============================================================================
# 4) Prepare sentiment lexicon
# ==============================================================================


# load German-specific letters
# ----------------------------
GeLe = read.xlsx(paste(sFolder2, sInFile1, sep=""))
NGeLe = nrow(GeLe)

# replace German-specific letters in sentiment lexicon
# ------------------------------------------------------
for(iGL in 1:NGeLe)
{
  sLet_deu = GeLe$German[iGL]
  sLet_lat = GeLe$Transcription[iGL] 
  PosNeg$Word = gsub(sLet_deu, sLet_lat, PosNeg$Word)
}

# print words with max positve and max negativ sentiment
# ------------------------------------------------------
Pos = PosNeg[which(PosNeg$Value>0),]
Neg = PosNeg[which(PosNeg$Value<0),]
Pos[which(Pos$sentiment == max(Pos$sentiment)),]
Neg[which(Neg$sentiment == min(Neg$sentiment)),]



# ==============================================================================
# 5) Read-in text from articles
# ==============================================================================

# Load all taz articles
# ------------------------------
X_sel = readRDS(paste0(sFolder1, sInFile8))


# ==============================================================================
# 6) Pre-process text data using the tidyr-package
# ==============================================================================

# transform article collection into data frame
# --------------------------------------------
taz_df = tibble(id=1:length(X_sel$text),text=X_sel$text)
head(taz_df,5)


# perform lemmatization
# ----------------------
NCorpus = nrow(taz_df)
taz_df_lem <- taz_df
if(lemma == "with"){
  ud_model <- udpipe_download_model(language="german")
  ud_german <- udpipe_load_model(ud_model$file_model)
  for(jA in 1:NCorpus)
  {
    taz_df_lem[jA,2] = mylem(taz_df[jA,2])
    # show progress
    cat("\r",jA*100/NCorpus," % done ");flush.console()
  }
  if(file.exists(ud_model$file_model)) {file.remove(ud_model$file_model)}
}

# split into words (tokens), strip punctuation, and convert to lowercase
# ----------------------------------------------------------------------
# Notes: set to_lower=FALSE if you do not want to convert to lowercase
# Notes: unnest_tokens() splits words at German Umlaute (e.g. grer = 'gr'+'er').
# Therefore, replace German Umlaute with 'ae' etc. before pre-processing. Do the
# same for stopwords.

taz_tokens <- taz_df_lem %>% unnest_tokens(word, text)
head(taz_tokens, 10)

# remove stopwords
# ----------------
taz_sw <- taz_tokens %>% anti_join(Stopwords_pck)
head(taz_sw,5)

# compute total frequencies
# --------------------------
taz_wordcount <- taz_sw %>% count(word, sort=TRUE) %>% ungroup
head(taz_wordcount,5)

# compute frequencies by article
# -------------------------------
taz_wordcount_art <- taz_sw %>% count(id, word, sort=TRUE) %>% ungroup
head(taz_wordcount_art,5)


# ==============================================================================
# 7) Run sentiment analysis: Calculate sentiment scores
# ==============================================================================

# ==============================================================================
# 7.1) Loop through all articles
# ==============================================================================

# Prepare data frame
# ------------------
X_sel$Year <- substr(X_sel$date,5,8)
Y = X_sel %>% select(title,date,Year,starts_with("LDA_topic"))

# Prepare empty vectors to store words for identification of most important words
# ------------------------------------------------------------------------------
#list_pos <- list()
#list_neg <- list()
words_pos = c()
word_freq_pos = c()
words_neg = c()
word_freq_neg = c()
year_pos = c()
year_neg = c()
value_pos = c()
value_neg = c()
article_num_pos = c()
article_num_neg = c()


# Start with sentiment analysis
# -----------------------------
for(i in 1:NCorpus)
{
  # select all unique words in the article
  # --------------------------------------
  svWords_i = taz_wordcount_art %>% filter(id==i)
  Y$n_words_uq[i] = nrow(svWords_i)
  Y$n_words[i] = sum(svWords_i$n)
  
  # intersect article's words with list of positive words from dictionary
  # ---------------------------------------------------------------------
  svIntersect = intersect(svWords_i$word, Pos$Word)
  
  # number of unique positive words per article
  # -------------------------------------------
  Y$Pos[i] = length(svIntersect)
  
  # take positions of common words in article vector and dictionary
  # ---------------------------------------------------------------
  # to access corresponding frequency and sentiment values
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Pos$Word)
  
  # total number of positive words per article
  # ------------------------------------------
  Y$Pos_all[i] = sum(svWords_i$n[vSel1])
  
  # calculate article's weighted positive sentiment
  # -----------------------------------------------
  # by counting words with positive sentiment and multiply by sentiment weight:
  # Sentiment values multiplied with the number of corresp. words
  Y$Pos_weight[i] = sum(Pos$Value[vSel2]*svWords_i$n[vSel1]) 
  
  # save results
  # ------------
  #save word and weight vectors
  #list_pos[[i]] <- list(Y$Year[i], Y$LDA_topic[i], svIntersect, Pos$Value[vSel2])
  # save positive words
  words_pos <- c(words_pos, svIntersect)
  # save their frequency
  word_freq_pos <- c(word_freq_pos, svWords_i$n[vSel1])
  # save their sentiment scores
  value_pos <- c(value_pos, Pos$Value[vSel2])
  # save publication year
  year_pos <- c(year_pos, rep(Y$Year[i],length(svIntersect)))
  # save id
  article_num_pos <- c(article_num_pos, rep(i, length(svIntersect)))
  
  # repeat for words with negative sentiment
  # ----------------------------------------
  svIntersect_n = intersect(svWords_i$word, Neg$Word)
  Y$Neg[i] = length(svIntersect_n)
  vSel1n = match(svIntersect_n, svWords_i$word)
  vSel2n = match(svIntersect_n, Neg$Word)
  Y$Neg_all[i] = sum(svWords_i$n[vSel1n])
  Y$Neg_weight[i] = sum(Neg$Value[vSel2n]*svWords_i$n[vSel1n]) 
  #list_neg[[i]] <- list(Y$Year[i], Y$LDA_topic[i], svIntersect_n, Neg$Value[vSel2n])
  words_neg <- c(words_neg, svIntersect_n)
  word_freq_neg <- c(word_freq_neg, svWords_i$n[vSel1n])
  value_neg <- c(value_neg, Neg$Value[vSel2n])
  year_neg <- c(year_neg, rep(Y$Year[i],length(svIntersect_n)))
  article_num_neg <- c(article_num_neg, rep(i, length(svIntersect_n)))
  
  # show progress
  # -------------
cat("\r",i*100/NCorpus," % done ");flush.console()
}


# ==============================================================================
# 7.2) Define sentiment measures
# ==============================================================================


# calculate net sentiment (unique words)
# --------------------------------------
# = subtract the number of (unique) negative words from (unique) positive words
# and normalize by article length (number of words after pre-processing)
Y$Sentiment = Y$Pos - Y$Neg
Y$Sentiment_norm = Y$Sentiment/Y$n_words_uq

# calculate net sentiment (frequency)
# -----------------------------------
# substract total number of negative words from positive words and normalize 
# by article length (number of words after pre-processing)
Y$Sentiment_freq = Y$Pos_all - Y$Neg_all
Y$Sentiment_freq_norm = Y$Sentiment_freq/Y$n_words

#calculate net weighted sentiment
# --------------------------------
# weight by frequency of words and sentiment score and normalize by article 
# length (number of words after pre-processing)
Y$Sentiment_weight = Y$Pos_weight + Y$Neg_weight
Y$Sentiment_weight_norm = Y$Sentiment_weight/Y$n_words

# ==============================================================================
# 8) Explorative analysis of results
# ==============================================================================

# ==============================================================================
# 8.1) Distribution of sentiment measures
# ==============================================================================

# Boxplot of net sentiment (unique words)
# ----------------------------------------
par(mfrow=c(1,2), mar=c(3,3,1,1), bty="l")
boxplot(Y$Sentiment_norm, main="net sentiment (unique words")
abline(h=0, lty=3)

# Boxplot of net sentiment (total words)
# ----------------------------------------
par(mfrow=c(1,2), mar=c(3,3,1,1), bty="l")
boxplot(Y$Sentiment_freq_norm, main="net sentiment (total words")
abline(h=0, lty=3)

# Boxplot of weighted sentiment 
# -----------------------------
par(mfrow=c(1,2), mar=c(3,3,1,1), bty="l")
boxplot(Y$Sentiment_weight_norm, main="Weighted sentiment")
abline(h=0, lty=3)

# ==============================================================================
# 8.2) Most positive and negative articles
# ==============================================================================

# Show articles with most positive or negative score (unique)
# -----------------------------------------------------------
Y = Y[order(Y$Sentiment_norm),]
head(Y$title)
tail(Y$title)

# Show articles with most positive or negative score (frequency)
# --------------------------------------------------------------
Y = Y[order(Y$Sentiment_freq_norm),]
head(Y$title)
tail(Y$title)

# Show articles with most positive or negative score (weighted)
# -------------------------------------------------------------
Y = Y[order(Y$Sentiment_weight_norm),]
head(Y$title)
tail(Y$title)

# ==============================================================================
# 8.3) Most important positive and negative words
# ==============================================================================

# data frames with list of positive and negative words
# ----------------------------------------------------
df_pos <- data.frame(words_pos, word_freq_pos, value_pos, year_pos, article_num_pos)
df_neg <- data.frame(words_neg, word_freq_neg, value_neg, year_neg, article_num_neg)

# export data frame to .txt
# ---------------------------
write.table(df_pos, paste0(sFolder1, sOutFile2), sep=";",col.names = T, row.names = FALSE)
write.table(df_neg, paste0(sFolder1, sOutFile3), sep=";",col.names = T, row.names = FALSE)

# export data frame to .rds
# ---------------------------
saveRDS(df_pos, paste0(sFolder1, sOutFile4))
saveRDS(df_neg, paste0(sFolder1, sOutFile5))

# ==============================================================================
# 8.4) Matches with dictionary
# ==============================================================================

# For how many words in the articles do we have sentiment scores?

# unique words
# ------------
Y$matches_uq = (Y$Pos + Y$Neg)/Y$n_words_uq
summary(Y$matches_uq)

# total words (excluding stop words)
# ----------------------------------
Y$matches_all = (Y$Pos_all + Y$Neg_all)/(Y$n_words)
summary(Y$matches_all)

# save results
# ------------
write.xlsx(Y, paste0(sFolder1, sOutFile1), overwrite = T)

# ==============================================================================
proc.time() - Begin # elapsed time
# ============================End of file ======================================