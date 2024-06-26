# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: LDA sub-topic modelling for selected topics in 'taz'
#
#		OUTLINE: PART 1: Load stopword list
#				 PART 2: Load housing-related articles in taz
#				 PART 3: Read-in text from articles
#				 PART 4: Pre-process text data using the tidyr-package
#				 PART 5: Apply LDA topic model
#						5.1 Find optimal number of topics
#						5.2 Run LDA with k topics
# 						5.3 Analyze top 100 terms
#						5.4 Intertopic distance map (LDAvis())
#						5.5 Associate each article with one or more topics
#
# -------------------------------------------------------------------------------
# code authors: Caroline Stiel and Felix Aubele (DIW Berlin)
#
# First created --- May 11, 2022										
# Last modified --- August 30, 2022
# ------------------------------------------------------------------------------
# content: This code pre-processes the taz text data for the topic 
# models and applies an LDA topic model for the sub topic 'Wohnungsmarkt'.
# ==============================================================================

# ==============================================================================
# 0) Initial settings
# ==============================================================================

# clean
# -----
rm(list = ls())
gc()
options(scipen = 999)


# record process time
# -------------------
Begin = proc.time()


# load libraries
# --------------
library(openxlsx)
#library(XML)
library(stringr)
library(dplyr)
library(tidyr)
library(topicmodels)
library(tidytext)
library(tm)
library(stopwords)
library(quanteda)
library(reshape2)
library(ggplot2)
library(gmodels)
library(seededlda)
library(LDAvis)
library(servr)

# set R system language (including dates etc.) to German
# ------------------------------------------------------
Sys.setlocale('LC_ALL', 'german')


# define paths and data input/output names
# ----------------------------------------
sFolder1 = "" # path where data, drafts etc. are stored
sFolder2 = "" # path where local repositories are located
sInFile1 = "Data/Taz/taz_Housing_articles_unambiguous_withtopics_2022-06-09.rds"
sOutFile1 = "Data/Taz/topic model top terms/k=11 neu/only housing/taz_Housing_articles_unambiguous_withtopics_only_housing_market_2022-06-17.rds"


# ==============================================================================
# 1) Load stopword list
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
Stopwords_pck = c("berlin", "berliner","euro","prozent", "dr","st","ber"
                  ,"nnen", "taz", Stopwords_pck)
Stopwords_pck = sort(Stopwords_pck)
Stopwords_pck


# Replace Umlaute
# ---------------
Stopwords_pck = gsub("�", "ae", Stopwords_pck)
Stopwords_pck = gsub("�", "Ae", Stopwords_pck)
Stopwords_pck = gsub("�", "Oe", Stopwords_pck)
Stopwords_pck = gsub("�", "Ue", Stopwords_pck)
Stopwords_pck = gsub("�", "oe", Stopwords_pck)
Stopwords_pck = gsub("�", "ue", Stopwords_pck)
Stopwords_pck = gsub("�", "ss", Stopwords_pck)


# Remove negations etc. from stopwords list
# -----------------------------------------
# Notes: Stopwords currently contain negations, i.e., "nichts", "nicht", "kein",
# "keiner" and amplification ("viel") etc.
# -> should be exempted for sentiment analysis
# Stopwords_pck2 = Stopwords_pck %>% char_remove(c("nicht", "nichts", "kein"
#                                                  , "keiner", "viel"))

# Convert to data frame
# ---------------------
Stopwords_pck = data.frame("word"=c(Stopwords_pck))
head(Stopwords_pck,5)

# ==============================================================================
# 2) Load housing-related articles in 'taz'
# ==============================================================================

# Load housing market articles info
# ---------------------------------
X = readRDS(paste0(sFolder1, sInFile1))

# Select only articles focusing on rental housing market (Topic 'Wohnungsmarkt')
# ------------------------------------------------------------------------------
# select all articles with most associated and second most associated topic 'Wohnungsmarkt'
X = X_sel%>%filter(LDA_topic_1_name == "Wohnungsmarkt"| LDA_topic_2_name == "Wohnungsmarkt")

# How many articles cover housing market topic?
# ------------------------------------------------
nrow(X)

# Save article titles
# -------------------
svOrdinance = X$title


# ==============================================================================
# 3) Read-in text from articles
# ==============================================================================

# Notes: Screening all articles takes about 10min (at DIW Berlin).

# Generate empty vector to store text data
# -----------------------------------------
Ordinance = c()
NFile = nrow(X)

# for each article
  for(i in 1:NFile)
  {
  # save file name where article text is stored
	sFile_i = X[i,]
  # select info on publication year
  sYear_i = substring(sFile_i$date, first = 5, last = 8)
  
  #select text
  Text = sFile_i$text
  # clean text
  Text = gsub("\n", " ", Text)
  Text = gsub("\t", " ", Text)
  Text = gsub("�", "ae", Text)
  Text = gsub("�", "Ae", Text)
  Text = gsub("�", "Oe", Text)
  Text = gsub("�", "Ue", Text)
  Text = gsub("�", "oe", Text)
  Text = gsub("�", "ue", Text)
  Text = gsub("�", "ss", Text)
  Text = trimws(Text)
  Text = tolower(Text)
  # add to previous set of articles
  Ordinance = c(Ordinance, Text)
  # show progress
  cat("\r",i*100/NFile," % done ");flush.console()
  }


# ==============================================================================
# 4) Pre-process text data using the tidyr-package
# ==============================================================================

# transform article collection into data frame
# --------------------------------------------
taz_df = tibble(id=1:length(Ordinance),text=Ordinance)
head(taz_df,5)

# split into words (tokens), strip punctuation, and convert to lowercase
# ----------------------------------------------------------------------
# Notes: set to_lower=FALSE if you do not want to convert to lowercase
# Notes: unnest_tokens() splits words at German Umlaute (e.g. grer = 'gr'+'er').
# Therefore, replace German Umlaute with 'ae' etc. before pre-processing. Do the
# same for stopwords.

taz_tokens <- taz_df %>% unnest_tokens(word, text)
head(taz_tokens, 10)

# remove stopwords
# ----------------
taz_sw <- taz_tokens %>% anti_join(Stopwords_pck)
head(taz_sw,5)

# compute total frequencies and drop terms which occur less than 10times overall
# --------------------------
taz_wordcount10 <- taz_sw %>% count(word, sort=TRUE)%>% filter(n > 9) %>% ungroup
head(taz_wordcount10,5)

taz_sw_larger_10 <- taz_sw %>% inner_join(taz_wordcount10)

# compute frequencies by article
# -------------------------------
taz_wordcount_art <- taz_sw_larger_10 %>% count(id, word, sort=TRUE) %>% ungroup
head(taz_wordcount_art,5)

# Convert into document-term-matrix
# ---------------------------------
taz_dtm <- taz_wordcount_art %>% cast_dtm(id,word,n)
taz_dtm

# ==============================================================================
# 5) Apply LDA topic model
# ==============================================================================

# ==============================================================================
# 5.1) Find optimal number of topics
# ==============================================================================

# loop to obtain LDA results with different number of topics
# -----------------------------------------------------------
# run only to find the best k: comment loop out afterwards
# for (i in c(3,6,9)){

# after that: choose optimal k
# ----------------------------
i=6

# ==============================================================================
# 5.2) Run LDA with k topics
# ==============================================================================

  # apply LDA topic with k=i topics
  # -------------------------------
  taz_lda <- LDA(taz_dtm, k=11, control=list(seed=1234))
  taz_lda


  # save LDA topic model
  # -------------------
  save(taz_lda, file = paste0(sFolder1, "Data/taz/topic model top terms/k=11 neu/only housing/taz_lda_k=",as.character(i),".Rda")) 

# ==============================================================================
# 5.3) Analyze top 100 terms
# ==============================================================================

  # Calculate per-topic-per-word probabilities
  # ------------------------------------------
  taz_topics <- tidy(taz_lda, matrix="beta")

  # calculate document-topic probabilities
  # --------------------------------------
  taz_topics_gamma <- tidy(taz_lda, matrix="gamma")

  # Display top 100 terms per topics
  # --------------------------------
  top_terms <- taz_topics %>%
    group_by(topic) %>%
    top_n(100, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  # rearrange data frame for export
  # -------------------------------
  # following structure: topic1 | terms1 | beta1 | topic2 | terms2 | beta2 | ...
  top_100 <- top_terms[1:100, ]
  names(top_100) <- c("topic1","term1","beta1")

  for(j in 2:i){

    k_1 <- ((j-1)*100)+1
    k_2 <- j*100
    top_100_i <- top_terms[k_1 : k_2, ]
    names(top_100_i) <- c(paste0("topic",j),paste0("term",j),paste0("beta",j))
    top_100 <-  bind_cols(top_100, top_100_i)
  }

  # save top terms
  # -------------
  write.xlsx(top_100, paste0(sFolder1, "Data/taz/topic model top terms/k=11 neu/top_terms_only_housing_k=", as.character(i), ".xlsx"))

# ==============================================================================
# 5.4) Intertopic distance map (LDAvis())
# ==============================================================================

  # transform data in the form required by LDAvis: create column for each term
  # --------------------------------------------------------------------------
  taz_wide_beta <- taz_topics%>%pivot_wider(names_from = term, values_from = beta
                                          , names_repair = "unique")

  # transform data in the form required by LDAvis: create column for each topic
  # --------------------------------------------------------------------------
  taz_wide_gamma <- taz_topics_gamma%>%pivot_wider(names_from = topic
                                                 , values_from = gamma)

  # Create list with unique terms
  # -----------------------------
  vocab <- names(taz_wide_beta[2:ncol(taz_wide_beta)])

  # add count of each term in whole corpus
  # --------------------------------------
  vocab_df <- tibble(word = vocab)%>%left_join(taz_wordcount10, by = "word")

  # extract vector with overall frequency of each unique term
  # ----------------------------------------------------------
  term_frequency <- vocab_df$n

  # matrix with topics as rows and terms as columns
  # -----------------------------------------------
  phi <- as.matrix(taz_wide_beta[2:ncol(taz_wide_beta)])

  # calculate total number of terms in each article
  # ------------------------------------------------
  doc.length <- taz_sw %>% count(id, sort=TRUE) %>% ungroup
  head(doc.length,5)

  # recode article id
  # -----------------
  doc.length$document <-  as.character(doc.length$id)

  # add article length to topic probability data frame
  # --------------------------------------------------
  taz_wide_gamma_n <- left_join(taz_wide_gamma, doc.length, by = "document")
  doc_length <- taz_wide_gamma_n$n

  # matrix with documents as rows and topics as columns
  # ---------------------------------------------------
  # for each article, save topic probabilities
  theta <- as.matrix(taz_wide_gamma_n[2:(i+1)])

  # apply ldavis
  # ------------
 lda_visu <- createJSON(phi = phi, theta = theta, doc.length = doc_length
                        , vocab = vocab, term.frequency = term_frequency)

  # save ldavis results
  # --------------------
  serVis(lda_visu, out.dir = paste0(sFolder1, "Data/taz/topic model top terms/k=11 neu/only_housing_k=", as.character(i)), open.browser = F)

  # check out results (intertopic distance map)
  # -------------------------------------------
  httd(paste0(sFolder1, "Data/taz/topic model top terms/k=11 neu/only_housing_k=", as.character(i)))

  # clean
  # -----
  gc()
  
  # show progress
  # -------------
  #  cat("\r LDA model with",i,"topics done");flush.console()
  #}
  

# ==============================================================================
# 5.5 Associate each article with one or more topics
# ==============================================================================

# ==============================================================================
# 5.5.1  Most relevant topic per article
# ==============================================================================

# select for each article the most associated topic
# -------------------------------------------------
article_classification_1t1 <- taz_topics_gamma %>%
  group_by(document) %>%
  top_n(1,gamma) %>%
  ungroup


# ==============================================================================
# 5.5.3  Name topics
# ==============================================================================

# give each topic a name
# ----------------------
topic_names <- c("Politische Interventionen in den Wohnungsmarkt", 
                 "Eigent�mer/Vermieter","Wohnsituation Mieter", 
                 "Kontroversen bzgl. Wohnungsbaugesellschaften",
                 "Wohnsituation Sozialhilfeempf�nger / Wohnungsloser",
                 "rechtliche Konflikte zwischen Mietern und Vermietern")

name_coding <- tibble(topic = c(1:6),topic_names)


# collect all names in a data frame
# ---------------------------------
name_coding <- tibble(topic = c(1:11),topic_names)

# merge topics to articles
# ------------------------
article_classification_1t1_names  <- article_classification_1t1%>%left_join(name_coding, by = "topic")

# how many articles are associated with each topic?
# -------------------------------------------------
table(article_classification_1t1_names$topic_names)



# ==============================================================================
# 5.5.4  Save and export results
# ==============================================================================

# prepare main data frame X_sel (select only relevant columns)
# ------------------------------
X_sel = X[c(1:7)] 
X_sel$LDA_topic_1 <- NA
X_sel$LDA_topic_1_name <- NA


# merge results to main data frame 'X_sel'
# ----------------------------------------
for (i in 1:nrow(X_sel)){
  X_sel$LDA_topic_1[i] <- article_classification_1t1_names$topic[article_classification_1t1_names$document==i]
  X_sel$LDA_topic_1_name[i] <- article_classification_1t1_names$topic_names[article_classification_1t1_names$document==i] 
  # show progress
  cat("\r",i*100/nrow(X_sel)," % done ");flush.console()
}

# export article list
# --------------------
saveRDS(X_sel, paste0(sFolder1, sOutFile1))



# ==============================================================================
proc.time() - Begin # elapsed time
# ============================End of file ======================================
