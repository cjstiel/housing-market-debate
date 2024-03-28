# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: LDA sub-topic modelling for selected topics in F.A.Z.
#
# 		OUTLINE: PART 1: Load stopword list
#				 PART 2: Load housing-related articles in 'F.A.Z.'
#				 PART 3: Pre-process text data using the tidyr-package
#				 PART 4: Apply LDA topic model
#						4.1 Find optimal number of topics
#						4.2 Run LDA with k topics
# 						4.3 Analyze top 100 terms
#						4.4 Intertopic distance map (LDAvis())
#						4.5 Associate each article with one or more topics
#
#
# -------------------------------------------------------------------------------
# code authors: Caroline Stiel and Felix Aubele (DIW Berlin)
#
# First created --- August 29, 2022										
# Last modified --- August 29, 2022 (cs)
# ------------------------------------------------------------------------------
# content: This code pre-processes the FAZ text data for the topic 
# models and applies an LDA topic model for the sub topics 'Immobilienmarkt' and
# 'Mietpreisentwicklung'.
# ==============================================================================

# ==============================================================================
# 0) Initial settings
# ==============================================================================

# clean
# -----
rm(list = ls())


# record process time
# -------------------
Begin = proc.time()


# load libraries
# --------------
library(openxlsx)
library(XML)
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
sFolder2 = "" # path where local repository is located
sInFile1 = "data pre-processing/housing_market_keywords.xlsx"
sInFile2 = "Data/FAZ/FAZ_Housing_articles_unambiguous_withtopics_2022-08-23.rds"
sOutFile1 = "Data/FAZ/FAZ_Housing_articles_unambiguous_only_housing_withtopics_2022-08-29.rds"

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
Stopwords_pck = c("prozent", "dr","st","nnen","faz","geb","f.a.z.","abb", Stopwords_pck)
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
# 2) Load housing-related articles in 'F.A.Z.'
# ==============================================================================

# Load all F.A.Z. articles
# ------------------------------
X_sel = readRDS(paste0(sFolder1, sInFile2))

# Filter for the topics 'Mietpreisentwicklung und Mietrecht' (2)
# -----------------------------------------------------------------------------
X_sel_h = X_sel %>% filter(LDA_topic_1_name == "Mietpreisentwicklung und Mietrecht")

table(X_sel_h$LDA_topic_1_name)

# ==============================================================================
# 3) Pre-process text data using the tidyr-package
# ==============================================================================

# transform article collection into data frame
# --------------------------------------------
FAZ_df = tibble(id=1:length(X_sel_h$text),text=X_sel_h$text)
head(FAZ_df,5)


# split into words (tokens), strip punctuation, and convert to lowercase
# ----------------------------------------------------------------------
# Notes: set to_lower=FALSE if you do not want to convert to lowercase
# Notes: unnest_tokens() splits words at German Umlaute (e.g. grer = 'gr'+'er').
# Therefore, replace German Umlaute with 'ae' etc. before pre-processing. Do the
# same for stopwords.

FAZ_tokens <- FAZ_df %>% unnest_tokens(word, text)
head(FAZ_tokens, 10)

# remove stopwords
# ----------------
FAZ_sw <- FAZ_tokens %>% anti_join(Stopwords_pck)
head(FAZ_sw,5)


# compute total frequencies and filter for unique words with more than 10 
# occurrences between 1950 and 2022
# --------------------------------------------------------------------------
FAZ_wordcount10 <- FAZ_sw %>% count(word, sort=TRUE) %>% filter(n > 9) %>% ungroup
head(FAZ_wordcount10,5)
tail(FAZ_wordcount10,5)

# keep only words with more than 10 occurrences
# ---------------------------------------------
FAZ_sw_larger_10 <- FAZ_sw %>% inner_join(FAZ_wordcount10)

# compute frequencies by article
# -------------------------------
FAZ_wordcount_art <- FAZ_sw_larger_10 %>% count(id, word, sort=TRUE) %>% ungroup
head(FAZ_wordcount_art,5)

# Convert into document-term-matrix
# ---------------------------------
FAZ_dtm <- FAZ_wordcount_art %>% cast_dtm(id,word,n)
FAZ_dtm


# ==============================================================================
# 4) Apply LDA topic model
# ==============================================================================

# ==============================================================================
# 4.1) Find optimal number of topics
# ==============================================================================

# loop to obtain LDA results with different number of topics
# -----------------------------------------------------------
# run only to find the best k: comment loop out afterwards
# for (i in c(3,6,9)){

# after that: choose optimal k
# ----------------------------
i=6

# ==============================================================================
# 4.2) Run LDA with k topics
# ==============================================================================

  # apply LDA topic with k=i topics
  # -------------------------------
  FAZ_lda <- LDA(FAZ_dtm, k= i, control=list(seed=1234))

  # save LDA topic model
  # -------------------
  save(FAZ_lda, file = paste0(sFolder1, "Data/FAZ/topicmodels/top terms/only housing k=10/FAZ_lda_k="
                              ,as.character(i),".Rda"))

# ==============================================================================
# 4.3) Analyze top 100 terms
# ==============================================================================

  # Calculate per-topic-per-word probabilities
  # ------------------------------------------
  FAZ_topics <- tidy(FAZ_lda, matrix="beta")

  # calculate document-topic probabilities
  # --------------------------------------
  FAZ_topics_gamma <- tidy(FAZ_lda, matrix="gamma")

  # Save top 100 terms per topics
  # -------------------------------
  top_terms <- FAZ_topics %>%
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
  write.xlsx(top_100, paste0(sFolder1, "Data/FAZ/topicmodels/top terms/only housing k=10/top_terms_k="
                             , as.character(i), ".xlsx"))
  
  
# ==============================================================================
# 4.4) Intertopic distance map (LDAvis())
# ==============================================================================
  
  # transform data in the form required by LDAvis: create column for each term
  # --------------------------------------------------------------------------
  FAZ_wide_beta <- FAZ_topics%>%pivot_wider(names_from = term
                                            , values_from = beta
                                            , names_repair = "unique")

  # transform data in the form required by LDAvis: create column for each topic
  # --------------------------------------------------------------------------
  FAZ_wide_gamma <- FAZ_topics_gamma%>%pivot_wider(names_from = topic
                                                   , values_from = gamma)

  # Create list with unique terms
  # -----------------------------
  vocab <- names(FAZ_wide_beta[2:ncol(FAZ_wide_beta)])

  # add count of each term in whole corpus
  # --------------------------------------
  vocab_df <- tibble(word = vocab)%>%left_join(FAZ_wordcount10, by = "word")

  # extract vector with overall frequency of each unique term
  # ----------------------------------------------------------
  term_frequency <- vocab_df$n

  # matrix with topics as rows and terms as columns
  # -----------------------------------------------
  phi <- as.matrix(FAZ_wide_beta[2:ncol(FAZ_wide_beta)])
  
  # calculate total number of terms in each article
  # ------------------------------------------------
  doc.length <- FAZ_sw %>% count(id, sort=TRUE) %>% ungroup
  head(doc.length,5)

  # recode article id
  # -----------------
  doc.length$document <-  as.character(doc.length$id)

  # add article length to topic probability data frame
  # --------------------------------------------------
  FAZ_wide_gamma_n <- left_join(FAZ_wide_gamma, doc.length, by = "document")
  doc_length <- FAZ_wide_gamma_n$n

  # matrix with documents as rows and topics as columns
  # ---------------------------------------------------
  # for each article, save topic probabilities
  theta <- as.matrix(FAZ_wide_gamma_n[2:(i+1)])

  #apply ldavis
  lda_visu <- createJSON(phi = phi, theta = theta, doc.length = doc_length
                         , vocab = vocab, term.frequency = term_frequency)

  #save ldavis results
  serVis(lda_visu, out.dir = paste0(sFolder1, "Data/FAZ/topicmodels/top terms/only housing k=10/FAZ_K_="
                                    , as.character(i)), open.browser = F)

  # check out results (intertopic distance map)
  # -------------------------------------------
  httd(paste0(sFolder1, "Data/FAZ/topicmodels/top terms/only housing k=10/FAZ_K_=",as.character(i)))
  
  
  # clean
  # -----
  gc()
  
  # show progress
  # -------------
  #  cat("\r LDA model with",i,"topics done");flush.console()
  #}
  
  
# ==============================================================================
# 4.5 Associate each article with one or more topics
# ==============================================================================

# Since the analysis did not reveal any more interesting insights (all sub topics 
# are pretty homogeneous), skip rest of the analysis.
  
# # ==============================================================================
# # 4.5.1  Most relevant topic per article
# # ==============================================================================
#   
# # select for each article the most associated topic
# # -------------------------------------------------
# article_classification_1t1 <- FAZ_topics_gamma %>%
#     group_by(document) %>%
#     top_n(1,gamma) %>%
#     ungroup
#   
# # ==============================================================================
# # 4.5.2  Second relevant topic per article
# # ==============================================================================
#   
# # select for each article the second most associated topic
# #---------------------------------------------------------
# article_classification_2t1 <- FAZ_topics_gamma %>%
#     group_by(document) %>%
#     arrange(desc(gamma)) %>% 
#     slice(2:2)%>%
#     ungroup()
#   
#   
# # ==============================================================================
# # 4.5.3  Name topics
# # ==============================================================================
#   
# # give each topic a name
# # ----------------------
# topic_names <- c("Architektur und Stadtbild", "Mietpreisentwicklung und Mietrecht"
#                    , "Regionales Wohnungsangebot und -politik", "Immobilienmarkt"
#                    ,"Politik regional", "Aussenpolitik",	"Wohnungsbau regional"
#                    ,"Wohnraum als Lebensraum","Wohnungspolitik Bund","Unternehmen und Arbeitswelt")
#   
# # collect all names in a data frame
# # ---------------------------------
# name_coding <- tibble(topic = c(1:10),topic_names,topic_categories,relevance_housing)
#   
# # merge names to article classifications
# # --------------------------------------
# article_classification_1t1_names  <- article_classification_1t1%>% left_join(name_coding, by = "topic")
# article_classification_2t2_names  <- article_classification_2t1%>% left_join(name_coding, by = "topic")
#   
# # how many articles are associated with each topic?
# # -------------------------------------------------
# table(article_classification_1t1$topic)
#   
#   
# # ==============================================================================
# # 4.5.4  Save and export results
# # ==============================================================================
#   
# # prepare main data frame X_sel
# # ------------------------------
# X_sel_h$LDA_topic_3 <- NA
# X_sel_h$LDA_topic_3_name <- NA
# 
# X_sel_h$LDA_topic_4 <- NA
# X_sel_h$LDA_topic_4_name <- NA
# 
#   
#   
# # merge results to main data frame 'X_sel_h'
# # ----------------------------------------
# for (i in 1:nrow(X_sel)){
#   X_sel_h$LDA_topic_3[i] <- article_classification_1t1_names$topic[article_classification_1t1_names$document==i]
#   X_sel_h$LDA_topic_3_name[i] <- article_classification_1t1_names$topic_names[article_classification_1t1_names$document==i]
#  
#   X_sel_h$LDA_topic_4[i] <- article_classification_2t2_names$topic[article_classification_2t2_names$document==i]
#   X_sel_h$LDA_topic_4_name[i] <- article_classification_2t2_names$topic_names[article_classification_2t2_names$document==i]
# 
#   # show progress
#   cat("\r",i*100/nrow(X_sel_h)," % done ");flush.console()
# }
#   
# # export article list
# # --------------------
# saveRDS(X_sel_h, paste0(sFolder1, sOutFile1))
#   


  
# ==============================================================================
proc.time() - Begin # elapsed time
# ============================End of file ======================================