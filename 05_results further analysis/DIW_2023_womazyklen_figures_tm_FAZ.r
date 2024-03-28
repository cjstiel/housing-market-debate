# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: Distance map for topic model in 'F.A.Z.'
#
#		OUTLINE: PART 1: Load stopword list
#				 PART 2: Load housing-related articles in taz
#				 PART 3: Pre-process text data using the tidyr-package
#				 PART 4: Load results from LDA topic model
#				 PART 5: Run graphical analysis from ldavis() package
#				 PART 6: Customize ldavis() plot
#
# -------------------------------------------------------------------------------
# code authors: Caroline Stiel and Felix Aubele (DIW Berlin)
#
# First created --- September 14, 2022										
# Last modified --- November 11, 2022
# ------------------------------------------------------------------------------
# content: Reproduces the graphs that visualize the results of the LDA topic 
# model for the F.A.Z. articles 
# ==============================================================================

# ==============================================================================
# 0) Initial settings
# ==============================================================================

# clean
# -----
rm(list = ls())
gc()
options(scipen = 999)

# load libraries
# --------------
library(openxlsx)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(topicmodels)
library(tm)
library(tidyverse)
library(LDAvis)
library(servr)
library(ggrepel)

# set R system language (including dates etc.) to German
# ------------------------------------------------------
Sys.setlocale('LC_ALL', 'german')


# define paths and data input/output names
# ----------------------------------------
sFolder1 = "" # path where data, drafts etc. are stored
sFolder2 = "" # path where local repositories are located
sInFile1 = "Data/FAZ/FAZ_Housing_articles_unambiguous_withtopics_2022-08-23.rds"
sInFile3 = "Data/FAZ/topicmodels/top terms/FAZ_lda_k=10.Rda"
sOutFile = "Draft/figures/topic-modelling/fig_topic_modelling_FAZ_distances_k=10.pdf"


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
# 2) load housing-related articles in 'F.A.Z.'
# ==============================================================================

# Load all F.A.Z. articles
# ------------------------------
X_sel = readRDS(paste0(sFolder1, sInFile1))

# ==============================================================================
# 3) Pre-process text data using the tidyr-package
# ==============================================================================

# transform article collection into data frame
# --------------------------------------------
FAZ_df = tibble(id=1:length(X_sel$text),text=X_sel$text)
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


# compute total frequencies and filter for unique words with at least 10 
# occurrences between 1950 and 2022
# --------------------------------------------------------------------------
FAZ_wordcount10 <- FAZ_sw %>% count(word, sort=TRUE) %>% filter(n > 9) %>% ungroup

# ==============================================================================
# 4) Load results from LDA topic model
# ==============================================================================

# load LDA results for model with 10 topics
# -----------------------------------------
load(paste0(sFolder1, sInFile3))

# Calculate per-topic-per-word probabilities
# ------------------------------------------
FAZ_topics <- tidy(FAZ_lda, matrix="beta")

# calculate document-topic probabilities
# --------------------------------------
FAZ_topics_gamma <- tidy(FAZ_lda, matrix="gamma")

# ==============================================================================
# 5) Run graphical analysis from ldavis() package
# ==============================================================================

# filter for relevant topics
# --------------------------
# eliminate topic 6: foreign policy and topic 10: companies/employees
FAZ_topics_sel <- FAZ_topics%>%filter(topic  %in% c(1:5, 7:9))
FAZ_topics_gamma_sel <- FAZ_topics_gamma%>%filter(topic  %in% c(1:5, 7:9))

# transform data in the form required by LDAvis: create column for each term
# --------------------------------------------------------------------------
FAZ_wide_beta_sel <- FAZ_topics_sel%>%pivot_wider(names_from = term
                                                  , values_from = beta
                                                  , names_repair = "unique")

# transform data in the form required by LDAvis: create column for each topic
# --------------------------------------------------------------------------
FAZ_wide_gamma_sel <- FAZ_topics_gamma_sel%>%pivot_wider(names_from = topic
                                                         , values_from = gamma)

# Create list with unique terms
# -----------------------------
vocab_sel <- names(FAZ_wide_beta_sel[2:ncol(FAZ_wide_beta_sel)])

# add count of each term in whole corpus
# --------------------------------------
vocab_df_sel <- tibble(word = vocab_sel)%>%left_join(FAZ_wordcount10
                                                     , by = "word")

# extract vector with overall frequency of each unique term
# ----------------------------------------------------------
term_frequency_sel <- vocab_df_sel$n

# matrix with topics as rows and terms as columns
# -----------------------------------------------
phi_sel <- as.matrix(FAZ_wide_beta_sel[2:ncol(FAZ_wide_beta_sel)])

# normalize distribution
# ----------------------
# (only if topic model was filtered for specific topics)
phi_sel <- phi_sel/rowSums(phi_sel)

# calculate total number of terms in each article
# ------------------------------------------------
doc.length <- FAZ_sw %>% count(id, sort=TRUE) %>% ungroup
head(doc.length,5)

# recode article id
# -----------------
doc.length$document <-  as.character(doc.length$id)

# add article length to topic probability data frame
# --------------------------------------------------
FAZ_wide_gamma_n_sel <- left_join(FAZ_wide_gamma_sel, doc.length, by = "document")
doc_length <- FAZ_wide_gamma_n_sel$n

# matrix with documents as rows and topics as columns
# ---------------------------------------------------
# here: 2 topics were dropped, so adjust column selector by 2
theta_sel <- as.matrix(FAZ_wide_gamma_n_sel[2:9])
# normalize distribution (only when filtered)
theta_sel <- theta_sel/rowSums(theta_sel)


# ==============================================================================
# 6) Customize ldavis() plot
# ==============================================================================

  
# function to calculate distance and project into 2d space from ldavis package
# ----------------------------------------------------------------------------
jsPCA <- function(phi_sel) {
  # first, we compute a pairwise distance between topic distributions
  # using a symmetric version of KL-divergence
  jensenShannon <- function(x, y) {
    m <- 0.5 * (x + y)
    lhs <- ifelse(x == 0, 0, x * (log(x) - log(m)))
    rhs <- ifelse(y == 0, 0, y * (log(y) - log(m)))
    0.5 * sum(lhs) + 0.5 * sum(rhs)
  }
  dist.mat <- proxy::dist(x = phi_sel, method = jensenShannon)
  # then, we reduce the K by K proximity matrix down to K by 2 using PCA
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[,1], y = pca.fit[,2])
}

# topic frequencies and proportions
# ---------------------------------
  topic.frequency <- colSums(theta_sel * doc_length)
  topic.proportion <- topic.frequency/sum(topic.frequency)

  
# calculate the distances
# -----------------------
distance_mat <- jsPCA(phi_sel)
colnames(distance_mat) <- c("x", "y")
    
distance_df <- data.frame(distance_mat, Freq = topic.proportion*100, 
                       cluster = 1, stringsAsFactors = FALSE)

# add topic names to each row (including line breaks in graph)
# -----------------------------------------------------------
distance_df$topic_names <- c("Architektur und \n Stadtbild"
                 ,"Miet- und \n Baupreisentwicklung"
                ,"Wohnsituation \n und -politik \n (Rhein-Main)"
                ,"Immobilien- \n markt"
                ,"Wohnungspolitik \n im Wahlkampf \n (Rhein-Main)"
                ,"Wohnungsbau \n (Rhein-Main)"
                ,"Familienleben und \n Wohnumfeld"
                ,"Wohnungspolitik auf \n Bundesebene")

# add category to each row
# ------------------------
distance_df$category <- c("Wohnraum als Lebensraum","Wirtschaft","Politik"
                          ,"Wirtschaft","Politik","Wirtschaft"
                          ,"Wohnraum als Lebensraum","Politik")


# plot the distances
# ------------------
pdf(paste0(sFolder1, sOutFile))
ggplot(distance_df, aes(x =x, y= y))+
  geom_point(aes(size = Freq, color = category, alpha = 1), show.legend = T
             ,shape = 19)+
  scale_size_continuous(range = c(30, 50))+
  geom_text_repel(aes(label = topic_names), size=5,point.size = NA)+
  scale_color_manual(values = c("darkgoldenrod","darkgreen","darkslateblue"))+
  theme_minimal()+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=14))+
  guides(alpha = "none",size = "none",color = guide_legend(nrow = 1,byrow = T
                                                           ,override.aes = list(size=5)))+
  labs(title = "",subtitle = "",y = "",x = "",color="")+
  coord_cartesian(clip="off")+
  scale_x_continuous(expand = expansion(mult = 0.2))
dev.off()

# ==============================================================================
# End of file
# ==============================================================================