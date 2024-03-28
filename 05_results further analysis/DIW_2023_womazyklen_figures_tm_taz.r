# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: Distance map for topic model in 'taz'
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
# First created --- September 21, 2022										
# Last modified --- November 10, 2022
# ------------------------------------------------------------------------------
# content: Reproduces the graphs that visualize the results of the LDA topic 
# model for the taz articles 
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
sInFile1 = "Data/taz/taz_Housing_articles_unambiguous_withtopics_2022-09-22.rds"
sInFile2 = "Data/taz/topic model top terms/k=11 neu2/taz_lda_k=11.Rda"
sInFile3 = "Data/taz/topic model top terms/k=11 neu/taz_k11neu_daten_ldavis_plot.xlsx"
sOutFile = "Draft/figures/topic-modelling/fig_topic_modelling_taz_distances_k=11_new.pdf"


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
Stopwords_pck = c("prozent", "dr","st","nnen","taz","geb","abb", Stopwords_pck)
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
# 2) load housing-related articles in taz
# ==============================================================================

# Load all taz articles
# ------------------------------
X_sel = readRDS(paste0(sFolder1, sInFile1))

# ==============================================================================
# 3) Pre-process text data using the tidyr-package
# ==============================================================================

# transform article collection into data frame
# --------------------------------------------
taz_df = tibble(id=1:length(X_sel$text),text=X_sel$text)
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


# compute total frequencies and filter for unique words with at least 10 
# occurrences between 1950 and 2022
# --------------------------------------------------------------------------
taz_wordcount10 <- taz_sw %>% count(word, sort=TRUE) %>% filter(n > 9) %>% ungroup

# ==============================================================================
# 4) Load results from LDA topic model
# ==============================================================================

# load LDA results for model with 11 topics
# -----------------------------------------
load(paste0(sFolder1, sInFile2))

# Calculate per-topic-per-word probabilities
# ------------------------------------------
taz_topics <- tidy(taz_lda, matrix="beta")

# calculate document-topic probabilities
# --------------------------------------
taz_topics_gamma <- tidy(taz_lda, matrix="gamma")

# ==============================================================================
# 5) Run graphical analysis from ldavis() package
# ==============================================================================

# filter for relevant topics
# --------------------------
# eliminate topic 5: foreign policy, 7: social issues, 9: cultural activities
taz_topics_sel <- taz_topics%>%filter(topic  %in% c(1:4, 7:8,10:11))
taz_topics_gamma_sel <- taz_topics_gamma%>%filter(topic  %in% c(1:4, 7:8,10:11))

# transform data in the form required by LDAvis: create column for each term
# --------------------------------------------------------------------------
taz_wide_beta_sel <- taz_topics_sel%>%pivot_wider(names_from = term
                                                  , values_from = beta
                                                  , names_repair = "unique")

# transform data in the form required by LDAvis: create column for each topic
# --------------------------------------------------------------------------
taz_wide_gamma_sel <- taz_topics_gamma_sel%>%pivot_wider(names_from = topic
                                                         , values_from = gamma)

# Create list with unique terms
# -----------------------------
vocab_sel <- names(taz_wide_beta_sel[2:ncol(taz_wide_beta_sel)])

# add count of each term in whole corpus
# --------------------------------------
vocab_df_sel <- tibble(word = vocab_sel)%>%left_join(taz_wordcount10
                                                     , by = "word")

# extract vector with overall frequency of each unique term
# ----------------------------------------------------------
term_frequency_sel <- vocab_df_sel$n

# matrix with topics as rows and terms as columns
# -----------------------------------------------
phi_sel <- as.matrix(taz_wide_beta_sel[2:ncol(taz_wide_beta_sel)])

# normalize distribution
# ----------------------
# (only if topic model was filtered for specific topics)
phi_sel <- phi_sel/rowSums(phi_sel)

# calculate total number of terms in each article
# ------------------------------------------------
doc.length <- taz_sw %>% count(id, sort=TRUE) %>% ungroup
head(doc.length,5)

# recode article id
# -----------------
doc.length$document <-  as.character(doc.length$id)

# add article length to topic probability data frame
# --------------------------------------------------
taz_wide_gamma_n_sel <- left_join(taz_wide_gamma_sel, doc.length, by = "document")
doc_length <- taz_wide_gamma_n_sel$n

# matrix with documents as rows and topics as columns
# ---------------------------------------------------
# here: d=3 topics were dropped, so adjust column selector by [2,i+1-d]
theta_sel <- as.matrix(taz_wide_gamma_n_sel[2:9])
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
distance_df$topic_names <- c("Situation von \n Gefluechteten"
                ,"Mietmarkt und \n Mietrecht"
                ,"Architektur und \n oeffentlicher Raum"
                ,"Wohnungspolitik \n (Bremen)"
                ,"Familienleben und \n Wohnumfeld"
                ,"Hausbesetzungen \n (Berlin)"
                ,"Wohnungspolitik im \n Wahlkampf (Berlin)"
                ,"Wohnungsbau \n (Hamburg)")

# add category to each row
# ------------------------
distance_df$category <- c("Politik", "Wirtschaft","Wohnraum als Lebensraum"
                          ,"Politik","Wohnraum als Lebensraum","Politik","Politik"
                          ,"Wirtschaft")

# alternatively: load coordinates from saved file
# ------------------------------------------------
# (old topic model from older k=11 neu!)
distance_df2 <- read.xlsx(paste0(sFolder1,sInFile3))

# adjust axes to common space (def. of coordinates) across newspapers
# ---------------------------------------------------------------------
distance_df2 = distance_df2 %>% mutate(x = -x
                                       ,y = -y)

# add topic names to each row (including line breaks in graph)
# -----------------------------------------------------------
distance_df2$topic_names <- c("Wohnungsbau \n (Hamburg)"
                              ,"Situation von \n Gefluechteten"
                             ,"Mietmarkt und \n Mietrecht"
                             ,"Hausbesetzungen \n (Berlin)"
                             ,"Wohnungspolitik \n (Bremen)"
                             ,"Familienleben und \n Wohnumfeld"
                             ,"Wohnungspolitik im \n Wahlkampf (Berlin)"
                             ,"Architektur und \n oeffentlicher Raum")


# add category to each row
# ------------------------
distance_df2$category <- c("Wirtschaft", "Politik","Wirtschaft"
                          ,"Politik","Politik","Wohnraum als Lebensraum"
                          ,"Politik","Wohnraum als Lebensraum")

# plot the distances (Version new: distances_df)
# ----------------------------------------------
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


# Version old: distances_df2
#---------------------------
ggplot(distance_df2, aes(x =x, y= y))+
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