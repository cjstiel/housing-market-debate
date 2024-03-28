# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: EMOTION ANALYSIS (F.A.Z.)
#
# 		OUTLINE: PART 1: Define stemming and tokenization functions
#				 PART 2: Load stopword list
#				 PART 3: Prepare emotions lexicon
#				 		3.1 Load lexicon and German-specific letters list
#				 		3.2 Replace German letters and prepare emotion lexicon
#				 PART 4: Prepare news data
#						4.1 Select housing-related articles in 'die tageszeitung (taz)'
#				 		4.2 Pre-process text data using the tm-package
#				 PART 5: Emotions analysis
#						5.1 Prepare data
#						5.2 Start analysis
#						5.3 Results
#				 PART 6: Save results
#
# -------------------------------------------------------------------------------
# code author: Felix Aubele, Caroline Stiel (DIW Berlin)
#
# First created --- September 13, 2021									
# Last modified --- October 13, 2022
# ------------------------------------------------------------------------------
# content: This code conducts the emotion analysis for the housing articles 
# from 'F.A.Z.' using the new strategy to identify housing articles.
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
library(tm)
library(tidytext)
library(openxlsx)
library(stringr) # To use function str_replace
library(XML)
library(udpipe) # To stem German words
library(tidyverse)
library(stopwords)
library(data.table)
library(splitstackshape)

# set R system language (including dates etc.) to German
# ------------------------------------------------------
Sys.setlocale('LC_ALL', 'german')


# define threshold of keywords for article selection
# --------------------------------------------------
cKeyword = 1

# version of keywords
# ------------------
v_keywords = "new"

# choose dictionary
# -----------------
dict = "custom_nrc"

# choose lemmatization
# --------------------
lemma = "with"


# define paths and data input/output names
# ----------------------------------------
sFolder1 = "" # path where data, drafts etc. are stored
sFolder2 = "" # path where data, drafts etc. are stored
sInFile1 = "data pre-processing/Stopwords_German.xlsx"
sInFile2 = "data pre-processing/german-gsd-ud-2.5-191206.udpipet"
sInFile3 = "data pre-processing/German_specific_letters.xlsx"
sInFile4 = "Data/nrc/Housing_emotion_custom_dict.xlsx"
sInFile5 = "Data/FAZ/FAZ_Housing_articles_unambiguous_withtopics_2022-08-23.rds"

sOutFile = paste("Data/FAZ/articlelists_sentiment/emotion_analysis_FAZ_Housing_", dict,"_", lemma, "_lemma.xlsx", sep="")
sOutFile2 = paste("Data/FAZ/FAZ_Housing_lemma.rds", sep="")
sOutFile3 = "Data/FAZ/articlelists_sentiment/FAZ_Housing_emotion_count.xlsx"


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
Stopwords_pck = c("prozent", "dr","st","nnen","FAZ","geb","f.a.z.","abb"
                  ,Stopwords_pck)
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
# 3) Prepare emotions lexicon
# ==============================================================================

# ==============================================================================
# 3.1 Load lexicon and German-specific letters list
# ==============================================================================

# German-specific letters
# -----------------------
GeLe = read.xlsx(paste(sFolder2, sInFile3, sep=""))
NGeLe = nrow(GeLe)

# load emotions dictionary based on initialization
# ------------------------------------------------
emotions_ger = read.xlsx(paste(sFolder1, sInFile4, sep=""))


# ==============================================================================
# 3.2 Replace German letters and prepare emotion lexicon
# ==============================================================================

# replace German letters in emotion lexicon
# -----------------------------------------
for(iGL in 1:NGeLe)
{
  sLet_deu = GeLe$German[iGL]
  sLet_lat = GeLe$Transcription[iGL] 
  emotions_ger$Word = gsub(sLet_deu, sLet_lat, emotions_ger$Word)
}

# prepare emotion lexicon
# -----------------------
emotions_ger$Word = tolower(emotions_ger$Word)

Anger <- emotions_ger%>%filter(Emotion == "Anger")
Anticipation <- emotions_ger%>%filter(Emotion == "Anticipation")
Disgust <- emotions_ger%>%filter(Emotion == "Disgust")
Fear <- emotions_ger%>%filter(Emotion == "Fear")
Joy <- emotions_ger%>%filter(Emotion == "Joy")
Sadness <- emotions_ger%>%filter(Emotion == "Sadness")
Surprise <- emotions_ger%>%filter(Emotion == "Surprise")
Trust <- emotions_ger%>%filter(Emotion == "Trust")


# ==============================================================================
# 4) Prepare news data
# ==============================================================================

# ==============================================================================
# 4.1 Select housing-related articles in 'F.A.Z.'
# ==============================================================================

# Load all F.A.Z. articles
# ------------------------------
X_sel = readRDS(paste(sFolder1, sInFile5, sep=""))


# ==============================================================================
# 4.2 Pre-process text data using the tm-package
# ==============================================================================

# transform article collection into data frame
# --------------------------------------------
FAZ_df = tibble(id=1:length(X_sel$text),text=X_sel$text)
head(FAZ_df,5)

# perform lemmatization
# ----------------------
NCorpus = nrow(FAZ_df)

#ts_df = tibble(id= 1,text= "koennen koennt kann")
FAZ_df_lem <- FAZ_df
if(lemma == "with"){
  ud_model <- udpipe_download_model(language="german")
  ud_german <- udpipe_load_model(ud_model$file_model)
  for(jA in 1:NCorpus)
  {
    FAZ_df_lem[jA,2] = mylem(FAZ_df[jA,2])
    # show progress
    cat("\r",jA*100/NCorpus," % done ");flush.console()
  }
  if(file.exists(ud_model$file_model)) {file.remove(ud_model$file_model)}
}


# Replace umlaute in FAZ data
# ----------------------------
for(i in 1:NCorpus){
  
  FAZ_df_lem[i,2] = gsub("ä", "ae", FAZ_df_lem[i,2])
  FAZ_df_lem[i,2] = gsub("Ä", "Ae", FAZ_df_lem[i,2])
  FAZ_df_lem[i,2] = gsub("Ö", "Oe", FAZ_df_lem[i,2])
  FAZ_df_lem[i,2] = gsub("Ü", "Ue", FAZ_df_lem[i,2])
  FAZ_df_lem[i,2] = gsub("ö", "oe", FAZ_df_lem[i,2])
  FAZ_df_lem[i,2] = gsub("Ü", "ue", FAZ_df_lem[i,2])
  FAZ_df_lem[i,2] = gsub("ß", "ss", FAZ_df_lem[i,2])
}

# save data frame
# ----------------
saveRDS(FAZ_df_lem,file=paste0(sFolder1,sOutFile2))

# short cut: load data frame
# ---------------------------
FAZ_df_lem <- readRDS(file=paste0(sFolder1,sOutFile2))

# split into words (tokens), strip punctuation, and convert to lowercase
# ----------------------------------------------------------------------
# Notes: set to_lower=FALSE if you do not want to convert to lowercase
# Notes: unnest_tokens() splits words at German Umlaute (e.g. grer = 'gr'+'er').
# Therefore, replace German Umlaute with 'ae' etc. before pre-processing. Do the
# same for stopwords.
FAZ_tokens <- FAZ_df_lem %>% unnest_tokens(word, text)
head(taz_tokens, 10)

# remove stopwords
# ----------------
FAZ_sw <- FAZ_tokens %>% anti_join(Stopwords_pck)
head(FAZ_sw,10)

# compute total frequencies
# --------------------------
FAZ_wordcount <- FAZ_sw %>% count(word, sort=TRUE) %>% ungroup
head(FAZ_wordcount,10)

# compute frequencies by article
# -------------------------------
FAZ_wordcount_art <- FAZ_sw %>% count(id, word, sort=TRUE) %>% ungroup
head(FAZ_wordcount_art,10)


# ==============================================================================
# 5) Emotion analysis
# ==============================================================================

# ==============================================================================
# 5.1 Prepare data
# ==============================================================================

# construct 'Year' and select relevant columns
# -------------------------------------------
X_sel$Year <- substr(X_sel$date,5,8)
Y = X_sel %>% select(title,Year,starts_with("LDA_topic"))

NCorpus = nrow(FAZ_df_lem)


# Prepare empty emotions columns in FAZ data
# ------------------------------------------              
Y$Anger = 0
Y$Anticipation = 0
Y$Disgust = 0
Y$Fear = 0
Y$Joy = 0
Y$Sadness = 0
Y$Surprise = 0
Y$Trust = 0
Y$Anger_weight = 0
Y$Anticipation_weight = 0
Y$Disgust_weight = 0
Y$Fear_weight = 0
Y$Joy_weight = 0
Y$Sadness_weight = 0
Y$Surprise_weight = 0
Y$Trust_weight = 0


# Prepare empty emotions columns in emotion dictionary
# ----------------------------------------------------
Anger$Count = 0
Anticipation$Count = 0
Disgust$Count = 0
Fear$Count = 0
Joy$Count = 0
Sadness$Count = 0
Surprise$Count = 0
Trust$Count = 0


# ==============================================================================
# 5.2 Start analysis
# ==============================================================================

for(i in 1:NCorpus)
{
  # select all unique words in the article
  # --------------------------------------
  svWords_i = FAZ_wordcount_art %>% filter(id==i)
  Y$n_words_uq[i] = nrow(svWords_i)
  Y$n_words[i] = sum(svWords_i$n)
  
  
  # Example for Anger:
  # ------------------
  #intersect words in taz data and in emotion lexicon
  svIntersect = intersect(svWords_i$word, Anger$Word)
  
  # resulting number of words in FAZ data 
  Y$Anger[i] = length(svIntersect)
  
  # take positions of common words in word vector and dictionary to access 
  # corresponding frequency and emotion values
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Anger$Word)
  
  # multiply emotion value with the absolute frequency at which the word occurs
  # in article (emotion value may range from 0 to 3)
  Y$Anger_weight[i] = sum(Anger$weight[vSel2]*svWords_i$n[vSel1])
  
  # add occurrence of words to total word count for this emotion
  Anger$Count[vSel2] = Anger$Count[vSel2] + svWords_i$n[vSel1]
  
  # repeat for words with other emotions
  # ------------------------------------
  # anticipation
  # ------------
  svIntersect = intersect(svWords_i$word, Anticipation$Word)
  Y$Anticipation[i] = length(svIntersect)
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Anticipation$Word)
  Y$Anticipation_weight[i] = sum(Anticipation$weight[vSel2]*svWords_i$n[vSel1])
  Anticipation$Count[vSel2] = Anticipation$Count[vSel2] + svWords_i$n[vSel1]
  
  # disgust
  # -------
  svIntersect = intersect(svWords_i$word, Disgust$Word)
  Y$Disgust[i] = length(svIntersect)
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Disgust$Word)
  Y$Disgust_weight[i] = sum(Disgust$weight[vSel2]*svWords_i$n[vSel1])
  Disgust$Count[vSel2] = Disgust$Count[vSel2] + svWords_i$n[vSel1]
  
  # fear
  # ----
  svIntersect = intersect(svWords_i$word, Fear$Word)
  Y$Fear[i] = length(svIntersect)
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Fear$Word)
  Y$Fear_weight[i] = sum(Fear$weight[vSel2]*svWords_i$n[vSel1])
  Fear$Count[vSel2] = Fear$Count[vSel2] + svWords_i$n[vSel1]
  
  # joy
  # ---
  svIntersect = intersect(svWords_i$word, Joy$Word)
  Y$Joy[i] = length(svIntersect)
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Joy$Word)
  Y$Joy_weight[i] = sum(Joy$weight[vSel2]*svWords_i$n[vSel1])
  Joy$Count[vSel2] = Joy$Count[vSel2] + svWords_i$n[vSel1]
  
  # sadness
  # -------
  svIntersect = intersect(svWords_i$word, Sadness$Word)
  Y$Sadness[i] = length(svIntersect)
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Sadness$Word)
  Y$Sadness_weight[i] = sum(Sadness$weight[vSel2]*svWords_i$n[vSel1])
  Sadness$Count[vSel2] = Sadness$Count[vSel2] + svWords_i$n[vSel1]
  
  # surprise
  # --------
  svIntersect = intersect(svWords_i$word, Surprise$Word)
  Y$Surprise[i] = length(svIntersect)
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Surprise$Word)
  Y$Surprise_weight[i] = sum(Surprise$weight[vSel2]*svWords_i$n[vSel1])
  Surprise$Count[vSel2] = Surprise$Count[vSel2] + svWords_i$n[vSel1]
  
  # trust
  # ------
  svIntersect = intersect(svWords_i$word, Trust$Word)
  Y$Trust[i] = length(svIntersect)
  vSel1 = match(svIntersect, svWords_i$word)
  vSel2 = match(svIntersect, Trust$Word)
  Y$Trust_weight[i] = sum(Trust$weight[vSel2]*svWords_i$n[vSel1])
  Trust$Count[vSel2] = Trust$Count[vSel2] + svWords_i$n[vSel1]
  
  # show progress
  # -------------
  cat("\r",i*100/NCorpus," % done ");flush.console()
  
} # end of performers loop


# ==============================================================================
# 5.3 Results
# ==============================================================================

# add identifying column to emotion count dataframe
# ---------------------------------------------------
Anger = Anger%>%select(Word, Count)%>%mutate(Emotion = "Anger")
Anticipation = Anticipation%>%select(Word, Count)%>%mutate(Emotion = "Anticipation")
Disgust = Disgust%>%select(Word, Count)%>%mutate(Emotion = "Disgust")
Fear = Fear%>%select(Word, Count)%>%mutate(Emotion = "Fear")
Joy = Joy%>%select(Word, Count)%>%mutate(Emotion = "Joy")
Sadness = Sadness%>%select(Word, Count)%>%mutate(Emotion = "Sadness")
Surprise = Surprise%>%select(Word, Count)%>%mutate(Emotion = "Surprise")
Trust = Trust%>%select(Word, Count)%>%mutate(Emotion = "Trust")


# combine vectors to dataframe
# ----------------------------
emotion_count = do.call(rbind, list(Anger, Anticipation, Disgust, Fear,Joy, Sadness, Surprise, Trust))

# select relevant columns (emotion weights) from lexicon
# ------------------------------------------------------
emotions_j = emotions_ger%>%select(c(Word, Emotion, weight))

# merge both data frames and compute importance as count * weight
# --------------------------------------------------------------
# note: if weight = 0, importance = 0. 
emotion_count = emotion_count%>%left_join(emotions_j)%>%mutate(importance = Count*weight)

# top emotion words
# -----------------
top_10 = emotion_count %>%                                     
  arrange(desc(importance)) %>% 
  group_by(Emotion) %>%
  slice(1:10)

# ==============================================================================
# 6) Save results
# ==============================================================================

write.xlsx(Y, paste(sFolder1, sOutFile, sep=""), overwrite = T)
write.xlsx(emotion_count, paste(sFolder1, sOutFile3, sep=""), overwrite = T)


# ==============================================================================
proc.time() - Begin # elapsed time
# ============================End of file ======================================
