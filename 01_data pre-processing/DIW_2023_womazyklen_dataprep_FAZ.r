# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: PRE-PROCESSES NEWSPAPER ARTICLES (F.A.Z.)
#
# 		OUTLINE: PART 1: Prepare data
#				 PART 2: Read-in text from articles
#
#
# -------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
#
# First created --- August 12, 2022										
# Last modified --- August 22, 2022
# ------------------------------------------------------------------------------
# content: This code pre-processes the text data from the German newspaper 
# 'Frankfurter Allgemeine Zeitung' (F.A.Z.) and builds a data frame with all 
# articles published between 1950 and 2021 containing #wohnung#.
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
library(openxlsx)
library(stringr) # To use function str_replace
library(XML)
library(xml2)
library(tidyr)


# set R system language (including dates etc.) to German
# ------------------------------------------------------
Sys.setlocale('LC_ALL', 'german')


# define paths and data input/output names
# ----------------------------------------
sFolder1 = "" # path where data, drafts etc. are stored
sFolder2 = "" # path where local repositories are located
sInFile = "German_specific_letters.xlsx"
sOutFile = "FAZ_articles_1950-2021.rds"

# ==============================================================================
# 1) Prepare data
# ==============================================================================

# The articles are stored in .xml-files with each .xml-file corresponding to one
# year.

# list all .xml-files in the folder
# ---------------------------------
svFile = list.files(paste0(sFolder1, "Data/FAZ/article_texts/FAZ_1950-2022/"))
NFile = length(svFile)

# Load German-specific letters and their transcript to normal letters
# -------------------------------------------------------------------
# ex.: ü = ue, ß = ss etc.
GeLe = read.xlsx(paste0(sFolder2,"womazyklen/",sInFile))
NGeLe = nrow(GeLe)

# ==============================================================================
# 2) Read-in text from articles
# ==============================================================================

# Generate empty vector to save the final data
# -------------------------------------------
Articles = c()

# Extract article texts and metadata from .xml files (loop)
# ---------------------------------------------------------
  # for each year (file) i 
  # ----------------------
  for(i in 1:NFile){
    # load article texts
    # ------------------
    # construct directory to .xml-file
    sInPath = paste0(sFolder1, "Data/FAZ/article_texts/FAZ_1950-2022/",svFile[i])
    # read-in .xml file
    X_i = read_xml(sInPath, encoding="iso-8859-1")
    # for each article within the xml-file, extract date and author
    metadata <- xml_find_all(X_i, ".//metadaten")
    date = metadata %>% xml_find_first("./quelle/datum") %>% xml_text()
    # for each article, extract title and article text
    inhalt <- xml_find_all(X_i, ".//inhalt")
    title = inhalt %>% xml_find_first("./titel-liste/titel") %>% xml_text()
    text = inhalt %>% xml_find_first("./text") %>% xml_text
    first_par = inhalt %>% xml_find_first("./text/absatz") %>% xml_text()
    # clean text
    # ----------
      # remove first paragraph with author name and location (recent years)
      # -------------------------------------------------------------------
      # Some articles start their first paragraph with "Von 'author', 'location'"
      # algorithm: check whether first paragraph is composed of less than 100
      # characters and starts with 'Von'. If so, remove, otherwise, keep.
    for(j in 1:length(text)){
      text[j] <- ifelse((nchar(first_par[j])<50 & startsWith(first_par[j],"Von")==TRUE)
        ,unlist(lapply(strsplit(as.character(text[j]),first_par[j]), function(x) x[2]))
        ,text[j])
    }
      #replace German-specific letters
      # ----------------------------
    for(iGL in 1:NGeLe){
      # read-in German-specific letter
      sLet_deu = GeLe$German[iGL]
      # read-in corresponding transcription
      sLet_lat = GeLe$Transcription[iGL] 
      # replace German-specific letter with transcription
      text = gsub(sLet_deu, sLet_lat, text)
    }
      # remove location and author at the beginning of the article (early years)
      # ------------------------------------------------------------------------
      # Some articles start with location or author written in uppercase letters
      # followed by fullstop (e.g., "FRANKFURT. Hier gibt es...")
      # algorithm: Check whether article starts with 2 capital letters in a row.
      # To do so, locate first occurrence of 2 capital letters in text and check 
      # whether second capital letter is at position 2. Note: As of now, the
      # startsWith()-fctcannot handle regex, therefore use this (rather 
      # complicated) approach.
      # If not, keep text. Otherwise, remove everything before first fullstop.
    for(j in 1:length(text)){
    text[j] <- ifelse(is.na(str_locate(text[j],"^[A-Z]{2}")[,2])==TRUE
                                          ,text[j]
                      ,ifelse(str_locate(text[j],"^[A-Z]{2}")[,2]==2
                   ,unlist(lapply(strsplit(as.character(text[j]), "\\.")
                                  , function(x) x[2]))))
    }
      # replace with lower case letters
      # -------------------------------
    text <- tolower(text)
      # trim white spaces
      # -----------------
    text <- trimws(text)
    # create data frame
    # -----------------
    Articles_i <- as.data.frame(cbind(date,title,text))
    # add to previous set of articles
    # -------------------------------
    Articles = rbind(Articles,Articles_i)
    # show progress
    cat("\r",i*100/NFile," % done ");flush.console()
  }

# results in a data frame with 4 columns
# --------------------------------------
# date: publication date
# title: title of the newspaper article
# text: text of the newspaper article

# Check how many article texts went missing in data preparation algo
# -----------------------------------------------------------------
length(which(is.na(Articles$text)==TRUE))

# export articles (dataframe) to .rds file
# ----------------------------------------
saveRDS(Articles,file=paste0(sFolder1,"Data/FAZ/article_texts/",sOutFile))


# ==============================================================================
proc.time() - Begin # elapsed time
# ============================End of file ======================================