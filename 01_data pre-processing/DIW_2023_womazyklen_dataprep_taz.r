# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: PRE-PROCESSES NEWSPAPER ARTICLES (taz)
#
# 		OUTLINE: PART 1: Prepare data
#				 PART 2: Read-in text from articles
#
#
# -------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
#
# First created --- May 4, 2022										
# Last modified --- May 6, 2022
# ------------------------------------------------------------------------------
# content: This code pre-processes the text data from the German newspaper 
# 'die tageszeitung' (taz) and builds a data frame with all articles published
# (print) in taz between 1991 and 2021.
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

# Choose sample issue
# -------------------
cYear = 2014
sIssue = paste(cYear, "11", sep="-")

# define paths and data input/output names
# ----------------------------------------
sFolder1 = "" # path where data, drafts etc. are stored
sFolder2 = "" # path where local repositories are located
sInFile = "German_specific_letters.xlsx"
sOutFile = "taz_articles_1991-2021.rds"

# ==============================================================================
# 1) Prepare data
# ==============================================================================

# The articles are stored in .xml-files with each .xml-file corresponding to one
# issue (day). The issues are saved in three folders corresponding to three 
# periods: 1991-1999, 2000-2009, and 2010-2021.

# Load German-specific letters and their transcript to normal letters
# -------------------------------------------------------------------
# ex.: ü = ue, ß = ss etc.
GeLe = read.xlsx(paste0(sFolder2,"womazyklen/",sInFile))
NGeLe = nrow(GeLe)

# Define folder structure where articles are stored
# -------------------------------------------------
cPeriod = c("taz-1991-1999","taz-2000-2009","taz-2010-2021")
Nperiods = length(cPeriod)

# ==============================================================================
# 2) Read-in text from articles
# ==============================================================================

# Generate empty vector to save the final data
# -------------------------------------------
Articles = c()

# Extract article texts and metadata from .xml files (loop)
# ---------------------------------------------------------
# for each period (folder)
# ------------------------
for(p in 1:Nperiods){
  # list all .xml-files in this folder
  svFile = list.files(paste0(sFolder1, "Data/taz/article_texts/", cPeriod[p]))
  NFile = length(svFile)
  # for each issue (file) i within folder p
  # ---------------------------------------
  for(i in 1:NFile){
    # load article texts
    # ------------------
    # construct directory to .xml-file
    sInPath = paste0(sFolder1, "Data/taz/article_texts/",cPeriod[p],"/",svFile[i])
    # read-in .xml file
    X_i = read_xml(sInPath, encoding="UTF-8")
    # for each article within the xml-file, extract date and author
    metadata <- xml_find_all(X_i, ".//metadaten")
    date = metadata %>% xml_find_first("./quelle/datum") %>% xml_text()
    author = metadata %>% xml_find_first("./autor/autor-name") %>% xml_text()
    # for each article, extract title and article text
    inhalt <- xml_find_all(X_i, ".//inhalt")
    title = inhalt %>% xml_find_first("./titel-liste/titel") %>% xml_text()
    text = inhalt %>% xml_find_first("./text") %>% xml_text
    # clean texts
    # -----------
    # search for German-specific letters
    for(iGL in 1:NGeLe){
      # read-in German-specific letter
      sLet_deu = GeLe$German[iGL]
      # read-in corresponding transcription
      sLet_lat = GeLe$Transcription[iGL] 
      # replace German-specific letter with transcription
      text = gsub(sLet_deu, sLet_lat, text)
    }
    # replace with lower case letters
    text <- tolower(text)
    # delete author at the end of the text
    for(j in 1:length(text)){
      text[j] <- ifelse(is.na(author[j])==FALSE,gsub(author[j],"",text[j]),text[j])
    }
    # delete location at beginning of text
    for(j in 1:length(text)){
      text[j] <- ifelse(str_detect(text[j],"taz[)]")==TRUE
                        ,unlist(lapply(strsplit(as.character(text[j]), "taz)")
                                       , function(x) x[2])),text[j])
    }
    # trim white spaces
    text <- trimws(text)
    # create data frame
    # -----------------
    Articles_i <- as.data.frame(cbind(date,author,title,text))
    # add to previous set of articles
    # -------------------------------
    Articles = rbind(Articles,Articles_i)
    # show progress
    cat("\r",i*100/NFile," % done ");flush.console()
  }
}


# results in a data frame with 4 columns
# --------------------------------------
# date: publication date
# author: author
# title: title of the newspaper article
# text: text of the newspaper article

# export articles (dataframe) to .rds file
# ----------------------------------------
saveRDS(Articles,file=paste0(sFolder1,"Data/taz/article_texts/",sOutFile))


# ==============================================================================
proc.time() - Begin # elapsed time
# ============================End of file ======================================