# ==============================================================================
#                             DIW Berlin
#
# Project:  Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: 
#           Eine Betrachtung der langfristigen Verteilungswirkungen von
#           Wohnungsmarktzyklen (2020-2023)
#
# ------------------------------------------------------------------------------
#
#	 	CONTENT: IDENTIFY HOUSING-MARKET RELATED ARTICLES (F.A.Z.)
#
# 		OUTLINE: PART 1: Define keywords
#				 PART 2: Count occurence of keywords in each article
#				 PART 3: Identify articles dealing with housing market issues
#				 PART 4: Summary statistics
#						4.1 Number of articles per year
#						4.2 Number of keywords per article
# 						4.3 Most and least frequent keywords
#
#
# -------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
#
# First created --- August 13, 2022										
# Last modified --- September 02, 2022
# ------------------------------------------------------------------------------
# content: This code is designed to identify and describe the articles of the 
# German newspaper 'Frankfurter Allgemeine Zeitung (F.A.Z.)' dealing with 
# housing issues.
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
library(stringr) # To use function str_replace
library(XML)
library(dplyr)
library(tibble)
library(stargazer)
library(readstata13)


# set R system language (including dates etc.) to German
# ------------------------------------------------------
Sys.setlocale('LC_ALL', 'german')


# define paths and data input/output names
# -------------------------------------------------
sFolder1 = "" # path where data, drafts etc. are stored
sFolder2 = "" # path where local repositories are located
sInFile = "Data/FAZ/article_texts/FAZ_articles_1950-2021.rds"
sInFile2 = "housing_market_keywords.xlsx"
sInFile3 = "Data/FAZ/FAZ_Statistik_total_number_of_articles_1950-2021.xlsx"
sOutFile = "Data/FAZ/FAZ_list_articles_with_keywords_1950-2021.rds"
sOutFile2 = "Data/FAZ/FAZ_articles_with_keywords_1950-2021.rds"
sOutFile3 = "Data/FAZ/FAZ_Housing_articles_unambiguous_2022-08-13.xlsx"
sOutFile4 = "Data/FAZ/FAZ_Housing_articles_unambiguous_withText_2022-08-13.rds"
sOutFile_graph1 = "Draft/figures/data-preparation/Fig_Housing_notions_frequency_FAZ.pdf"
sOutFile_graph2 = "Draft/figures/data-preparation/Fig_Housing_notions_most_common_keywords_FAZ.pdf"
sOutFile_graph3 = "Draft/figures/data-preparation/Fig_Housing_market_articles_per_year_FAZ.pdf"


# ==============================================================================
# 1) Define keywords
# ==============================================================================

# new strategy: use user-defined keywords based on manually scanning housing
# market newspaper articles
# --------------------------------------------------
# reads in data frame with following structure:
# keyword: word related to housing market topics
# unambiguous: 1 if word clearly identifies housing market topic, 0 otherwise
# ambiguous: 1 if word used in other context as well, 0 otherwise
keywords_list = read.xlsx(paste(sFolder2, sInFile2, sep=""))

# transform all letters to lower case
# -----------------------------------
keywords_list$keyword = tolower(keywords_list$keyword)

# show frequency of unambiguous and ambiguous words
# -------------------------------------------------
table(keywords_list$unambiguous,dnn = "number of unambiguous keywords")
table(keywords_list$ambiguous,dnn = "number of ambiguous keywords")

# keep simple list of keywords without ambiguity classifier
# ---------------------------------------------------------
svKeyword = keywords_list$keyword


# ==============================================================================
# 2) Count occurence of keywords in each article
# ==============================================================================

# attention: full sample takes about 28 hours. comment out when not used.

# Load list with articles and publication year
-------------------------------------------
X = readRDS(paste0(sFolder1, sInFile))


# Prepare empty column for each keyword (default = 0/no)
# ------------------------------------------------------
# attention: memory in VDI not sufficient for adding all columns at once. Instead,
# proceed in steps: X[, svKeyword[1:50]] = 0, X[, svKeyword[51:100]] = 0 etc.
X[, svKeyword] = 0
NArt = nrow(X)
# NArt = 100


# Load article text and search for keyword
# ----------------------------------------
# for each article
  for(i in 1:NArt)
  {
    # search for each keyword in article text
    for(j in svKeyword)
    {
      Sel = grep(j, X$text[i])
      if(length(Sel)>0)
      {
        # if article contains keyword, set dummy to 1
        X[i, j] = 1
      }
    }
  # show progress
  cat("\r",i*100/NArt," % done ");flush.console()
    }

# export results to .rds
# -----------------------
# delete article texts and only keep date, title, and keywords
X_oT <-  X %>% select(-text)
saveRDS(X_oT,paste0(sFolder1, sOutFile))
saveRDS(X,paste0(sFolder1, sOutFile2))

# ==============================================================================
# 3) Identify articles dealing with housing market issues
# ==============================================================================

# short cut: load results
# -----------------------
X = readRDS(paste0(sFolder1, sOutFile2))

# filter for ambiguous and unambiguous keywords
# --------------------------------------------
svKeyword_unambiguous = keywords_list %>% filter(unambiguous==1) %>% select(keyword) 
head(svKeyword_unambiguous,5)

# transform data frame to character list
# --------------------------------------
svKeyword_unambiguous = svKeyword_unambiguous$keyword 

# Compute number of keywords contained in each article
# ----------------------------------------------------
X$Sum = rowSums(X[, svKeyword_unambiguous])

# Construct variable 'Year' and show total number of articles by year
# -------------------------------------------------------------------
X$Year <- substr(X$date,5,8)
addmargins(table(X$Year))

# Select all articles that contain at least one unambiguous keyword
# -----------------------------------------------------------------
vSel = which(X$Sum>=1)
head(vSel,5)
X_sel = X[vSel,]


# export article list
# --------------------
X_sel_oT <-  X_sel %>% select(-text)
write.xlsx(X_sel_oT, paste0(sFolder1, sOutFile3))
saveRDS(X_sel, paste0(sFolder1, sOutFile4))

# How many articles cover housing market topics?
# ------------------------------------------------
#nrow(X_sel)


# ==============================================================================
# 4) Summary statistics
# ==============================================================================

# short cut: load results
# -----------------------
X_sel = readRDS(paste0(sFolder1, sOutFile4))

# load statistics on total number of articles
# -------------------------------------------
X = read.xlsx(paste0(sFolder1,sInFile3))
X <- rename(X,year=Jahrg‰nge,freq=Anzahl)

# ==============================================================================
# 4.1 Number of articles per year
# ==============================================================================

# ==============================================================================
# 4.1.1 Absolute frequency
# ==============================================================================

# descriptive statistics
# ----------------------
addmargins(table(X_sel$Year))

# English graph
# ------------
pdf(paste(sFolder1, sOutFile_graph3, sep=""))
par(mar=c(5,5,5,1))
plot(table(X_sel$Year),type="l",ylab="number of articles"
     ,xlab="year",lwd=5,bty="l",main="F.A.Z. articles on housing issues"
     ,font.main=1,las=1)
segments(x0=1973,y0=-60,x1=1973,y1=882,col="red",lwd=2)
segments(x0=1981,y0=-60,x1=1981,y1=1057,col="red",lwd=2)
segments(x0=1993,y0=-60,x1=1993,y1=1720,col="red",lwd=2)
segments(x0=2004,y0=-60,x1=2004,y1=589,col="red",lwd=2)
segments(x0=2013,y0=-60,x1=2013,y1=1295,col="red",lwd=2)
segments(x0=2019,y0=-60,x1=2019,y1=1365,col="red",lwd=2)
text(1974,100,"1973",srt=90)
text(1982,100,"1981",srt=90)
text(1994,100,"1993",srt=90)
text(2005,100,"2004",srt=90)
text(2014,100,"2013",srt=90)
text(2020,100,"2019",srt=90)

# German graph
# ------------
par(mar=c(5,5,5,1))
plot(table(X_sel$Year),type="l",ylab="Anzahl der Artikel"
     ,xlab="Jahr",lwd=5,bty="l",main="F.A.Z. Artikel zum Thema Wohnungsmarkt"
     ,font.main=1,las=1,xaxt="n")
axis(1, at=seq(1950,2021,5), labels=seq(1950,2021,5))
#abline(v=c(1973,1981,1990,1998,2002),col="red",lwd=2)
segments(x0=1973,y0=-60,x1=1973,y1=882,col="red",lwd=2)
segments(x0=1981,y0=-60,x1=1981,y1=1057,col="red",lwd=2)
segments(x0=1993,y0=-60,x1=1993,y1=1720,col="red",lwd=2)
segments(x0=2004,y0=-60,x1=2004,y1=589,col="red",lwd=2)
segments(x0=2013,y0=-60,x1=2013,y1=1295,col="red",lwd=2)
segments(x0=2019,y0=-60,x1=2019,y1=1365,col="red",lwd=2)
text(1974,100,"1973",srt=90)
text(1982,100,"1981",srt=90)
text(1994,100,"1993",srt=90)
text(2005,100,"2004",srt=90)
text(2014,100,"2013",srt=90)
text(2020,100,"2019",srt=90)


# ==============================================================================
# 4.1.2 Relative frequency
# ==============================================================================

# share in total number of F.A.Z. articles
# --------------------------------------
round(table(X_sel$Year)/X$freq,3)


# English graph
# -------------
par(mar=c(5,5,5,1))
plot(round((table(X_sel$Year)/X$freq)*100,1),type="l"
     ,ylab="share of articles in %",xlab="year",lwd=5,bty="l",las=1
     ,main="share of articles on housing issues \n in total number of F.A.Z. articles"
     ,font.main=1)
#abline(v=c(1971,1981,1990,1998,2008),col="red",lwd=2)
segments(x0=1973,y0=-60,x1=1973,y1=1.9,col="red",lwd=2)
segments(x0=1981,y0=-60,x1=1981,y1=2.2,col="red",lwd=2)
segments(x0=1992,y0=-60,x1=1992,y1=2.7,col="red",lwd=2)
segments(x0=2011,y0=-60,x1=2011,y1=1.2,col="red",lwd=2)
text(1974,.5,"1973",srt=90)
text(1982,.5,"1981",srt=90)
text(1993,.5,"1992",srt=90)
text(2012,.5,"2011",srt=90)

# German graph
# -------------
par(mar=c(5,5,5,1))
plot(round((table(X_sel$Year)/X$freq)*100,1),type="l"
     ,ylab="Anteil an allen Artikeln in %",xlab="Jahr",lwd=5,bty="l",las=1
     ,main="Anteil der Artikel ¸ber den Wohnungsmarkt \n an allen F.A.Z.-Artikeln"
     ,font.main=1)
#abline(v=c(1971,1981,1990,1998,2008),col="red",lwd=2)
segments(x0=1973,y0=-60,x1=1973,y1=1.9,col="red",lwd=2)
segments(x0=1981,y0=-60,x1=1981,y1=2.2,col="red",lwd=2)
segments(x0=1992,y0=-60,x1=1992,y1=2.7,col="red",lwd=2)
segments(x0=2011,y0=-60,x1=2011,y1=1.2,col="red",lwd=2)
text(1974,.5,"1973",srt=90)
text(1982,.5,"1981",srt=90)
text(1993,.5,"1992",srt=90)
text(2012,.5,"2011",srt=90)
dev.off()

# ==============================================================================
# 4.2 Number of keywords per article
# ==============================================================================

# frequency table
# ---------------
Tab = table(X_sel$Sum)
Max = max(Tab)

vX = as.integer(names(Tab)) + 0.5
YLim = range(pretty(Tab))

# graph with frequency table of unambiguous keywords (en/de)
# ----------------------------------------------------------
pdf(paste(sFolder1, sOutFile_graph1, sep=""))
par(mar=c(3,5,1,1))
barplot(Tab, space=0, col="cyan4", ylim=YLim, las=1) #c(0, 1.05*Max))
text(Tab, x=vX, y=Tab, pos=3, cex=0.8)
mtext("Number of housing keywords per article", side=1, line=2)
mtext("Number of articles", side=2, line=4)

par(mar=c(3,5,1,1))
barplot(Tab, space=0, col="cyan4", ylim=YLim, las=1) #c(0, 1.05*Max))
text(Tab, x=vX, y=Tab, pos=3, cex=0.8)
mtext("Anzahl Schlagwoerter pro Artikel", side=1, line=2)
mtext("Anzahl der Artikel", side=2, line=4)
dev.off()


# ==============================================================================
# 4.3 Most and least frequent keywords
# ==============================================================================

# compute in how many articles each keyword occurs
# ------------------------------------------------
keywords_freq = data.frame("freq"=colSums(Filter(is.numeric, X_sel)))

# focus on unambiguous keywords and sort in descending order
# ----------------------------------------------------------
keywords_unamb_freq <- keywords_freq %>%
  rownames_to_column('keyword') %>%
  filter_if(is.numeric, all_vars(row.names(keywords_freq) %in% svKeyword_unambiguous))  %>% arrange(desc(freq))

# list most and least frequent unambiguous keywords
# --------------------------------------------------
head(keywords_unamb_freq,10)
tail(keywords_unamb_freq,10)


# english graphs
# --------------
pdf(paste(sFolder1, sOutFile_graph2, sep=""))
par(mar=c(6,12,1,1))
barplot(height=keywords_unamb_freq[10:1,2],horiz=TRUE,col="cyan4",las=1,
        names=keywords_unamb_freq[10:1,1])
mtext("Top10 unambiguous keywords", side=2, line=10)
mtext("number of articles", side=1, line=4)

par(mar=c(6,12,1,1))
barplot(height=keywords_unamb_freq[20:1,2],horiz=TRUE,col="cyan4",las=1,
        names=keywords_unamb_freq[20:1,1])
mtext("Top20 unambiguous keywords", side=2, line=10)
mtext("number of articles", side=1, line=4)

par(mar=c(5,14,1,1))
barplot(height=tail(keywords_unamb_freq[,2],10),horiz=TRUE,col="cyan4",las=1,
        names=tail(keywords_unamb_freq[,1],10))
mtext("last 10 unambiguous keywords", side=2, line=12)
mtext("number of articles", side=1, line=3)

par(mar=c(5,14,1,1))
barplot(height=tail(keywords_unamb_freq[,2],20),horiz=TRUE,col="cyan4",las=1,
        names=tail(keywords_unamb_freq[,1],20))
mtext("last 20 unambiguous keywords", side=2, line=12)
mtext("number of articles", side=1, line=3)

# German graphs
# -------------
# red4: controversial, yellow3: policy measure
par(mar=c(6,12,1,1))
barplot(height=keywords_unamb_freq[20:1,2],horiz=TRUE
        ,col=c("yellow3","red4","cyan4","red4","yellow3",rep("cyan4",3)
               ,"yellow3","red4",rep("cyan4",2),"red4",rep("cyan4",2),"red4"
               ,rep("cyan4",3),"red4")
               ,las=1,names=keywords_unamb_freq[20:1,1])
mtext("Anzahl der Artikel", side=1, line=4)
legend(-15000,-2,inset=c(-0.2,0),c("neutral","kontrovers","polit. Maﬂnahme")
       ,col=c("cyan4","red4","yellow3"),xpd=TRUE,box.lty=0,lty=1,lwd=5)
dev.off()

# ==============================================================================
proc.time() - Begin # elapsed time
# ============================End of file ======================================