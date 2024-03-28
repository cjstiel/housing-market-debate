# The housing market in public and political debate: a text analysis

research project 'Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: Eine Betrachtung der langfristigen Verteilungswirkungen von Wohnungsmarktzyklen (WLSN)' at DIW Berlin
 | 2020-2023

Contributors: Felix Aubele, Konstantin Kholodilin, Linus Pfeiffer, Caroline Stiel 

The following codes describe how the data is pre-processed and analyzed using `R`.


## Data preparation

Pre-processes the news articles and minutes of the plenary proceedings for different types of text analyses. The data consist of 

- newspaper articles from different German media outlets
- minutes of plenary proceedings _(Berliner Abgeordnetenhaus)_

### Selection of articles dealing with housing market
First, we select all articles dealing with the housing market based on a user-defined list of keywords. The keywords were identified by reviewing a sample of 100 newspaper articles from various media outlets and websites dealing with housing issues. The keywords are categorized as *unambiguous* (the keyword clearly identifies housing market topics) and *ambiguous* (the keyword is frequently used in articles about the housing market, but there is no unique relationship). In the remainder of the analysis, articles are assumed to report on the housing market if they contain at least one unambiguous keyword.

### Pre-processing of the text data
Second, we apply the following steps for pre-processing:

1. Delete punctuation, numbers etc.
2. Transform all letters to lowercase.
3. Remove stop words.
4. Transform text into lists of words(_bag of words approach)._
5. Compute document-term-matrix (DTM)

## Topic model

We estimate a _Latent Dirichlet Allocation (LDA) model_ with 8 topics for each media outlet and the minutes from the plenary proceedings.


## Sentiment and emotion analysis

Finally, we analyze the sentiments and emotions of the newspaper articles and the minutes. The focus is on

- whether the reporting on the housing market tends to be positive or negative in tone and how this evolves over time
- how the tone differs between topics
- which emotions (anger, fear, ...) dominate media coverage of the housing market

## Further reading

The results are published in Aubele, F..; Baake, P.; Duso, T.; Kholodilin, K.A.; Pfeiffer, L.; Stiel, C. (2023): [Wohnkosten, Lebenszufriedenheit, Sicherheitsempfinden und Narrative: Eine Betrachtung der langfristigen Verteilungswirkungen von Wohnungsmarktzyklen (WLSN).](https://www.diw.de/documents/publikationen/73/diw_01.c.887817.de/diwkompakt_2023-199.pdf) Politikberatung kompakt 199. DIW Berlin.

