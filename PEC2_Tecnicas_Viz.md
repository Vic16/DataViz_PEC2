PEC2\_TecnicasViz
================
Victor H. Ayala Sánchez
19/4/2022

Librerias

``` r
library(tidyverse)
library(ggstream)
library(lubridate)

library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tm)
```

## Tecnica 1

``` r
data_1 <- read.csv2("data/Tec1/twitter_training.csv", sep = ",", col.names = c('TweetID','Entity','Sentiment','TweetContent'), nrows = 2500)

docs <- Corpus(VectorSource(data_1$TweetContent))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))


dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df = df[-1,]

wordcloud(words = df$word, freq = df$freq, 
          min.freq = 5, max.words=500, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
```

![](PEC2_Tecnicas_Viz_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->