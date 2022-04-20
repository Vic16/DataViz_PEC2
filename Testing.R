library(tidyverse)
library(ggstream)
library(lubridate)

library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tm)

library(waffle)
library(fontawesome)
library(extrafont)
library(emojifont)


## Tecnica 1
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



## Tecnica 2
prods.all$Date2 <- mdy(prods.all$Date2)


data_2 <- read.csv2("data/Tec2/commodity 2000-2022.csv", sep = ",")
data_2$Date2 <- ymd(data_2$Date)
data_2$Year <- year(data_2$Date2)

data_2<-data_2 %>% 
  arrange(Symbol,Date2, Close) 

data_2 %>% 
  group_by(Symbol, Year) %>% 
  summarize(MeanClose = mean(as.numeric(Close), na.rm=TRUE)) %>%
  ggplot(aes(x=Year, y=MeanClose, fill=Symbol)) + geom_stream()

data_2 %>% 
  group_by(Symbol, Year) %>% 
  summarize(MeanClose = mean(as.numeric(Close), na.rm=TRUE)) %>%
  ggplot(aes(x=Year, y=MeanClose, fill=Symbol)) + geom_stream(type = "ridge") 


## Tecnica 3

data_3 <- read.csv2("data/Tec3/Unicorn_Companies.csv", sep = ",")

comp <- data_3 %>% 
  group_by(Country) %>% 
  summarize(NumberOfCompanies = n()) %>% 
  arrange(NumberOfCompanies, desc(NumberOfCompanies)) %>% 
  tail(5)

comp <- c(`USA`=536,`China`=168,`India`=63,`United Kingdom`=42,`Germany`=24)

waffle(comp/5, rows = 10, glyph_size = 10, title = "Top 5 Countries by Number of Unicorn Companies",  pad = 12)


