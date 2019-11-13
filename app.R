data <- readline("C:\Users\Thon Okocha\Desktop\macos.txt") 
data <- sapply(data,function(row) iconv(row, 'latin1', 'ASCII',sub = ''))
library(tm)
newCorpus <- Corpus(VectorSource(data))
newCorpus <- tm_map(newCorpus, content_transformer(tolower))
newCorpus <- tm_map(newCorpus, removePunctuation)
newCorpus <- tm_map(newCorpus, removeNumbers)
newCorpus <- tm_map(newCorpus, stripWhitespace)
newCorpus <- tm_map(newCorpus, removeWords, stopwords("english"))
newCorpus <- tm_map(newCorpus, stemDocument, language = 'english')
ndtm <- DocumentTermMatrix(newCorpus)
nndtm <- as.matrix(ndtm)
library(wordcloud)
library(RColorBrewer)


f <- colSums(nndtm)
f <- sort(f, decreasing = TRUE)
head(f)
words <- names(f)
words <- as.vector(words)
f1 <- as.data.frame(f)
write.csv(x = f1,file = '../Desktop/newdata.txt')
freq <- f1$f
wordcloud(words = words, 
         freq = freq, 
          min.freq = 4,
          scale = c(5,0.5),
          random.order = FALSE,
          colors = brewer.pal(20, "Dark2"),bg='blue')
library('syuzhet')
d<-get_nrc_sentiment(words)
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:6]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:9,]
library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("song sentiments")
