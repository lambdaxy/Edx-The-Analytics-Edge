library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(words = colnames(allTweets),freq = colSums(allTweets))

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(words = colnames(allTweets),freq = colSums(allTweets), scale = c(2, 0.25))
wordcloud(words = colnames(allTweets),freq = colSums(allTweets), scale = c(2, 0.25), random.order = FALSE)

display.brewer.all()

wordcloud(words = colnames(allTweets),freq = colSums(allTweets), scale = c(2, 0.25),colors=brewer.pal(9, "Blues"))
