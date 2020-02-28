library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#Set constant request URL
requestURL <- "https://api.twitter.com/oauth/request_token"

# Set constant access URL
accessURL <- "https://api.twitter.com/oauth/access_token"

# Set constant auth URL
authURL <- "https://api.twitter.com/oauth/authorize"


# Put the both Consumer Key and Consumer Secret key from Twitter App.
consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE"  
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"


#Create the authorization object by calling function OAuthFactory
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)

consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE" 
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"
access_Token <- "3060838521-u5eXreDFHOqaxUcvTYMFyuEXImu5RlpdiY436h8" 
access_Secret <- "Q55FxITLmzlJWW4xpNbwnsW2UPXQZL4KiOWf9QdsDlYKt"

# Create Twitter connection
setup_twitter_oauth(consumerKey,consumerSecret,access_Token,access_Secret)

#Objectname <- searchTwitter(searchString, n=no.of tweets, lang=NULL)
# 
namo <- searchTwitter('facebook', n=300, lang="en")


library(wordcloud)
library(SnowballC)
library(tm)
namo
namo_text <- sapply(namo, function(x) x$getText())
namo_text_corpus <- iconv(namo_text, 'UTF-8', 'ASCII')

namo_text_corpus <- Corpus(VectorSource(namo_text))
namo_text_corpus <- tm_map(namo_text_corpus, removePunctuation)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(tolower))
namo_text_corpus <- tm_map(namo_text_corpus, function(x)removeWords(x,stopwords()))
namo_text_corpus <- tm_map(namo_text_corpus, removeWords, c('RT', 'are','that'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(removeURL))

insta_2 <- TermDocumentMatrix(namo_text_corpus)
insta_2 <- as.matrix(insta_2)
insta_2 <- sort(rowSums(insta_2),decreasing=TRUE)
insta_2
d <- data.frame(word = names(insta_2), freq = insta_2)
View(d)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

namo_text_corpus=str_replace_all(namo_text_corpus,"[^[:graph:]]", " ") 
tm_map(namo_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
namo_text <- sapply(bjp, function(x) x$getText())
namo_text_corpus <- Corpus(VectorSource(namo_text))
namo_text_corpus <- tm_map(namo_text_corpus, removePunctuation)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(tolower))
namo_text_corpus <- tm_map(namo_text_corpus, function(x)removeWords(x,stopwords()))
namo_text_corpus <- tm_map(namo_text_corpus, removeWords, c('RT', 'are','that'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(removeURL))

