# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("tm")
#install.packages("SnowballC")
#install.packages("RWeka")
#install.packages("rJava")
#install.packages("RWekajars")


library("twitteR")
library("wordcloud")
library("tm")
library("SnowballC")
library("RWeka")
library("rJava")
library("RWekajars")

#necessary file for Windows
## download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#the cainfo parameter is necessary only on Windows
#r_stats <- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")

########
#GET TWEETS BY QUERY STRING
########
r_stats <- searchTwitter("#Rstats", n=3)  #n is The maximum number of tweets to return
class(r_stats)

##TEST TERM FREQUENCIES#########
r_stats <- list("ciao che bello il mare ciao", "matteo è bello", "matteo dice ciao ciao mondo")
r_stats_text_corpus <- Corpus(VectorSource(r_stats))
inspect(myDtm[30:40,1:50])
## ENDTEST TERM FREQUENCIES#########

########
#GET USER TIMELINE
########
r_stats <- userTimeline("mattemanca", n=150)  #n is The maximum number of tweets to return
r_stats[1:10]

#should get 1500
length(r_stats)
#[1] 1500

#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())

#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))


#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower),mc.cores=1) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation,mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()),mc.cores=1)
#inspect(r_stats_text_corpus[1])
##MATTEO: ADD STEMMING
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removeNumbers)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, stripWhitespace); 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, stemDocument,mc.cores=1)


wordcloud(r_stats_text_corpus, min.freq = 1)

myDtm <- TermDocumentMatrix(r_stats_text_corpus, control = list(minWordLength = 1))
inspect(myDtm[30:40,1:50])
findFreqTerms(myDtm, lowfreq=10)



########
#GET USER INFO
########
me <- getUser("mattemanca")

me$getId() #267808325
getUser(267808325)

followersIds <- me$getFollowerIDs()
length(followers)

followers <- me$getFollowers() ##returns a list
followers[[1]]

trend <- availableTrendLocations()
head(trend)


trend <- getTrends(1)











#alternative steps if you're running into problems 
#r_stats<- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")
#save text
#r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
#r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function

#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
#r_stats_text_corpus <- tm_map(r_stats_text_corpus,
#                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
#                              mc.cores=1
#)
#r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
#r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
#r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
#wordcloud(r_stats_text_corpus)
