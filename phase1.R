#install.packages("ggplot2")
#install.packages("fpc")

library("twitteR")
library("wordcloud")
library("tm")
library(ggplot2)
library(wordcloud)
library(fpc)

consumer_key <- 'h226mYlUmcprSObE9MJtEDPPx'
consumer_secret <- 'e6KW7kIt1SHVoDePo5aYZ1Mk3dkL73nps2inSekgVDQqEX5usl'
access_token <- '701039102456524801-17X4X8kSVeqeWou7rDuNFCuWKRJLiBR'
access_secret <- 'zLh4IaF3Vf6Z5RGUbIsljbKQKkWbehJp4AvEIV3FNMvCe'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# Retrieving Text
rdmTweets <- userTimeline("rdatamining", n=200)
(nDocs <- length(rdmTweets))
rdmTweets[11:15]

# Transforming Text
df <- twListToDF(rdmTweets)
dim(df)
myCorpus <- Corpus(VectorSource(df$text))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myStopwords <- c(stopwords('english'), "available", "via")
myStopwords <- setdiff(myStopwords, c("r", "big"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, stripWhitespace)
# inspect(myCorpus[11])

# Stemming Words
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
for (i in 11:15) {
        cat(paste("[[", i, "]] ", sep=""))
        writeLines(strwrap(myCorpus[[i]], width=73))
        }
for (i in 11:15) {
        cat(paste0("[", i, "] "))
        writeLines(strwrap(as.character(myCorpus[[i]]), 60))
       }
stemCompletion2 <- function(x, dictionary) {
        x <- unlist(strsplit(as.character(x), " "))
       
              x <- x[x != ""]
              x <- stemCompletion(x, dictionary=dictionary)
              x <- paste(x, sep="", collapse=" ")
              PlainTextDocument(stripWhitespace(x))
              }
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))
inspect(myCorpus[11:15])
miningCases <- lapply(myCorpusCopy,
                      function(x) { grep(as.character(x), pattern = "\\<mining")} )
sum(unlist(miningCases))
minerCases <- lapply(myCorpusCopy,
                     function(x) {grep(as.character(x), pattern = "\\<miner")} )
sum(unlist(minerCases))
myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                   pattern = "miners", replacement = "mining")
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
tdm
idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx+(0:5),101:110])

tdm <- TermDocumentMatrix(myCorpus, control=list(minWordLength=1))
findFreqTerms(tdm, lowfreq=10)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
        xlab("Terms") + ylab("Count") + coord_flip()
barplot(termFrequency, las=2)

# Wordcloud
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
set.seed(375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,
                        colors=pal)
tdm2 <- removeSparseTerms(tdm, sparse=0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
rect.hclust(fit, k=10)
plot(fit)
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))
m3 <- t(m2)
set.seed(122)
k <- 8
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits=3)
for (i in 1:k) {
        cat(paste("cluster ", i, ": ", sep=""))
        s <- sort(kmeansResult$centers[i,], decreasing=T)
        cat(names(s)[1:3], "\n")
}
pamResult <- pamk(m3, metric="manhattan")
(k <- pamResult$nc)
pamResult <- pamResult$pamobject
 for (i in 1:k) {
        cat(paste("cluster", i, ": "))
        cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
        
 }
layout(matrix(c(1,2),2,1))
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
              col.p=pamResult$clustering)