####################################
## Facebook Text Mining ############
####################################

# Libraries
library("tm")
library("SnowballC")
library("Rgraphviz")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")

# Read dataframe
df  <- read.csv("group379869335364146.csv", stringsAsFactors = FALSE)

# Define Corpus
corpus  <- VCorpus(VectorSource(df$msgs))

# Remove blank spaces, transforming to Lower, remove punctuation, remove stopwords and stem doc.
corpus  <- tm_map(corpus, content_transformer(stripWhitespace))
corpus  <- tm_map(corpus, content_transformer(tolower))
corpus  <- tm_map(corpus, content_transformer(removePunctuation))
corpus  <- tm_map(corpus, content_transformer(removeNumbers))
# corpus  <- tm_map(corpus, removeWords, stopwords("portuguese"))
corpus  <- tm_map(corpus, stemDocument, "portuguese", lazy = TRUE)


# Inspect Corpus
dtm <- TermDocumentMatrix(corpus)

# Find frequent words
findFreqTerms(dtm, lowfreq =25)

# Find more correlated words
findAssocs(dtm, "cigarro", corlimit=0.2)

# plot
plot(dtm, terms = findFreqTerms(dtm, lowfreq= 25), corThreshold=.4)

# Clustering ---- 

dtm2 <- removeSparseTerms(dtm, sparse=0.90)

# convert the sparse term-document matrix to a standard data frame
mydata.df <- as.data.frame(inspect(dtm2))

# Scale
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit, main="") # display dendogram?

groups <- cutree(fit, k=10) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=10, border="red")

# Create wordcloud
wordcloud(corpus, random.order = F, min.freq = 20, colors = brewer.pal(5, "Dark2"))

