# Farees Patel
# IST687 - Assignment 11: Text Mining

# Loading positive and negative words
positive <- "D:/Syracuse/MIS/Fall '16/687/Homework/PositiveWords.txt"
negative <- "D:/Syracuse/MIS/Fall '16/687/Homework/NegativeWords.txt"

# Reading the file
positiveList <- scan(positive, character(0), sep = "\n")
negativeList <- scan(negative, character(0), sep = "\n")

# Cleaning data
positiveList <- positiveList[-1:-34]
negativeList <- negativeList[-1:-34]

# Reading mlk file
mlk <- read.delim(file="D:/Syracuse/MIS/Fall '16/687/Lab/MLKSpeech.txt", header=FALSE, stringsAsFactors=FALSE)

# Loading packages
install.packages("tm")
library(tm)

# Function to extract wordcount
extractWordCount <- function(vectorList)
{
  # Treating each element of the input vector as a document
  words.vec <- VectorSource(vectorList)
  
  # Creating corpus of words
  words.corpus <- Corpus(words.vec)
  
  # Converting to lower case
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  
  # Removing punctuation
  words.corpus <- tm_map(words.corpus, removePunctuation)
  
  # Removing numbers
  words.corpus <- tm_map(words.corpus, removeNumbers)
  
  # Considering only english words by removing stopwords
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  
  # Stemming words in the corpus
  words.corpus <- tm_map(words.corpus, stemDocument)
  
  # Cleaning whitespace caused due to transformation
  words.corpus <- tm_map(words.corpus, stripWhitespace)
  
  # Conducting statistical analysis
  tdm <- TermDocumentMatrix(words.corpus)
  
  # Creating matrix
  matrixTdm <- as.matrix(tdm)
  
  # Getting word count
  wordCount <- rowSums(matrixTdm)
  
  return(wordCount)
}

# Obtaining word count
wordCounts <- extractWordCount(mlk)
wordCounts

# Calculating total number of words
totalWordCount <- sum(wordCounts)
totalWordCount

# Vector having all words
words <- names(wordCounts)
words

# Obtaining list of matching positive words
matchedPositive <- match(words, positiveList, nomatch = 0)
matchedPositive

# Count of matching positive words
countMatchedPositive <- wordCounts[which(matchedPositive != 0)]
countMatchedPositive
length(countMatchedPositive)

# Obtaining list of matching negative words
matchedNegative <- match(words, negativeList, nomatch = 0)
matchedNegative

# Count of matching negative words
countMatchedNegative <- wordCounts[which(matchedNegative != 0)]
countMatchedNegative
length(countMatchedNegative)

# Creating data frame
wordCountsDf <- data.frame(wordCounts)

# Creating a matrix to store the word, wordcount, wordname and removing rownames
wordCountsDf <- cbind(Word=rownames(wordCountsDf),wordCountsDf)
rownames(wordCountsDf)<-c()

# Converting column types
wordCountsDf$Word <- as.character(wordCountsDf$Word)
wordCountsDf$wordCounts<-as.integer(wordCountsDf$wordCounts)

# Breaking MLK speech into 4 equal parts
part1 <- wordCountsDf[1:(nrow(wordCountsDf)/4),]
part2 <- wordCountsDf[((nrow(wordCountsDf)/4)+1):(nrow(wordCountsDf)/2),]
part3 <- wordCountsDf[((nrow(wordCountsDf)/2)+1):((nrow(wordCountsDf)/4)*3),]
part4 <- wordCountsDf[(((nrow(wordCountsDf)/4)*3)+1):nrow(wordCountsDf),]

# Part 1
# Obtaining word count
wordCountsPart1 <- extractWordCount(part1)
wordCountsPart1

# Calculating total number of words
totalWordCountPart1 <- sum(wordCountsPart1)
totalWordCountPart1

# Vector having all words
wordsPart1 <- names(wordCountsPart1)
wordsPart1

# Obtaining list of matching positive words
matchedPositivePart1 <- match(wordsPart1, positiveList, nomatch = 0)
matchedPositivePart1

# Count of matching positive words
countMatchedPositivePart1 <- wordCountsPart1[which(matchedPositivePart1 != 0)]
countMatchedPositivePart1
length(countMatchedPositivePart1)

# Obtaining list of matching negative words
matchedNegativePart1 <- match(wordsPart1, negativeList, nomatch = 0)
matchedNegativePart1

# Count of matching negative words
countMatchedNegativePart1 <- wordCountsPart1[which(matchedNegativePart1 != 0)]
countMatchedNegativePart1
length(countMatchedNegativePart1)

# Part 2
# Obtaining word count
wordCountsPart2 <- extractWordCount(part2)
wordCountsPart2

# Calculating total number of words
totalWordCountPart2 <- sum(wordCountsPart2)
totalWordCountPart2

# Vector having all words
wordsPart2 <- names(wordCountsPart2)
wordsPart2

# Obtaining list of matching positive words
matchedPositivePart2 <- match(wordsPart2, positiveList, nomatch = 0)
matchedPositivePart2

# Count of matching positive words
countMatchedPositivePart2 <- wordCountsPart2[which(matchedPositivePart2 != 0)]
countMatchedPositivePart2
length(countMatchedPositivePart2)

# Obtaining list of matching negative words
matchedNegativePart2 <- match(wordsPart2, negativeList, nomatch = 0)
matchedNegativePart2

# Count of matching negative words
countMatchedNegativePart2 <- wordCountsPart2[which(matchedNegativePart2 != 0)]
countMatchedNegativePart2
length(countMatchedNegativePart2)

# Part 3
# Obtaining word count
wordCountsPart3 <- extractWordCount(part3)
wordCountsPart3

# Calculating total number of words
totalWordCountPart3 <- sum(wordCountsPart3)
totalWordCountPart3

# Vector having all words
wordsPart3 <- names(wordCountsPart3)
wordsPart3

# Obtaining list of matching positive words
matchedPositivePart3 <- match(wordsPart3, positiveList, nomatch = 0)
matchedPositivePart3

# Count of matching positive words
countMatchedPositivePart3 <- wordCountsPart3[which(matchedPositivePart3 != 0)]
countMatchedPositivePart3
length(countMatchedPositivePart3)

# Obtaining list of matching negative words
matchedNegativePart3 <- match(wordsPart3, negativeList, nomatch = 0)
matchedNegativePart3

# Count of matching negative words
countMatchedNegativePart3 <- wordCountsPart3[which(matchedNegativePart3 != 0)]
countMatchedNegativePart3
length(countMatchedNegativePart3)

# Part 4
# Obtaining word count
wordCountsPart4 <- extractWordCount(part4)
wordCountsPart4

# Calculating total number of words
totalWordCountPart4 <- sum(wordCountsPart4)
totalWordCountPart4

# Vector having all words
wordsPart4 <- names(wordCountsPart4)
wordsPart4

# Obtaining list of matching positive words
matchedPositivePart4 <- match(wordsPart4, positiveList, nomatch = 0)
matchedPositivePart4

# Count of matching positive words
countMatchedPositivePart4 <- wordCountsPart4[which(matchedPositivePart4 != 0)]
countMatchedPositivePart4
length(countMatchedPositivePart4)

# Obtaining list of matching negative words
matchedNegativePart4 <- match(wordsPart4, negativeList, nomatch = 0)
matchedNegativePart4

# Count of matching negative words
countMatchedNegativePart4 <- wordCountsPart4[which(matchedNegativePart4 != 0)]
countMatchedNegativePart4
length(countMatchedNegativePart4)

# Plotting graph
vectorPositive <- c(length(countMatchedPositivePart1),length(countMatchedPositivePart2),length(countMatchedPositivePart3),length(countMatchedPositivePart4))
barplot(vectorPositive,main = "Barplot for quarter wise positive match", xlab = "Quarter", ylab = "Positive Match", names.arg = c("Quarter 1","Quarter 2","Quarter 3","Quarter 4"))

vectorNegative <- c(length(countMatchedNegativePart1),length(countMatchedNegativePart2),length(countMatchedNegativePart3),length(countMatchedNegativePart4))
barplot(vectorNegative,main = "Barplot for quarter wise negative match", xlab = "Quarter", ylab = "Negative Match", names.arg = c("Quarter 1","Quarter 2","Quarter 3","Quarter 4"))
