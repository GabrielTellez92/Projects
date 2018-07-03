#Airbnb Austin
# Word clouds and bar graphs

#libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(xtable)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

#############################################################
#LOCATION_DESCRIPTION_NAME
#read csv file
name1 = read.csv(file = "C:/Users/Jonathan/Documents/Visualization/Final Project AirBnB/location_description_name.csv", stringsAsFactors = FALSE)

#create corpus
nameCorpus = Corpus(VectorSource(name1))
#convert to lowercase, remove puncs, convert to plain text, remove stopwords
nameCorpus = tm_map(nameCorpus, content_transformer(tolower))
nameCorpus = tm_map(nameCorpus, removePunctuation)
nameCorpus = tm_map(nameCorpus, PlainTextDocument)
nameCorpus = tm_map(nameCorpus, removeWords, stopwords('english'))
#nameCorpus = tm_map(nameCorpus, removeWords, "austin")
nameCorpus = tm_map(nameCorpus, removeNumbers)
#stem words
#nameCorpus = tm_map(nameCorpus, stemDocument)

#create termdocumentmatrix
tdm = TermDocumentMatrix(nameCorpus)
#get frequency of top words to plot in bar graph
freq = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df = data.frame(word = names(freq), freq = freq)
head(freq.df, 10)

#create wordcloud
wordcloud(nameCorpus, max.words = 200, random.order = FALSE, colors = brewer.pal(4, "Dark2"))

#plot showing top 10
ggplot(head(freq.df, 10), aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.5, color = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, family = "Times New Roman")) +
  geom_text(aes(label = freq), vjust = 1.5, color = "black", family = "Times New Roman") +
  labs(x = "Words", y = "Frequency", title = "Top 10 Most Frequent Words") +
  scale_y_continuous(labels = scales::comma)

##################################################################
#HOST_DESCRIPTION_HOST_ABOUT
#read csv file
host_about = read.csv(file = "C:/Users/Jonathan/Documents/Visualization/Final Project AirBnB/host_description_host_about.csv", stringsAsFactors = FALSE)

#create corpus
aboutCorpus = Corpus(VectorSource(host_about))

#convert to lowercase, remove puncs, convert to plain text, remove stopwords
aboutCorpus = tm_map(aboutCorpus, content_transformer(tolower))
aboutCorpus = tm_map(aboutCorpus, removePunctuation)
aboutCorpus = tm_map(aboutCorpus, PlainTextDocument)
aboutCorpus = tm_map(aboutCorpus, removeWords, stopwords('english'))
#stem words
#aboutCorpus = tm_map(aboutCorpus, stemDocument)

#create termdocumentmatrix
tdm = TermDocumentMatrix(aboutCorpus)
#get frequency of top words to plot in bar graph
freq = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df = data.frame(word = names(freq), freq = freq)
head(freq.df, 10)

#create wordcloud
wordcloud(aboutCorpus, max.words = 150, random.order = FALSE, colors = brewer.pal(4, "Dark2"))

#without image
ggplot(head(freq.df, 10), aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.5, color = "black") +
  labs(x = "Words", y = "Frequency", title = "Top 10 Most Frequent Words") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, face = "bold", family = "Times New Roman")) +
  geom_text(aes(label = freq), vjust = 1.5, color = "black", family = "Times New Roman") +
  scale_y_continuous(labels = scales::comma)

#################################################################
#LOCATION_DESCRIPTION_DESCRIPTION
descrip = read.csv(file = "C:/Users/Jonathan/Documents/Visualization/Final Project AirBnB/location_description_description.csv", stringsAsFactors = FALSE)

#create corpus
descripCorpus = Corpus(VectorSource(descrip))

#convert to lowercase, remove puncs, convert to plain text, remove stopwords
descripCorpus = tm_map(descripCorpus, content_transformer(tolower))
descripCorpus = tm_map(descripCorpus, removePunctuation)
descripCorpus = tm_map(descripCorpus, PlainTextDocument)
descripCorpus = tm_map(descripCorpus, removeWords, stopwords('english'))
#stem words
#aboutCorpus = tm_map(aboutCorpus, stemDocument)

#create termdocumentmatrix
tdm = TermDocumentMatrix(descripCorpus)
#get frequency of top words to plot in bar graph
freq = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df = data.frame(word = names(freq), freq = freq)
head(freq.df, 10)

#create wordcloud
wordcloud(descripCorpus, max.words = 500, random.order = FALSE, colors = brewer.pal(4, "Dark2"))

#without image
ggplot(head(freq.df, 10), aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.5, color = "black") +
  labs(x = "Words", y = "Frequency", title = "Top 10 Most Frequent Words") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, face = "bold", family = "Times New Roman")) +
  geom_text(aes(label = freq), vjust = 1.5, color = "black", family = "Times New Roman", size = 3) +
  scale_y_continuous(labels = scales::comma)

###################################################################
#NAME BY ROOM TYPE (HOUSE/APT, PRIVATE, SHARED)
###################################################################
#change file between houseapt, private, and shared
A = read.csv(file = "C:/Users/Jonathan/Documents/Visualization/Final Project AirBnB/location_description_name_shared.csv", stringsAsFactors = FALSE)

#create corpus
fullCorpus = Corpus(VectorSource(A))

#convert to lowercase, remove puncs, convert to plain text, remove stopwords
fullCorpus = tm_map(fullCorpus, content_transformer(tolower))
fullCorpus = tm_map(fullCorpus, removePunctuation)
fullCorpus = tm_map(fullCorpus, PlainTextDocument)
fullCorpus = tm_map(fullCorpus, removeWords, stopwords('english'))
#stem words
#aboutCorpus = tm_map(aboutCorpus, stemDocument)

#create termdocumentmatrix
tdm = TermDocumentMatrix(fullCorpus)
#get frequency of top words to plot in bar graph
freq = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df = data.frame(word = names(freq), freq = freq)
head(freq.df, 10)

#create wordcloud
wordcloud(fullCorpus, max.words = 100, random.order = FALSE, colors = brewer.pal(4, "Dark2"))

#without image
ggplot(head(freq.df, 10), aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.5, color = "black") +
  labs(x = "Words", y = "Frequency", title = "Top 10 Most Frequent Words") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, face = "bold")) +
  geom_text(aes(label = freq), vjust = 1.5, color = "black") +
  scale_y_continuous(labels = scales::comma)

###################################################################
#DESCRIPTION BY ROOM TYPE (HOUSE/APT, PRIVATE, SHARED)
###################################################################
#change file between houseapt, private, and shared
B = read.csv(file = "C:/Users/Jonathan/Documents/Visualization/Final Project AirBnB/location_description_descrip_shared.csv", stringsAsFactors = FALSE)

#create corpus
fullCorpus = Corpus(VectorSource(B))

#convert to lowercase, remove puncs, convert to plain text, remove stopwords
fullCorpus = tm_map(fullCorpus, content_transformer(tolower))
fullCorpus = tm_map(fullCorpus, removePunctuation)
fullCorpus = tm_map(fullCorpus, PlainTextDocument)
fullCorpus = tm_map(fullCorpus, removeWords, stopwords('english'))
#stem words
#aboutCorpus = tm_map(aboutCorpus, stemDocument)

#create termdocumentmatrix
tdm = TermDocumentMatrix(fullCorpus)
#get frequency of top words to plot in bar graph
freq = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df = data.frame(word = names(freq), freq = freq)
head(freq.df, 10)

#create wordcloud
wordcloud(fullCorpus, max.words = 200, random.order = FALSE, colors = brewer.pal(4, "Dark2"))

#without image
ggplot(head(freq.df, 10), aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.5, color = "black") +
  labs(x = "Words", y = "Frequency", title = "Top 10 Most Frequent Words") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, face = "bold")) +
  geom_text(aes(label = freq), vjust = 1.5, color = "black") +
  scale_y_continuous(labels = scales::comma)


###################################################################

