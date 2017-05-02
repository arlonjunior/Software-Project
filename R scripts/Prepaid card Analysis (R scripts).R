#Creating a sentimrnt function

library (plyr) #Tools for Splitting, Applying and Combining Data
library (stringr) #Make it easier working with string

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{  
  require(plyr)  
  require(stringr)       
  
  # we got a vector of sentences. 
  #plyr will handle a list or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    
    sentence = gsub('[[:punct:]]', '', sentence) #Remove punctuation
    
    sentence = gsub('[[:cntrl:]]', '', sentence) #Remove Control characters
    
    sentence = gsub('\\d+', '', sentence)  #Remove digits
    
    # and convert to lower case:  
    
    sentence = tolower(sentence)  
    
    # split into words. str_split is in the stringr package  
    
    word.list = str_split(sentence, '\\s+')  
    
    # sometimes a list() is one level of hierarchy too much  
    
    words = unlist(word.list)  
    
    # compare our words to the dictionaries of positive & negative terms  
    
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    
    # match() returns the position of the matched term or NA  
    # we just want a TRUE/FALSE:  
    
    pos.matches = !is.na(pos.matches)  
    
    neg.matches = !is.na(neg.matches)  
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
    
    score = sum(pos.matches) - sum(neg.matches)  
    
    return(score)  
    
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 

# Scoring Customer feedbacks (complaints) & Adding a column      

#Load sentiment word lists
hu.liu.pos = scan('C:/Users/molsj/Documents/4th Year/Semester 2/Software Project II/R Codes/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C:/Users/molsj/Documents/4th Year/Semester 2/Software Project II/R Codes/negative-words.txt', what='character', comment.char=';')

#Add words to list
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'fuck', 'shit', 'wait','waiting', 'epicfail', 'mechanical')


#Import csv file (Dataset)
DatasetCustomers_Narratives <- read.csv("C:/Users/molsj/Documents/4th Year/Semester 2/Software Project II/R Codes/Consumer_Complaint_Narratives.csv")
DatasetCustomers_Narratives$Consumer.complaint.narrative<-as.factor(DatasetCustomers_Narratives$Consumer.complaint.narrative)


#Score all complaints
Consumer_Complaint_Narratives.scores = score.sentiment(DatasetCustomers_Narratives$Consumer.complaint.narrative, pos.words,neg.words, .progress='Consumer.complaint.narrative')



path<-"C:/Users/molsj/Documents/4th Year/Semester 2/Software Project II/R Codes"
write.csv(Consumer_Complaint_Narratives.scores,file=paste(path,"Consumer_Complaint_Narratives_Scores.csv",sep=""),row.names=TRUE)


Consumer_Complaint_Narratives.scores$Feedback = 'Consumer_Complaint_Narratives'

# Visualizing  
library(ggplot2)
library(QPot)

#The positive values stand for positive feedback 
#and the negative values for negative feedback. 
hist(Consumer_Complaint_Narratives.scores$score)
qplot(Consumer_Complaint_Narratives.scores$score)

#Mean of the score distribution
mean(Consumer_Complaint_Narratives.scores$score)

#Range of the score distribution
range(Consumer_Complaint_Narratives.scores$score)

#Median of the score distribution
median(Consumer_Complaint_Narratives.scores$score)

#Frequency of each score
count(Consumer_Complaint_Narratives.scores$score)

#Histogram with ggplot2               
all.scores = rbind(Consumer_Complaint_Narratives.scores)
ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_histogram(mapping=aes(x=score, fill=Feedback), binwidth=0.5) +
  facet_grid(Feedback~.) + # make a separate plot for each hashtag
  theme_bw(base_size = 12) + scale_fill_brewer(palette = 18) # plain display, nicer colors

# Classification

#Get the text
Consumer_Complaint_Narratives_txt = DatasetCustomers_Narratives$Consumer.complaint.narrative

#Data cleansing to prepare text for the sentiment analysis
Consumer_Complaint_Narratives_txt = gsub("[[:punct:]]", "", Consumer_Complaint_Narratives_txt) #remove punctuation 

Consumer_Complaint_Narratives_txt = gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", Consumer_Complaint_Narratives_txt) #remove retweet entities 

Consumer_Complaint_Narratives_txt = gsub("@\\w+", "", Consumer_Complaint_Narratives_txt) #remove @

Consumer_Complaint_Narratives_txt = gsub("[[:digit:]]", "", Consumer_Complaint_Narratives_txt)  #remove digits

Consumer_Complaint_Narratives_txt = gsub("^\\s+|\\s+$", "", Consumer_Complaint_Narratives_txt)  #remove tabs

Consumer_Complaint_Narratives_txt = gsub("[ \t]{2,}", "", Consumer_Complaint_Narratives_txt) #remove spaces

Consumer_Complaint_Narratives_txt = gsub("http\\w+", "", Consumer_Complaint_Narratives_txt) #remove link

Consumer_Complaint_Narratives_txt = gsub("xx", "", Consumer_Complaint_Narratives_txt) #remove xx

Consumer_Complaint_Narratives_txt = gsub("xxx", "", Consumer_Complaint_Narratives_txt) #remove xxx

Consumer_Complaint_Narratives_txt = gsub("xxxx", "", Consumer_Complaint_Narratives_txt) #remove xxxx

Consumer_Complaint_Narratives_txt = gsub("xxxxxxxx", "", Consumer_Complaint_Narratives_txt) #remove xxxxxxxx

#Defining a function which can handle "tolower error handling", 
#in case arises any while converting all the words into lower case.
try.error = function(x)
{
  #create missing value
  y = NA
  
  #trycatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  
  #if not an error
  if(!inherits(try_error, "error"))
    y = tolower(x)
  
  #result
  return(y)
}

#Transforming all the words into lower case using the 
#try.error function created above with the sapply function
Consumer_Complaint_Narratives_txt = sapply(Consumer_Complaint_Narratives_txt, try.error)

#Remove NAs, if any exists, from Consumer_Complaint_Narratives_txt
Consumer_Complaint_Narratives_txt = Consumer_Complaint_Narratives_txt[!is.na(Consumer_Complaint_Narratives_txt)]

#Also remove names (column headings) from the text, 
#as we do not want them in the sentiment analysis
names(Consumer_Complaint_Narratives_txt) = NULL

#intsalling sentiment 0.2
install.packages("C:/Users/molsj/Documents/4th Year/Semester 2/Software Project II/R Codes/sentiment_0.2.tar.gz", repos = NULL, type="source")

library(RColorBrewer)
library(wordcloud)
library(NLP)
library(tm)
library(Rstem)
library(sentiment)


#We are using Bayes' algorithm to: 

#classify emotion 
#This function returns an object of class data.frame with seven columns 
#(anger, disgust, fear, joy, sadness, surprise, best_fit) and one row for each document:
class_emo = classify_emotion(Consumer_Complaint_Narratives_txt, algorithm="bayes", prior=1.0)
#get emotion best fit
emotion = class_emo[,7]
# Replacing NA's (if any, found while processing classification) 
#by the word "unknown"
emotion[is.na(emotion)] = "unknown"


#classify polarity
#This process will classify the text data into four categories
#(pos, neg, pos/neg, best_fit)
class_pol = classify_polarity(Consumer_Complaint_Narratives_txt, algorithm = "bayes")
#get polarity best fit
polarity = class_pol[,4]

#Creating data frame and rearrange data for plotting purposes

#Creating data frame
sent_categ = data.frame(text=Consumer_Complaint_Narratives_txt,
                        polarity=polarity, stringsAsFactors=FALSE)

#sort data frame
sent_categ = within(sent_categ,
                    emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


#Results in numbers

#Frequency of eah observation of polarity
count(sent_categ, "polarity")
str(count(sent_categ, "polarity"))

#Frequency of eah observation of emotion
count(sent_categ, "emotion")
str(count(sent_categ, "emotion"))


#Visualization

#Plot distribution of polarity
ggplot(sent_categ, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="Paired") +
  labs(x="polarity categories", y="Number of complaints",
       title = "Sentiment Analysis of customers' complaints (classification by polarity)",
       plot.title = element_text(size=12))

#Plot distribution of emotions
ggplot(sent_categ, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Paired") +
  labs(x="emotion categories", y="Number of complaints",
       title = "Sentiment Analysis of Financial Complaints\n(classification by emotion)",
       plot.title = element_text(size=12))

#Pie Chart with size polarity
ggplot(data = sent_categ) + 
  geom_bar(mapping = aes(x = polarity, fill = polarity), width = 1) + 
  scale_fill_brewer(palette="Paired") +
  theme_linedraw() +
  coord_polar()

#Pie Chart with size emotion
ggplot(data = sent_categ) + 
  geom_bar(mapping = aes(x = emotion, fill = emotion), width = 1) + 
  scale_fill_brewer(palette="Paired") +
  theme_linedraw() +
  coord_polar()

#Pie Chart Same Size polarity
ggplot(data = sent_categ) + 
  geom_bar(mapping = aes(x = factor(1), fill = polarity), width = 1) +
  scale_fill_brewer(palette="Paired") +
  theme_void() +
  coord_polar(theta = "y")

#Pie Chart Same Size emotion
ggplot(data = sent_categ) + 
  geom_bar(mapping = aes(x = factor(1), fill = emotion), width = 1) +
  scale_fill_brewer(palette="Paired") +
  theme_void() +
  coord_polar(theta = "y")

#Word Cloud
library(tm)
library(wordcloud)

#Word cloud of the full dataset (non-ajusted)
wordcloud(Consumer_Complaint_Narratives_txt)

#Word cloud ajusted 
wordcloud(Consumer_Complaint_Narratives_txt, scale=c(3,0.6), max.words=1000, 
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))


#Comparing word cloud 

#Compare words by emotion
#separating text by emotion
emos = levels(factor(sent_categ$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = Consumer_Complaint_Narratives_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse="")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(
  corpus,
  control = list(
    wordLengths=c(0,Inf),
    removePunctuation = TRUE,
    stopwords = c("prayformh370", "prayformh", stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE) )
tdm = as.matrix(tdm)
colnames(tdm) = emos

library(RColorBrewer)

# comparison word cloud by emotion
comparison.cloud(tdm, max.words=1000,colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

#Compare words by polarity
#separating text by polarity
emos = levels(factor(sent_categ$polarity))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = Consumer_Complaint_Narratives_txt[polarity == emos[i]]
  emo.docs[i] = paste(tmp, collapse="")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(
  corpus,
  control = list(
    wordLengths=c(0,Inf),
    removePunctuation = TRUE,
    stopwords = c("prayformh370", "prayformh", stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE) )
tdm = as.matrix(tdm)
colnames(tdm) = emos

library(RColorBrewer)

# comparison word cloud by polarity
comparison.cloud(tdm, max.words=3000, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
