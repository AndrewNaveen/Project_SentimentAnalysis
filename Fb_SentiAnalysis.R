library(Rfacebook)
library(httpuv)
library(dplyr)
library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(plyr)
library(ggplot2)
library(topicmodels)
library(tm)
library(ggplot2)
library(caret)
library(lattice)
library(quanteda)
library(tidyr)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ldatuning)
library(stringr)
#getwd()
#setwd("/Users/andrewnaveenkumarsekar/Documents/Data_Sets/R_Datasets")
#Facebook Authentication
fb_oauth <- fbOAuth(app_id="2040880412808652", app_secret="880418bd301c0ac01abfbca2d4395dd6", extended_permissions = TRUE)



#GetPage- Extract list of posts from a public Facebook page
page<-getPage(page="spotifyusa", token=fb_oauth, n = 150, since="2017/11/30", until="2017/12/2", feed = FALSE, 
              reactions = TRUE,  verbose = TRUE, api = NULL)
nrow(page)
colnames(page)
View(page)
write.csv(page, file="ListPosts_Pandora.csv")


post<-getPost(post = page$id[28], n=1000,token=fb_oauth)
#View(post)
post[3]
post.df<-as.data.frame(post[3])
View(post.df)
#write.csv(post.df, file="topComments2.csv")



#getPost-Extract information about a public Facebook post - Returns a list
post<-getPost(post = '244929708853349_1741464789199826', n=2000,token=fb_oauth)
#post[3]
post.df<-as.data.frame(post[3])
View(post.df)
write.csv(post.df, file="spotifyComments.csv")

#aa <- read.csv("topComments1.csv")
#View(aa)
#topcomments.df <- as.data.frame(aa)
#View(topcomments.df)

post1.df <- sapply(post.df, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
post1.df<-as.data.frame(post1.df)

#Question1
#Example of extracting comments from all the posts returned by getPage function

#wordcloud
#comments<-read.csv("topComments1.csv")
gsub("[^\x01-\x7F]", "", post.df)
corpus<-Corpus(VectorSource(post1.df$comments.message))
corpus<-tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', 'ASCII', sub='byte'))
corpus<- tm_map(corpus, content_transformer(tolower))

corpus<-tm_map(corpus, removeWords, c("spotifi","efbfbdefbfbd","efbfbdefbfbd","efbfbdefbfbd","efbfbdefbfbd","efbfbdefbfbdefbfbdefbfbdefbfbdefbfbd","efbfbdefbfbdefbfbdefbfbd","efbfbdefbfbdefbfbdefbfbdefbfbdefbfbd","just","efbfbdefbfbdefbfbdefbfbd","efbfbdefbfbdefbfbdefbfbd","efbfbdefbfbd"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, stemDocument)

dtm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))
m <- as.matrix(dtm)

v <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)

head(df, 10) 

wordcloud(words = df$word, freq = df$freq, min.freq = 20, max.words=1000, scale=c(4,0.1), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

View(df)



#Sentiment Analysis
pos<-readLines("positive_words.txt")
neg<-readLines("negative_words.txt")

score.sentiment<-function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores<-laply(sentences,
                function(sentence, pos.words, neg.words)
                {
                  # remove punctuation
                  sentence<-gsub("[[:punct:]]", "", sentence)
                  # remove control characters
                  sentence<-gsub("[[:cntrl:]]", "", sentence)
                  # remove digits?
                  sentence<-gsub('\\d+', '', sentence)
                  
                  #convert to lower
                  sentence<-tolower(sentence)
                  
                  
                  # split sentence into words with str_split (stringr package)
                  word.list<- str_split(sentence, "\\s+")
                  words<- unlist(word.list)
                  
                  # compare words to the dictionaries of positive & negative terms
                  pos.matches<-match(words, pos)
                  neg.matches<- match(words, neg)
                  
                  # get the position of the matched term or NA
                  # we just want a TRUE/FALSE
                  pos.matches<- !is.na(pos.matches)
                  neg.matches<- !is.na(neg.matches)
                  
                  # final score
                  score<- sum(pos.matches) - sum(neg.matches)
                  return(score)
                }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df<- data.frame(text=sentences, score=scores)
  return(scores.df)
}
View(post1.df)
#scores_facebook<-score.sentiment(comments$comments.message, pos, neg, .progress='text')
scores_facebook<-score.sentiment(post1.df$comments.message, pos, neg, .progress='text')
scores_facebook
View(scores_facebook)
summary(scores_facebook$score)

#Converting sentiment scores from numeric to character to enable the gsub function 
scores_facebook$score_character<-as.character(scores_facebook$score)

#After looking at the scores decide on a threshold for the sentiment labels
scores_facebook$score_character<-gsub("^0$", "Neutral", scores_facebook$score_character)
scores_facebook$score_character<-gsub("^1$|^2$|^3$|^4$|^5$", "positive", scores_facebook$score_character)
scores_facebook$score_character<-gsub("^6$|^7$|^8$|^9$|^10$", "very positive", scores_facebook$score_character)
scores_facebook$score_character<-gsub("^-1$|^-2$|^-3$|^-4$|^-5$", "negative", scores_facebook$score_character)
scores_facebook$score_character<-gsub("^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$|^-13$|^-14$|^-15$", "very negative", scores_facebook$score_character)


#Convert score_character to factor for visualizations
scores_facebook$score_character<-as.factor(scores_facebook$score_character)
View(scores_facebook)

#Create a bar chart that shows distributions of sentiment labels
ggplot(scores_facebook, aes(x=score_character, fill = score))+geom_bar()+theme_bw()+labs(y = "Count",
                                                                                         title = "Score_Character")

comments.df<-cbind(post1.df,scores_facebook)
View(comments.df)

#my_source<-VectorSource(comments.df$comments.message)
#corpus<-Corpus(my_source)
#corpus<-tm_map(corpus, removeWords, c(""))

corpus<-Corpus(VectorSource(comments.df$comments.message))
corpus<-tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', 'ASCII', sub='byte'))
corpus<- tm_map(corpus, content_transformer(tolower))
corpus<-tm_map(corpus, removeWords, c("efbfbdefbfbd", "efbfbdefbfbd", "efbfbdefbfbdefbfbdefbfbd"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus)
#Creating structure data from unstructures using a document term matraix
dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Remove empty cells and create a new corpus that aligns with the processed dtm
rowTotals <- apply(dtm , 1, sum)  #1=row; 2=column. Therefore, we apply the function(sum) to the row here.
empty.rows<-dtm[rowTotals == 0,]$dimnames[1][[1]] #name of the rows whose sum is 0   #ID of all the rows whose row sum is 0
empty.rows<-as.numeric(empty.rows)  #converting from character to numeric
corpus <- corpus[-empty.rows] #Deleting  those rows
inspect(corpus)

#Create a dataframe of the new corpus
corpus.df<-as.data.frame(corpus$content)

#Create the dtm again with the new corpus
dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Making sure that the original data set i.e., tweets.df aligns with the new corpus. To do that we have to remove the same row numbers in empty.rows in tweets.df
x<-length(as.numeric(empty.rows))# calculate the number of empty.rows

#Write a loop that goes through the row numbers of tweets.df and delete those rows that match with delete.rows
empty.rows[1]
for (i in 1:x){
  comments.df<-comments.df[-empty.rows[i],]
  i<-i+1
}

#Random check to see consistency between the new and old datasets
corpus.df[50,]
comments.df[50,]$text

#Run the LDA topic model
lda<-LDA(dtm, k=4, control = list(seed=2343))

#Using tidytext manifest topics. Tidy() function is a kind of an output function for lda.
topics<-tidy(lda, matrix="beta")
topics

#Showing the top terms and grouping them by topics created - Using dplyr's top_n limiting to 10. 
#"%>%" is a piping operator. i.e. it transfers the output of LHS to RHS.
top_terms<-topics %>%
  group_by(topic) %>%   
  top_n(5, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#Convert top_terms$topic to a factor variable - for visualization 
top_terms$topic<-as.factor(top_terms$topic)

#Visualization of the top terms.
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = topic)) + #fill=topic means that we are differentiating topics using different colors
  theme_bw()+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#Creating a per-document-per-topic probability list
documents<-tidy(lda, matrix="gamma")
documents
documents$document<- as.numeric(documents$document)
documents<-arrange(documents,document)# arranging documents acc to the document no.

#Finding the highest probablity gamma score to assign topic number for each document
i<-1
max_pos<-vector() #creating empty vector
for(i in documents$document){
  
  x<-filter(documents, document==i)
  max<-max(x$gamma) 
  max_pos[length(max_pos)+1]<-max
  #mutate(documents, max)
}

#Merging the per document topic probabilities to tweets.df
documents$max<-max_pos
documents$diff<-documents$gamma-documents$max
documents$corpus.df<-corpus.df$Content

#creating a document with only max gamma values
documents<-filter(documents, diff==0)
documents$document<-as.numeric(documents$document)
documents<-arrange(documents,document)

comments.df<-cbind(comments.df,documents)

#Export results 

class(comments.df$topic)

comments.df$topic<-as.factor(comments.df$topic)
ggplot(comments.df,aes(topic))+
  geom_bar()
#Finding out how many topics to create
result<-FindTopicsNumber(dtm,
                         topics=seq(from = 2, to =15, by=1),
                         metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                         method = "Gibbs",
                         control = list(seed = 77),
                         mc.cores = 3L,
                         verbose = TRUE)

FindTopicsNumber_plot(result)

#A bar chart that shows distribution of topic numbers segmented by sentiment labels
ggplot(comments.df,aes(x=topic,fill=score_character))+
  theme_bw() +
  geom_bar() +
  labs(y = " Count",
       title = "Distribution of topics by sentiment label")