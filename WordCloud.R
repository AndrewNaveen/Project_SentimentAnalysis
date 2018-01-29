#Libraries required for text-preprocessing and wordcloud
install.packages("SnowballC") 
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("rvest")
library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#Scraping information from Amazon
#Initially scrapped 71 pages, therefore a total of 710 reviews

url<-"https://www.amazon.com/Harry-Potter-Collectible-Trunk-Like-Decorative/product-reviews/B018IL7AQY/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=avp_only_reviews&pageNumber="
N_pages <- 150
A <- NULL
for (j in 1: N_pages){
  pant <- read_html(paste0(url, j)) 
  B <- cbind(pant %>% html_nodes(".review-text") %>%     html_text()     )
  A <- rbind(A,B)
}

#Now i scrape another 60 pages making the corpus of 1300 reviews.

url<-"https://www.amazon.com/Harry-Potter-Collectible-Trunk-Like-Decorative/product-reviews/B018IL7AQY/ref=cm_cr_getr_d_paging_btm_4?ie=UTF8&reviewerType=avp_only_reviews&pageNumber="
N_pages <- 130
C <- NULL
for (j in 101: N_pages){
  pant <- read_html(paste0(url, j)) 
  B <- cbind(pant %>% html_nodes(".review-text") %>%     html_text()     )
  C <- rbind(C,B)
}
A<-rbind(A,C)


#Changing the column name of the data matrix

reviews<-A
reviews<-as.data.frame(reviews)
colnames(A)<-c("Reviews")
colnames(reviews)<-c("Reviews")

#Calculate wordcount for reviews

reviews$total <- sapply(reviews$Reviews, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
avg<-mean(reviews$total)

#as the avg value is 66
reviews<-as.data.frame(subset(reviews$Reviews, reviews$total >50) )

#Creating Corpus

corpus<-Corpus(VectorSource(reviews$Reviews))
inspect(corpus)

corpus<-tm_map(corpus, function(x) iconv(x, to='UTF', sub='byte'))#use if u execute in mac
write.csv(reviews,file="Amazon_product.csv")
corpus<- tm_map(corpus, content_transformer(tolower))
inspect(corpus)
corpus<-tm_map(corpus, removeWords, c("harry", "potter", "rowling"))
inspect(corpus)
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus)
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus)
corpus<- tm_map(corpus, removeNumbers)
inspect(corpus)
corpus<- tm_map(corpus, stemDocument)
inspect(corpus)

dtm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))
m <- as.matrix(dtm)

v <- sort(rowSums(m),decreasing=TRUE)#sort the matrix acc to the sum of total row of each term
v
df <- data.frame(word = names(v),freq=v)  #To create two cols, one with the word and other with the frequency count 

head(df, 10) 

wordcloud(words = df$word, freq = df$freq, min.freq = 20, max.words=1000, 
          scale=c(10,0.1), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
