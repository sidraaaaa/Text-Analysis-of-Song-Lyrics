library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(textdata)

songs=read.csv("songdata.csv")
songs$text=as.character(songs$text)
songs$song=as.character(songs$song)
songs$artist=as.character(songs$artist)


#convert into tidy format data frame
songs=as.tibble(songs)
song_text <- songs %>% dplyr::select(lyrics = text, song, artist)

#need to analyze words...un-nest
song_words = songs %>%
  unnest_tokens(word, text)

#reduce dimension and keep informative words by removing stop words like the
data(stop_words)

song_words_no_stop <- song_words %>%
  anti_join(stop_words)  #Returns rows of first table where it can find match in second table

#now we can maybe start analyzing this data

# song frequency by artist in dataset
songs %>% 
  dplyr::count(artist,sort=T) %>% 
  print(n = 20)

# frequent words by Nirvana
song_words_no_stop %>% 
  filter(artist=="Nirvana") %>%
  dplyr::count(word,sort=T) %>% 
  print(n = 20)

# frequency of word by artist
frequency = song_words_no_stop %>%
  filter(artist %in% c("Nirvana","Taylor Swift")) %>%
  dplyr::count(artist, word) %>%
  group_by(artist) %>%
  mutate(proportion = n / sum(n)) %>% 
  dplyr::select(-n) %>%
  spread(artist,proportion) 

# plot freq of nirvana words against swift
frequency[is.na(frequency)]=0
names(frequency)[3]="Swift"
ggplot(frequency,aes(x=Nirvana,y=Swift)) + 
  geom_abline()+
  geom_text(aes(label=word),check_overlap=T)+
  scale_x_log10() +
  scale_y_log10() 

# sentiments of words
song_words_sentiment <- song_words_no_stop %>%
  inner_join(get_sentiments("afinn")) 

# average over artist
song_words_sentiment %>%
  group_by(artist) %>%
  dplyr::summarise(avg_sentiment = mean(value)) %>%
  arrange(desc(avg_sentiment)) %>% 
  print(n=20)

song_words_no_stop %>% dplyr::count(word,artist) %>% group_by(artist) %>% top_n(n=20) %>% 
 arrange(desc(artist)) %>%  print(n=40) 
#HOMEWORK
#Which band/artist says love the most (per song)? The least?
song_per_artist<-songs%>%group_by(artist)%>%summarize(song_count=n()) #counting total songs

love_per_artist=song_words_no_stop %>%
  filter(tolower(word)=="love")%>%
  group_by(artist)%>% 
  summarise(love_count=n())

love_per_song=song_per_artist%>%inner_join(love_per_artist,by="artist")
love_per_song$lps=love_per_song$love_count/love_per_song$song_count
love_per_song%>%arrange(desc(lps))#Top 
love_per_song%>%arrange(desc(-lps))#least

#Q2: Who is the most negative band in the data set (in terms of sentiment)? Positive?
#song_per_artist
song_per_artist_sent=song_per_artist%>%inner_join(song_words_sentiment, by="artist")
final_sent=song_per_artist_sent%>%group_by(artist)%>%summarise(sentiment=sum(value))
final_sent%>%arrange(desc(-sentiment))
#Positive sentiment Hillsong (value=3898)
#Negative sentiment Insane Clown Posse (value=-8819)
#songs%>%filter(artist=="Insane Clown Posse")%>%inner_join(song_words_sentiment)

#Q3: Which band has the "best" vocabulary?  First define what "best" means and then
#write tidytext code to determine the answer.

#Best vocab
#describing as songs without stop words and have most no stop words per song

#no_stop_words_per_artist
no_stop_words_per_artist<-song_words_no_stop%>%group_by(artist)%>%summarise(word_count=n())

best_vocab<-song_per_artist%>%inner_join(no_stop_words_per_artist, by="artist") #song_count and words
best_vocab$wps<-best_vocab$word_count/best_vocab$song_count

best_vocab%>%arrange(desc(wps)) # Vybz Kartel # wps=268



#Q4: Can you predict who a song is by? Take Katy Perry and Taylor Swift 
#(or 2 other artists with at least 50 songs each) and come up with 5-10 
#features for each song. Split data into train and test and see how accurate 
#a model can be (use glm or rf).


two_artists<-song_words_no_stop%>%filter(tolower(artist)=="katy perry"|tolower(artist)=="taylor swift")
two_songs<-song_words_no_stop%>%filter(tolower(artist)=="katy perry"|tolower(artist)=="taylor swift")%>%group_by(song)
#neg_senti 
#pos_senti
#nostop_words 
#avg_word_length
#words_per_song

two_freq = song_words_no_stop %>%
  filter(artist %in% c("Katy Perry","Taylor Swift"))%>% 
  dplyr::count(artist, word) %>%
  group_by(artist) %>%
  mutate(proportion = n / sum(n)) %>% 
  
  dplyr::select(-n) %>%
  spread(artist,proportion)

two_freq[is.na(two_freq)]=0
names(two_freq)[3]="Swift"
names(two_freq)[2]="Katy"
ggplot(two_freq,aes(x=Katy,y=Swift)) + 
  geom_abline()+
  geom_text(aes(label=word),check_overlap=T)+
  scale_x_log10() + scale_y_log10() 

#love word frequency in katy's song
love_table<-two_artists%>%filter(tolower(word)=="love")%>%
  dplyr::count(word,song)%>%group_by(artist="Katy Perry")%>%
  mutate(love_word=n/sum(n))%>%dplyr::select(-n)

#two_artists<-two_artists%>%filter(artist=="Taylor Swift")%>%cbind(left_join(love_table,by=c("song","artist")))


#time word frequency in swift's song
time_table<-two_artists%>%filter(tolower(word)=="time")%>%
  dplyr::count(word,song)%>%group_by(artist="Taylor Swift")%>%
  mutate(time_word=n/sum(n))%>%dplyr::select(-n)

taylor_avg_senti<-song_words_sentiment %>%filter(artist=="Taylor Swift")%>%
  group_by(artist) %>%
  dplyr::summarise(avg_sentiment = mean(value)) %>%
  arrange(desc(avg_sentiment)) #0.346

katy_avg_senti<-song_words_sentiment %>%filter(artist=="Katy Perry")%>%
  group_by(artist) %>%
  dplyr::summarise(avg_sentiment = mean(value)) %>%
  arrange(desc(avg_sentiment)) #0.346


song_per_artist_sent=song_per_artist%>%inner_join(song_words_sentiment, by="artist")
final_sent=song_per_artist_sent%>%group_by(artist)%>%summarise(sentiment=sum(value))
final_sent%>%arrange(desc(-sentiment))


love_count <- two_artists %>% 
  group_by(artist,song)  %>% 
  filter(word=="love") %>%
  dplyr::count(word,sort=T)
love_count$word = NULL
names(love_count)[3]="love"
two_songs = left_join(two_songs, love_count, by=c("song", "artist"))


time_count <- two_artists %>% 
  group_by(artist,song)  %>% 
  filter(word=="time") %>%
  dplyr::count(word,sort=T)
time_count$word = NULL
names(time_count)[3]="time"
two_songs = left_join(two_songs, time_count, by=c("song", "artist"))

two_songs = left_join(two_songs, taylor_avg_senti, by="artist")
two_songs = left_join(two_songs, katy_avg_senti, by="artist")

names(two_songs)[5]="taylor_avg_senti"
names(two_songs)[6]="katy_avg_senti"

two_songs[is.na(two_songs)]=0


artists_avg_senti<-song_words_sentiment%>%filter(artist=="Taylor Swift"|
                                                   artist=="Katy Perry")%>%group_by(artist,song)%>%summarise(avg_senti=mean(value))
two_songs<-left_join(two_songs,artists_avg_senti,by=c("artist","song"))
two_songs$song=two_songs$avg_senti
two_songs$avg_senti=0
two_songs<- two_songs%>%select(-7)

index=sample(nrow(two_songs),nrow(two_songs)/2,F)
train<-two_songs[index,]
test<-two_songs[-index,]

train$artist=as.numeric(as.factor(train$artist))
test$artist=as.numeric(as.factor(test$artist))
#art_table<-train%>%group_by(song,artist)%>%summarise(med_artist=median(artist))
#train=right_join(train,art_table,by="artist")
names(train)[2]="song_senti"
names(test)[2]="song_senti"


#song_per_artist_sent=song_per_artist%>%inner_join(song_words_sentiment, by="artist")
#final_sent=song_per_artist_sent%>%group_by(artist)%>%summarise(sentiment=sum(value))

library(glm2)
model_glm<-glm(artist~+artist+song_senti+love+time+taylor_avg_senti+katy_avg_senti,data=train,family=gaussian)
test1<-test
test1$artist=NULL
test1$glm_prediction<-predict(model_glm,test1,type="response")
library(Metrics)
rmse(test$artist,test1$glm_prediction)#2.931952e-14

#two_artists%>%left_join(time_table,by=c("song","artist"))

#
#nostop_df$all_word<-
#nostop_df%>%inner_join(two_artists,by="artist")

#  train<-df[1:50]  
