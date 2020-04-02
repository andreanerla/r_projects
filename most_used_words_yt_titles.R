library(dplyr)
library(ggplot2)
library(jsonlite)
library(data.table)
library(lubridate)
library(stringr)
library(tm)
library(tidytext)
library(repr)

CA_data <- read.csv("./Downloads/youtube-new/CAvideos.csv", stringsAsFactors = F)
DE_data <- read.csv("./Downloads/youtube-new/DEvideos.csv", stringsAsFactors = F)
FR_data <- read.csv("./Downloads/youtube-new/FRvideos.csv", stringsAsFactors = F)
GB_data <- read.csv("./Downloads/youtube-new/GBvideos.csv", stringsAsFactors = F)
IN_data <- read.csv("./Downloads/youtube-new/INvideos.csv", stringsAsFactors = F)
JP_data <- read.csv("./Downloads/youtube-new/JPvideos.csv", stringsAsFactors = F)
KR_data <- read.csv("./Downloads/youtube-new/KRvideos.csv", stringsAsFactors = F)
MX_data <- read.csv("./Downloads/youtube-new/MXvideos.csv", stringsAsFactors = F)
RU_data <- read.csv("./Downloads/youtube-new/RUvideos.csv", stringsAsFactors = F)
US_data <- read.csv("./Downloads/youtube-new/USvideos.csv", stringsAsFactors = F)

CA_JSON <- fromJSON("./Downloads/youtube-new/CA_category_id.json")
DE_JSON <- fromJSON("./Downloads/youtube-new/DE_category_id.json")
FR_JSON <- fromJSON("./Downloads/youtube-new/FR_category_id.json")
GB_JSON <- fromJSON("./Downloads/youtube-new/GB_category_id.json")
IN_JSON <- fromJSON("./Downloads/youtube-new/IN_category_id.json")
JP_JSON <- fromJSON("./Downloads/youtube-new/JP_category_id.json")
KR_JSON <- fromJSON("./Downloads/youtube-new/KR_category_id.json")
RU_JSON <- fromJSON("./Downloads/youtube-new/MX_category_id.json")
US_JSON <- fromJSON("./Downloads/youtube-new/US_category_id.json")
MX_JSON <- fromJSON("./Downloads/youtube-new/MX_category_id.json")

US_videos <- as.data.table(US_data)

US_videos$category[US_videos$category == "1"] <- "Film & Animation"
US_videos$category[US_videos$category == "2"] <- "Autos & Vehicles"
US_videos$category[US_videos$category == "10"] <- "Music"
US_videos$category[US_videos$category == "15"] <- "Pets & Animals"
US_videos$category[US_videos$category == "17"] <- "Sports"
US_videos$category[US_videos$category == "18"] <- "Short Movies"
US_videos$category[US_videos$category == "19"] <- "Travel & Events"
US_videos$category[US_videos$category == "20"] <- "Gaming"
US_videos$category[US_videos$category == "21"] <- "Videoblogging"
US_videos$category[US_videos$category == "22"] <- "People & Blogs"
US_videos$category[US_videos$category == "23"] <- "Comedy"
US_videos$category[US_videos$category == "24"] <- "Entertainment"
US_videos$category[US_videos$category == "25"] <- "News & Politics"
US_videos$category[US_videos$category == "26"] <- "Howto & Style"
US_videos$category[US_videos$category == "27"] <- "Education"
US_videos$category[US_videos$category == "28"] <- "Science & Technology"
US_videos$category[US_videos$category == "29"] <- "Nonprofits & Activism"
US_videos$category[US_videos$category == "30"] <- "Movies"
US_videos$category[US_videos$category == "31"] <- "Anime/Animation"
US_videos$category[US_videos$category == "32"] <- "Action/Adventure"
US_videos$category[US_videos$category == "33"] <- "Classics"
US_videos$category[US_videos$category == "34"] <- "Comedy"
US_videos$category[US_videos$category == "35"] <- "Documentary"
US_videos$category[US_videos$category == "36"] <- "Drama"
US_videos$category[US_videos$category == "37"] <- "Family"
US_videos$category[US_videos$category == "38"] <- "Foreign"
US_videos$category[US_videos$category == "39"] <- "Horror"
US_videos$category[US_videos$category == "40"] <- "Sci-Fi/Fantasy"
US_videos$category[US_videos$category == "41"] <- "Thriller"
US_videos$category[US_videos$category == "42"] <- "Short"
US_videos$category[US_videos$category == "43"] <- "Shows"
US_videos$category[US_videos$category == "44"] <- "Trailers"

US_videos$trending_date <- ydm(US_videos$trending_date)

US_videos$publish_time <- ydm_hms(US_videos$publish_time)

US_videos$Hour <- hour(US_videos$publish_time)
options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300) #Setting plot size
US_videos%>%
  ggplot(aes(Hour, fill=..count..))+
  geom_bar(stat = "count")+
  ggtitle("Publication's Hour of Trending Videos")+
  xlab('Hour')+
  ylab('Videos Number')

options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300) 
US_videos%>%
  ggplot(aes(category, views, color=views)) + 
  geom_boxplot() +  
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  ggtitle("Number of Visualizations per Category")+
  xlab('category')+
  ylab('visualizations')

options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300) 
US_videos%>%
  ggplot(aes(category, views, fill=category)) + 
  geom_boxplot() +  
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  scale_y_log10()+
  ggtitle("Number of Visualizations per Category")+
  xlab('category')+
  ylab('views')

US_videos$thumbnail_link <-NULL
US_videos <- US_videos%>%
  filter(!is.na(Hour))

US_videos <- US_videos%>%
  filter(comments_disabled == "False" & 
           ratings_disabled == "False" &
           video_error_or_removed == "False")
sum(US_videos$comments_disabled == "True")
sum(US_videos$ratings_disabled == "True")
sum(US_videos$video_error_or_removed == "True")
sum(is.na(US_videos$views))
#They return 0, as expected.

US_videos$comments_disabled <-NULL
US_videos$ratings_disabled <- NULL
US_videos$video_error_or_removed <- NULL

sapply(US_videos, function(x) {sum(is.na(x))})
#0, as expected

#Turning the number of views from factor to numeric
US_videos$views <- as.numeric(US_videos$views)

total_views <- US_videos%>%
  group_by(category)%>%
  summarise(sum_views = sum(views))%>%
  arrange(desc(sum_views))

total_views
options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300) 
total_views%>%
  ggplot(aes(reorder(category, -sum_views), sum_views, fill=sum_views))+
  geom_bar(stat = "identity", )+
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  ggtitle("Number of Visualizations per Category")+
  xlab('category')+
  ylab('views')

top_5_categories <- US_videos%>%
  filter(category == "Music" | 
           category ==  "Entertainment" | 
           category == "Film & Animation" | 
           category == "Comedy" |
           category == "People & Blogs")

USmusic <- top_5_categories%>%
  filter(category == "Music")

data(stop_words)

stop_words_2 <- data.frame(word = c("video)", "ft.", "feat", "trailer", "(official", "official", "video", "2018", "(2018)", "[official", "(lyric", "music", "(audio)", "video]", "audio]"), lexicon = c("")) 

stop_words <- rbind(stop_words, stop_words_2)
music_titles <- data.frame(USmusic$title)

setnames(music_titles, "title")

music_titles <- music_titles%>%
  mutate(title2 = title)

music_titles$title2 <- as.character(music_titles$title2)

#Removing stopwords
music_titles <- music_titles%>% 
  unnest_tokens(word, title2, token = "regex")%>%
  anti_join(stop_words)

#Removing punctuation
music_count <- mutate(music_titles, titles_no_punct = gsub("[^A-Za-z0-9,;]","", music_titles$word))

music_count <- music_count%>%
  filter(titles_no_punct != "")

music_count$titles_no_punct <- NULL 

music_count <- music_count%>%
  count(word, sort = TRUE)%>%
  arrange(desc(n))
#Filtering only the most used words
top_music_count <- music_count%>%
  filter(n>35)

options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300) 
top_music_count%>%
  ggplot(aes(reorder(word, -n), n, fill=n)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  labs(x = "Word", y = "Count")+
  coord_cartesian(ylim = c(25, 80))+
  ggtitle("Most Used Words in \"Music\"")+
  xlab('words')+
  ylab('count')

USentertainment <- top_5_categories%>%
  filter(category == "Entertainment")
entertainment_titles <- data.frame(USentertainment$title)

setnames(entertainment_titles, "title")

entertainment_titles <- entertainment_titles%>%
  mutate(title2 = title)

entertainment_titles$title2 <- as.character(entertainment_titles$title2)

#Removing stopwords
entertainment_titles <- entertainment_titles%>% 
  unnest_tokens(word, title2, token = "regex")%>%
  anti_join(stop_words)

#Removing punctuation
entertainment_count <- mutate(entertainment_titles, titles_no_punct = gsub("[^A-Za-z0-9,;]","", entertainment_titles$word))
entertainment_count <- entertainment_count%>%
  filter(titles_no_punct != "")

entertainment_count$titles_no_punct <- NULL 

entertainment_count <- entertainment_count%>%
  count(word, sort = TRUE)%>%
  arrange(desc(n))

#Filtering only the most used words
top_entertainment_count <- entertainment_count%>%
  filter(n>50)
options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300) 
top_entertainment_count%>%
  ggplot(aes(reorder(word, -n), n, fill=n)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  labs(x = "Words", y = "Count", title = "Most Used Words in \"Entertainment\"")+
  coord_cartesian(ylim = c(35, 150))

USfilm <- top_5_categories%>%
  filter(category == "Film & Animation")
film_titles <- data.frame(USfilm$title)

setnames(film_titles, "title")

film_titles <- film_titles%>%
  mutate(title2 = title)

film_titles$title2 <- as.character(film_titles$title2)

#Removing stopwords
film_titles <- film_titles%>% 
  unnest_tokens(word, title2, token = "regex")%>%
  anti_join(stop_words)

#Removing punctuation
film_count <- mutate(film_titles, titles_no_punct = gsub("[^A-Za-z0-9,;]","", film_titles$word))

film_count <- film_count%>%
  filter(titles_no_punct != "")

film_count$titles_no_punct <- NULL 

film_count <- film_count%>%
  count(word, sort = TRUE)%>%
  arrange(desc(n))

#Filtering only the most used words
top_film_count <- film_count%>%
  filter(n>20)
options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300)
top_film_count%>%
  ggplot(aes(reorder(word, -n), n, fill=n)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  labs(x = "Words", y = "Count", title = "Most Used Words in \"Film\"")+
  coord_cartesian(ylim = c(15, 100))

UScomedy <- top_5_categories%>%
  filter(category == "Comedy")
comedy_titles <- data.frame(UScomedy$title)

setnames(comedy_titles, "title")

comedy_titles <- comedy_titles%>%
  mutate(title2 = title)

comedy_titles$title2 <- as.character(comedy_titles$title2)

#Removing stopwords
comedy_titles <- comedy_titles%>% 
  unnest_tokens(word, title2, token = "regex")%>%
  anti_join(stop_words)

#Removing punctuation
comedy_count <- mutate(comedy_titles, titles_no_punct = gsub("[^A-Za-z0-9,;]","", comedy_titles$word))

comedy_count <- comedy_count%>%
  filter(titles_no_punct != "")

comedy_count$titles_no_punct <- NULL 
comedy_count <- comedy_count%>%
  count(word, sort = TRUE)%>%
  arrange(desc(n))

#Filtering only the most used words
top_comedy_count <- comedy_count%>%
  filter(n>20)
options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300)
top_comedy_count%>%
  ggplot(aes(reorder(word, -n), n, fill=n)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  labs(x = "Words", y = "Count", title = "Most Used Words in \"Comedy\"")+
  coord_cartesian(ylim = c(15, 70))

USblogs <- top_5_categories%>%
  filter(category == "People & Blogs")
blogs_titles <- data.frame(USblogs$title)

setnames(blogs_titles, "title")

blogs_titles <- blogs_titles%>%
  mutate(title2 = title)

blogs_titles$title2 <- as.character(blogs_titles$title2)

#Removing stopwords
blogs_titles <- blogs_titles%>% 
  unnest_tokens(word, title2, token = "regex")%>%
  anti_join(stop_words)

#Removing punctuation
blogs_count <- mutate(blogs_titles, titles_no_punct = gsub("[^A-Za-z0-9,;]","", blogs_titles$word))

blogs_count <- blogs_count%>%
  filter(titles_no_punct != "")

blogs_count$titles_no_punct <- NULL 
blogs_count <- blogs_count%>%
  count(word, sort = TRUE)%>%
  arrange(desc(n))

#Filtering only the most used words
top_blogs_count <- blogs_count%>%
  filter(n>20)
options(repr.plot.width = 8, repr.plot.height = 4, repr.plot.res = 300)
top_blogs_count%>%
  ggplot(aes(reorder(word, -n), n, fill=n)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
  labs(x = "Words", y = "Count", title = "Most Used Words in \"People & Blogs\"")+
  coord_cartesian(ylim = c(15, 40))
