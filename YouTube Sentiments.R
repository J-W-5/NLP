#Load required packages
library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(wordcloud2)
library(reshape2)

#Read in data
dat <- read.csv("Spam Classification.csv")

#Change classification from character to factor
dat$CLASS <- as.factor(dat$CLASS)

#Rename factor levels
levels(dat$CLASS) <- c("Real", "Spam")
levels(dat$CLASS)

#Split up real and spam comments
real_text <- dat %>%
  select(CONTENT, CLASS) %>%
  filter(CLASS == "Real")

spam_text <- dat %>%
  select(CONTENT, CLASS) %>%
  filter(CLASS == "Spam")

#Import NCR lexicon
nrc <- get_sentiments("nrc")

#Clean text
real_clean <- tolower(real_text$CONTENT)
real_clean <- removePunctuation(real_clean)
real_clean <- str_squish(real_clean)
real_clean <- str_split(real_clean, " ")
real_clean <- unlist(real_clean)
real_clean <- tibble(real_clean)
real_clean <- real_clean %>%
  select(word = 1) %>%
  anti_join(stop_words) %>%
  left_join(nrc)

spam_clean <- tolower(spam_text$CONTENT)
spam_clean <- removePunctuation(spam_clean)
spam_clean <- str_squish(spam_clean)
spam_clean <- str_split(spam_clean, " ")
spam_clean <- unlist(spam_clean)
spam_clean <- tibble(spam_clean)
spam_clean <- spam_clean %>%
  select(word = 1, everything()) %>%
  anti_join(stop_words) %>%
  left_join(nrc)

real_clean$class <- ifelse(real_clean$word == " ", 1, 0)
real_clean$class <- as.factor(real_clean$class)
levels(real_clean$class) <- c("Real", "Spam")

spam_clean$class <- ifelse(spam_clean$word == " ", 0, 1)
spam_clean$class <- as.factor(spam_clean$class)
levels(spam_clean$class) <- c("Spam", "Real")

#Join cleaned real and spam comments together
doc <- rbind(real_clean, spam_clean) %>%
  group_by(class) %>%
  mutate(words = n())

doc$sentiment <- as.factor(doc$sentiment)

#Create data frame for visualization
clean_text <- doc %>%
  group_by(class, sentiment) %>%
  summarise(sentiment = unique(sentiment),
            freq = n(),
            words = unique(words)) %>%
  filter(sentiment != "NA") %>%
  mutate(percentage = freq/words*100)

#Create visualization
ggplot(clean_text, aes(x = reorder(sentiment, desc(percentage)), 
                       y = percentage, fill = class)) +
  geom_col(position = "dodge") +
  ggtitle("Sentiment Analysis of YouTube Comments") +
  labs(subtitle = "Analysis of Real and Spam Comments Using the NRC Word-Emotion Association Lexicon (Mohammad & Turney, 2013)") +
  xlab("Sentiment") +
  ylab("Percentage") +
  scale_fill_manual(name = "", values = c("#FF0000", "#282828")) +
  theme_bw() 

#Visualize comparison between real and spam comments
doc %>%
  select(word, class) %>%
  count(word, class, sort = TRUE) %>%
  acast(word ~ class, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#282828", "#FF0000"), max.words = 100,
                   random.order = FALSE, title.size = 3, scale = c(3,.5),
                   rot.per = .1)



