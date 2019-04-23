#to get data
library(twitteR)
#for storing data
library(stringr)
library(qdapRegex)
# for analysis
library(tm)
library(tidyverse)
library(tidytext)
library(stringr)
library(tibble)
library(reshape2)
library(wordcloud)
library(wordcloud2)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(janeaustenr)

# code start
# getting data from twitter
consumer_key <-""
consumer_secret <-""               #twitter API keys  here 
access_token <-""
access_token_secret <-""

accessURL="https://api.twitter.com/oauth/access_token"
authURL="https://api.twitter.com/oauth/authorize"
reqURL="https://api.twitter.com/oauth/request_token"
#reqURL="https://api.twitter.com/oauth/authorize?source=twitter" 

twitCred <- OAuthFactory$new(consumerKey=consumer_key,
                             consumerSecret=consumer_secret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitCred$handshake()



setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_token_secret)

search_data <- searchTwitter('#notebook',n=100,lang = "en",include_rts = FALSE)
DataFrame<-twListToDF(search_data)
file.timeline<-paste("notebook_data.csv",sep="")
write.csv(DataFrame,file.timeline)

# storing data into a file

data = read.csv(file = "C:\\Users\\DELL-PC\\Documents\\tWdataNotebook1.csv")
text_data = data$text
x <- paste(text_data,collapse = " ")

fileConn<-file("tWdataNotebook1.txt")
writeLines(x,fileConn)
close(fileConn)

# analysis

# put/convert ind texts to a data frame
fetch_words <- tibble(file = paste0("C:\\Users\\DELL-PC\\Documents\\",c("notebook.txt"))) %>%
  mutate(text = map(file, read_lines))
fetch_words

# The resulting tibble has a variable file that is the name of the file that created 
# We want to unnest() that tibble, remove the lines that are LaTeX crude (either start with \[A-Z] or \[a-z], like \section or \figure) and compute a line number.

fetch_words <- fetch_words %>%
  unnest() %>%
  filter(text != "%!TEX root = ind.tex") %>%
  filter(!str_detect(text, "^(\\\\[A-Z,a-z])"),
         text != "") %>%
  mutate(line_number = 1:n(),
         file = str_sub(basename(file), 1, -5))


# Now we want to tokenize (strip each word of any formatting and reduce down to the root word, if possible).												  
# as well as we will have only words assigned with line numbers

fetch_words <- fetch_words %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[0-9]"),
         word != "#",
         word != ".",
         word != "is",
         word != "am",
         word != "are",
         word != "and",
         word != "a",
         word != "the",
         word != "rt",
         word != "t.co",
         word != "of",
         word != "https",
         word != "u",
         !str_detect(word, "textless"))
fetch_words

# Now to compute the sentiment using the words written per line in the fetched words
# Using the nrc lexicon.

fetch_words %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(index = line_number %/% 25, file, sentiment) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = index, y = n, fill = file)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  facet_wrap(~ sentiment, ncol = 5)



# Filter for negative words
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")
# Get a word count
wordcounts <- fetch_words %>%
  group_by(index = line_number %/% 25, file) %>%
  summarize(words = n())
wordcounts
# Build	a cloud chart
fetch_words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
# Build a contrasting cloud chart.
fetch_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)  
# END OF SCRIPT