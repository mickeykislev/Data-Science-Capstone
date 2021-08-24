


### Technical preparation ########################

# Set Locallity
Sys.setlocale(category = "LC_ALL", locale = "english")

# Set numbers format
options("scipen" = 20)

# define memory used to use also virtual memory to the CPU limits 
# after OS needs 64GB - 6GB = 58 GB = 59392MB
memory.limit(size = 59392)

# Load packages
requiredPackages <- c('tidyverse', 'textclean', 'stringr', 'qdapRegex', 'tm', 'tidytext');
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}
rm(p, requiredPackages)


# Loading the data as separate long charecter data sets
blogs <- readLines("en_US.blogs.txt", skipNul = TRUE, encoding="UTF-8") 
twitter <- readLines("en_US.twitter.txt", skipNul = TRUE, encoding="UTF-8")
news <- readLines("en_US.news.txt", skipNul = TRUE, encoding="UTF-8")


### sample all three data sets - Create a sample dataset ######

# set seed for reproducability
set.seed(15536)

# sample all three data sets
blogs_smpl <- sample(blogs, length(blogs) * 1, replace = FALSE)
news_smpl <- sample(news, length(news) * 10, replace = TRUE)
twitter_smpl <- sample(twitter, length(twitter) * 1, replace = FALSE)

# group all sources into one dataset
working_data <- c(blogs_smpl, news_smpl, twitter_smpl) # combine all three data sets into a single data set

# clean concole from unneeded sets
rm(blogs_smpl, news_smpl, twitter_smpl)

rm(twitter, news, blogs)

### samplee all three data sets - Clean data ######

# defining UTF-8 signs to symbols


# Clean extra spaces
working_data <- str_replace_all(working_data, "\\s+", " ") # Cleaning two or more open spaces 
working_data <- str_squish(working_data) # removes whitespace from start and end of string

# Clean Commercial bots 
working_data <- subset(working_data, !str_detect(working_data, " www")) # clean commorcial data
working_data <- subset(working_data, !str_detect(working_data, "www")) # clean commorcial data
working_data <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", working_data, ignore.case = FALSE, perl = TRUE) # remove URL, 
working_data <- gsub("\\S+[@]\\S+", "", working_data, ignore.case = FALSE, perl = TRUE) # remove email addresses, 
working_data <- gsub("@[^\\s]+", "", working_data, ignore.case = FALSE, perl = TRUE) # remove Twitter handles
working_data <- gsub("#[^\\s]+", "", working_data, ignore.case = FALSE, perl = TRUE) # remove hash tags
working_data <- replace_emoji(working_data)  # remove Emoji

# Clean extra signs
working_data <- gsub(pattern = "\\d", replace = "", working_data) # delete numbers
working_data <- gsub(pattern = "\n", replace = " ", working_data) # delete new lines
working_data <- gsub(pattern = "\t", replace = " ", working_data) # delete tabs
working_data <- gsub(pattern = "\\()", replace = "", working_data) # delete numbers
working_data <- gsub(pattern = "\\.|\\?|\\,|\\!", replace = "", working_data) # delete numbers
working_data <- gsub(pattern = "_", replace = " ", working_data) # delete signs
working_data <- gsub(pattern = "\\(", replace = " ", working_data) # delete signs
working_data <- gsub(pattern = "\\)", replace = " ", working_data) # delete signs
working_data <- gsub(pattern = "\\b[B-H]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "\\b[J-K]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "\\b[O-R]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "\\b[U-Z]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "\\b[b-h]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "\\b[j-k]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "\\b[o-r]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "\\b[u-z]\\b{1}", replace = "", working_data) # delete sepurate letters
working_data <- gsub(pattern = "('\\\\')", replace = "", working_data) # delete backslash signs
working_data <- gsub(pattern = " ‘", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "’ ", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "’", replace = "'", working_data) # delete signs
working_data <- gsub(pattern = "^“", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "^:", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "^ ", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "”$", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "\"", replace = "", working_data) # delete signs
working_data <- gsub(pattern = ";$", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "<", replace = "", working_data) # delete signs
working_data <- gsub(pattern = ">", replace = "", working_data) # delete signs
working_data <- gsub(pattern = "~", replace = "", working_data) # delete signs
working_data <- gsub(pattern = " :", replace = ":", working_data) # delete signs
working_data <- gsub(pattern = "^:", replace = "", working_data) # delete signs
working_data <- gsub(pattern = ":$", replace = "", working_data) # delete signs
working_data <- gsub(pattern = " $", replace = "", working_data) # delete signs

# Clean emotional symbols
working_data <- rm_emoticon(working_data) # emoticons


# Clean extra spaces after cleaning extra signs
working_data <- stripWhitespace(working_data) # delete extra open spaces

# replace all text to lower text
working_data <- tolower(working_data) 

# remove profane words
badWordsFile <- "bad-words.txt"
working_data <- removeWords(working_data, badWordsFile)
rm(badWordsFile)


save(working_data, file = "working_data_source_clean_final.rds")



### sample all three data sets - concordances ######

# Creating a table of words
working_data_token <- working_data %>% as_tibble() %>% unnest_tokens(words, value)
working_data_token;

# create data frame with words that are connected
working_data_ngram_2 <- working_data_token %>% rename(word1 = words) %>%
  mutate(word2 = c(word1[2:length(word1)], NA)) %>% na.omit();

# Creating a table of pers of words
working_data_token_2 <- working_data_ngram_2 %>% 
  mutate(bigram = paste(word1, word2, sep = " ")) %>% na.omit();
working_data_token_2

# create data frame with words that are connected to the pars
working_data_ngram_3 <- working_data_token_2 %>% rename(word2b = bigram) %>%
  mutate(word3 = c(word2b[2:length(word2b)], NA)) %>% na.omit()
rm(working_data_token_2, working_data_ngram_2, working_data_token)


# Creating a table of three words expressions
working_data_token_3 <- working_data_ngram_3 %>% 
  mutate(trigram = paste(word1, word3, sep = " ")) %>% na.omit();
working_data_token_3
rm(working_data_ngram_3)

# create data frame with words that are connected to three other words
working_data_ngram_4 <- working_data_token_3 %>% rename(word3b = trigram) %>%
  mutate(word4 = c(word3b[2:length(word3b)], NA)) %>% na.omit()
rm(working_data_token_3)
rm(working_data)
# Creating a table of four word expressions
working_data_token_4 <- working_data_ngram_4 %>% 
  mutate(forthgrams = paste(word1, word4, sep = " ")) %>% na.omit();
working_data_token_4
rm(working_data_ngram_4)

# Creating a summarized table
working_data_tokens_1_4 <- working_data_token_4[,c(1, 3, 5, 7)]
names(working_data_tokens_1_4) <- c("unigram", "bigrams", "threegrams", "fourgrams")

# Clean Consule
rm(working_data_token_4)
save(working_data_tokens_1_4, file = "working_data_tokens_1_4_final.rds")


### sample all three data sets - concordances ######

# split by frequencies, and delete the non-frequent words - 2gram
working_data2grams <- working_data_tokens_1_4 %>% group_by(bigrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
working_data2grams_top_mid <- working_data2grams[working_data2grams$frequency >= 20,]
save(working_data2grams, file = "working_data2grams.rds")
save(working_data2grams_top_mid, file = "working_data2grams_top_mid.rds")
rm(working_data2grams, working_data2grams_top_mid)


# split by frequencies, and delete the non-frequent words - 3gram
working_data3grams <- working_data_tokens_1_4 %>% group_by(threegrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
working_data3grams_top_mid <- working_data3grams[working_data3grams$frequency >= 20,]
save(working_data3grams_top_mid, file = "working_data3grams_top_mid.rds")
save(working_data3grams, file = "working_data3grams.rds")
rm(working_data3grams, working_data3grams_top_mid)

# split by frequencies, and delete the non-frequent words - 4gram
working_data4grams <- working_data_tokens_1_4 %>% group_by(fourgrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
working_data4grams_top_mid <- working_data4grams[working_data4grams$frequency >= 20,]
save(working_data4grams_top_mid, file = "working_data4grams_top_mid.rds")
save(working_data4grams, file = "working_data4grams.rds")
rm(working_data4grams, working_data4grams_top_mid)


### sample all three data sets - Create a sample dataset ######

# Loading the data as separate long charecter data sets
twitter <- readLines("en_US.twitter.txt", skipNul = TRUE, encoding="UTF-8")

# set seed for reproducability
set.seed(15536)


### samplee all three data sets - Clean data ######

# defining UTF-8 signs to symbols


# Clean extra spaces
twitter <- str_replace_all(twitter, "\\s+", " ") # Cleaning two or more open spaces 
twitter <- str_squish(twitter) # removes whitespace from start and end of string

# Clean Commercial bots 
twitter <- subset(twitter, !str_detect(twitter, " www")) # clean commorcial data
twitter <- subset(twitter, !str_detect(twitter, "www")) # clean commorcial data
twitter <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", twitter, ignore.case = FALSE, perl = TRUE) # remove URL, 
twitter <- gsub("\\S+[@]\\S+", "", twitter, ignore.case = FALSE, perl = TRUE) # remove email addresses, 
twitter <- gsub("@[^\\s]+", "", twitter, ignore.case = FALSE, perl = TRUE) # remove Twitter handles
twitter <- gsub("#[^\\s]+", "", twitter, ignore.case = FALSE, perl = TRUE) # remove hash tags
twitter <- replace_emoji(twitter)  # remove Emoji

# Clean extra signs
twitter <- gsub(pattern = "\\d", replace = "", twitter) # delete numbers
twitter <- gsub(pattern = "\n", replace = " ", twitter) # delete new lines
twitter <- gsub(pattern = "\t", replace = " ", twitter) # delete tabs
twitter <- gsub(pattern = "\\()", replace = "", twitter) # delete numbers
twitter <- gsub(pattern = "\\.|\\?|\\,|\\!", replace = "", twitter) # delete numbers
twitter <- gsub(pattern = "_", replace = " ", twitter) # delete signs
twitter <- gsub(pattern = "\\(", replace = " ", twitter) # delete signs
twitter <- gsub(pattern = "\\)", replace = " ", twitter) # delete signs
twitter <- gsub(pattern = "\\b[B-H]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "\\b[J-K]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "\\b[O-R]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "\\b[U-Z]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "\\b[b-h]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "\\b[j-k]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "\\b[o-r]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "\\b[u-z]\\b{1}", replace = "", twitter) # delete sepurate letters
twitter <- gsub(pattern = "('\\\\')", replace = "", twitter) # delete backslash signs
twitter <- gsub(pattern = " ‘", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "’ ", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "’", replace = "'", twitter) # delete signs
twitter <- gsub(pattern = "^“", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "^:", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "^ ", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "”$", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "\"", replace = "", twitter) # delete signs
twitter <- gsub(pattern = ";$", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "<", replace = "", twitter) # delete signs
twitter <- gsub(pattern = ">", replace = "", twitter) # delete signs
twitter <- gsub(pattern = "~", replace = "", twitter) # delete signs
twitter <- gsub(pattern = " :", replace = ":", twitter) # delete signs
twitter <- gsub(pattern = "^:", replace = "", twitter) # delete signs
twitter <- gsub(pattern = ":$", replace = "", twitter) # delete signs
twitter <- gsub(pattern = " $", replace = "", twitter) # delete signs

# Clean emotional symbols
twitter <- rm_emoticon(twitter) # emoticons


# Clean extra spaces after cleaning extra signs
twitter <- stripWhitespace(twitter) # delete extra open spaces

# replace all text to lower text
twitter <- tolower(twitter) 

# remove profane words
badWordsFile <- "bad-words.txt"
twitter <- removeWords(twitter, badWordsFile)
rm(badWordsFile)


save(twitter, file = "twitter_source_clean_final.rds")



### sample all three data sets - concordances ######

# Creating a table of words
twitter_token <- twitter %>% as_tibble() %>% unnest_tokens(words, value)
twitter_token;

# create data frame with words that are connected
twitter_ngram_2 <- twitter_token %>% rename(word1 = words) %>%
  mutate(word2 = c(word1[2:length(word1)], NA)) %>% na.omit();

# Creating a table of pers of words
twitter_token_2 <- twitter_ngram_2 %>% 
  mutate(bigram = paste(word1, word2, sep = " ")) %>% na.omit();
twitter_token_2

# create data frame with words that are connected to the pars
twitter_ngram_3 <- twitter_token_2 %>% rename(word2b = bigram) %>%
  mutate(word3 = c(word2b[2:length(word2b)], NA)) %>% na.omit()
rm(twitter_token_2, twitter_ngram_2, twitter_token)


# Creating a table of three words expressions
twitter_token_3 <- twitter_ngram_3 %>% 
  mutate(trigram = paste(word1, word3, sep = " ")) %>% na.omit();
twitter_token_3
rm(twitter_ngram_3)

# create data frame with words that are connected to three other words
twitter_ngram_4 <- twitter_token_3 %>% rename(word3b = trigram) %>%
  mutate(word4 = c(word3b[2:length(word3b)], NA)) %>% na.omit()
rm(twitter_token_3)
rm(twitter)
# Creating a table of four word expressions
twitter_token_4 <- twitter_ngram_4 %>% 
  mutate(forthgrams = paste(word1, word4, sep = " ")) %>% na.omit();
twitter_token_4
rm(twitter_ngram_4)

# Creating a summarized table
twitter_tokens_1_4 <- twitter_token_4[,c(1, 3, 5, 7)]
names(twitter_tokens_1_4) <- c("unigram", "bigrams", "threegrams", "fourgrams")

# Clean Consule
rm(twitter_token_4)
save(twitter_tokens_1_4, file = "twitter_tokens_1_4_final.rds")


### sample all three data sets - concordances ######

# split by frequencies, and delete the non-frequent words - 2gram
twitter2grams <- twitter_tokens_1_4 %>% group_by(bigrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
twitter2grams_top_mid <- twitter2grams[twitter2grams$frequency >= 20,]
save(twitter2grams, file = "twitter2grams.rds")
save(twitter2grams_top_mid, file = "twitter2grams_top_mid.rds")
rm(twitter2grams, twitter2grams_top_mid)


# split by frequencies, and delete the non-frequent words - 3gram
twitter3grams <- twitter_tokens_1_4 %>% group_by(threegrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
twitter3grams_top_mid <- twitter3grams[twitter3grams$frequency >= 20,]
save(twitter3grams, file = "twitter3grams.rds")
save(twitter3grams_top_mid, file = "twitter3grams_top_mid.rds")
rm(twitter3grams, twitter3grams_top_mid)

# split by frequencies, and delete the non-frequent words - 4gram
twitter4grams <- twitter_tokens_1_4 %>% group_by(fourgrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
twitter4grams_top_mid <- twitter4grams[twitter4grams$frequency >= 20,]
save(twitter4grams, file = "twitter4grams.rds")
save(twitter4grams_top_mid, file = "twitter4grams_top_mid.rds")
rm(twitter4grams, twitter4grams_top_mid)


### sample all three data sets - Create a sample dataset ######

# Loading the data as separate long charecter data sets
blogs <- readLines("en_US.blogs.txt", skipNul = TRUE, encoding="UTF-8")


# set seed for reproducability
set.seed(15536)


### samplee all three data sets - Clean data ######

# defining UTF-8 signs to symbols


# Clean extra spaces
blogs <- str_replace_all(blogs, "\\s+", " ") # Cleaning two or more open spaces 
blogs <- str_squish(blogs) # removes whitespace from start and end of string

# Clean Commercial bots 
blogs <- subset(blogs, !str_detect(blogs, " www")) # clean commorcial data
blogs <- subset(blogs, !str_detect(blogs, "www")) # clean commorcial data
blogs <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", blogs, ignore.case = FALSE, perl = TRUE) # remove URL, 
blogs <- gsub("\\S+[@]\\S+", "", blogs, ignore.case = FALSE, perl = TRUE) # remove email addresses, 
blogs <- gsub("@[^\\s]+", "", blogs, ignore.case = FALSE, perl = TRUE) # remove blogs handles
blogs <- gsub("#[^\\s]+", "", blogs, ignore.case = FALSE, perl = TRUE) # remove hash tags
blogs <- replace_emoji(blogs)  # remove Emoji

# Clean extra signs
blogs <- gsub(pattern = "\\d", replace = "", blogs) # delete numbers
blogs <- gsub(pattern = "\n", replace = " ", blogs) # delete new lines
blogs <- gsub(pattern = "\t", replace = " ", blogs) # delete tabs
blogs <- gsub(pattern = "\\()", replace = "", blogs) # delete numbers
blogs <- gsub(pattern = "\\.|\\?|\\,|\\!", replace = "", blogs) # delete numbers
blogs <- gsub(pattern = "_", replace = " ", blogs) # delete signs
blogs <- gsub(pattern = "\\(", replace = " ", blogs) # delete signs
blogs <- gsub(pattern = "\\)", replace = " ", blogs) # delete signs
blogs <- gsub(pattern = "\\b[B-H]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "\\b[J-K]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "\\b[O-R]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "\\b[U-Z]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "\\b[b-h]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "\\b[j-k]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "\\b[o-r]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "\\b[u-z]\\b{1}", replace = "", blogs) # delete sepurate letters
blogs <- gsub(pattern = "('\\\\')", replace = "", blogs) # delete backslash signs
blogs <- gsub(pattern = " ‘", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "’ ", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "’", replace = "'", blogs) # delete signs
blogs <- gsub(pattern = "^“", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "^:", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "^ ", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "”$", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "\"", replace = "", blogs) # delete signs
blogs <- gsub(pattern = ";$", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "<", replace = "", blogs) # delete signs
blogs <- gsub(pattern = ">", replace = "", blogs) # delete signs
blogs <- gsub(pattern = "~", replace = "", blogs) # delete signs
blogs <- gsub(pattern = " :", replace = ":", blogs) # delete signs
blogs <- gsub(pattern = "^:", replace = "", blogs) # delete signs
blogs <- gsub(pattern = ":$", replace = "", blogs) # delete signs
blogs <- gsub(pattern = " $", replace = "", blogs) # delete signs

# Clean emotional symbols
blogs <- rm_emoticon(blogs) # emoticons


# Clean extra spaces after cleaning extra signs
blogs <- stripWhitespace(blogs) # delete extra open spaces

# replace all text to lower text
blogs <- tolower(blogs) 

# remove profane words
badWordsFile <- "bad-words.txt"
blogs <- removeWords(blogs, badWordsFile)
rm(badWordsFile)


save(blogs, file = "blogs_source_clean_final.rds")



### sample all three data sets - concordances ######

# Creating a table of words
blogs_token <- blogs %>% as_tibble() %>% unnest_tokens(words, value)
blogs_token;

# create data frame with words that are connected
blogs_ngram_2 <- blogs_token %>% rename(word1 = words) %>%
  mutate(word2 = c(word1[2:length(word1)], NA)) %>% na.omit();

# Creating a table of pers of words
blogs_token_2 <- blogs_ngram_2 %>% 
  mutate(bigram = paste(word1, word2, sep = " ")) %>% na.omit();
blogs_token_2

# create data frame with words that are connected to the pars
blogs_ngram_3 <- blogs_token_2 %>% rename(word2b = bigram) %>%
  mutate(word3 = c(word2b[2:length(word2b)], NA)) %>% na.omit()
rm(blogs_token_2, blogs_ngram_2, blogs_token)


# Creating a table of three words expressions
blogs_token_3 <- blogs_ngram_3 %>% 
  mutate(trigram = paste(word1, word3, sep = " ")) %>% na.omit();
blogs_token_3
rm(blogs_ngram_3)

# create data frame with words that are connected to three other words
blogs_ngram_4 <- blogs_token_3 %>% rename(word3b = trigram) %>%
  mutate(word4 = c(word3b[2:length(word3b)], NA)) %>% na.omit()
rm(blogs_token_3)
rm(blogs)
# Creating a table of four word expressions
blogs_token_4 <- blogs_ngram_4 %>% 
  mutate(forthgrams = paste(word1, word4, sep = " ")) %>% na.omit();
blogs_token_4
rm(blogs_ngram_4)

# Creating a summarized table
blogs_tokens_1_4 <- blogs_token_4[,c(1, 3, 5, 7)]
names(blogs_tokens_1_4) <- c("unigram", "bigrams", "threegrams", "fourgrams")

# Clean Consule
rm(blogs_token_4)
save(blogs_tokens_1_4, file = "blogs_tokens_1_4_final.rds")


### sample all three data sets - concordances ######

# split by frequencies, and delete the non-frequent words - 2gram
blogs2grams <- blogs_tokens_1_4 %>% group_by(bigrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
blogs2grams_top_mid <- blogs2grams[blogs2grams$frequency >= 20,]
save(blogs2grams, file = "blogs2grams.rds")
save(blogs2grams_top_mid, file = "blogs2grams_top_mid.rds")
rm(blogs2grams, blogs2grams_top_mid)

# split by frequencies, and delete the non-frequent words - 3gram
blogs3grams <- blogs_tokens_1_4 %>% group_by(threegrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
blogs3grams_top_mid <- blogs3grams[blogs3grams$frequency >= 20,]
save(blogs3grams, file = "blogs3grams.rds")
save(blogs3grams_top_mid, file = "blogs3grams_top_mid.rds")
rm(blogs3grams, blogs3grams_top_mid)

# split by frequencies, and delete the non-frequent words - 4gram
blogs4grams <- blogs_tokens_1_4 %>% group_by(fourgrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
blogs4grams_top_mid <- blogs4grams[blogs4grams$frequency >= 20,]
save(blogs4grams, file = "blogs4grams.rds")
save(blogs4grams_top_mid, file = "blogs4grams_top_mid.rds")
rm(blogs4grams, blogs4grams_top_mid)

### sample all three data sets - Create a sample dataset ######

# Loading the data as separate long charecter data sets
news <- readLines("en_US.news.txt", skipNul = TRUE, encoding="UTF-8")


# set seed for reproducability
set.seed(15536)


### samplee all three data sets - Clean data ######

# defining UTF-8 signs to symbols


# Clean extra spaces
news <- str_replace_all(news, "\\s+", " ") # Cleaning two or more open spaces 
news <- str_squish(news) # removes whitespace from start and end of string

# Clean Commercial bots 
news <- subset(news, !str_detect(news, " www")) # clean commorcial data
news <- subset(news, !str_detect(news, "www")) # clean commorcial data
news <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", news, ignore.case = FALSE, perl = TRUE) # remove URL, 
news <- gsub("\\S+[@]\\S+", "", news, ignore.case = FALSE, perl = TRUE) # remove email addresses, 
news <- gsub("@[^\\s]+", "", news, ignore.case = FALSE, perl = TRUE) # remove news handles
news <- gsub("#[^\\s]+", "", news, ignore.case = FALSE, perl = TRUE) # remove hash tags
news <- replace_emoji(news)  # remove Emoji

# Clean extra signs
news <- gsub(pattern = "\\d", replace = "", news) # delete numbers
news <- gsub(pattern = "\n", replace = " ", news) # delete new lines
news <- gsub(pattern = "\t", replace = " ", news) # delete tabs
news <- gsub(pattern = "\\()", replace = "", news) # delete numbers
news <- gsub(pattern = "\\.|\\?|\\,|\\!", replace = "", news) # delete numbers
news <- gsub(pattern = "_", replace = " ", news) # delete signs
news <- gsub(pattern = "\\(", replace = " ", news) # delete signs
news <- gsub(pattern = "\\)", replace = " ", news) # delete signs
news <- gsub(pattern = "\\b[B-H]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "\\b[J-K]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "\\b[O-R]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "\\b[U-Z]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "\\b[b-h]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "\\b[j-k]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "\\b[o-r]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "\\b[u-z]\\b{1}", replace = "", news) # delete sepurate letters
news <- gsub(pattern = "('\\\\')", replace = "", news) # delete backslash signs
news <- gsub(pattern = " ‘", replace = "", news) # delete signs
news <- gsub(pattern = "’ ", replace = "", news) # delete signs
news <- gsub(pattern = "’", replace = "'", news) # delete signs
news <- gsub(pattern = "^“", replace = "", news) # delete signs
news <- gsub(pattern = "^:", replace = "", news) # delete signs
news <- gsub(pattern = "^ ", replace = "", news) # delete signs
news <- gsub(pattern = "”$", replace = "", news) # delete signs
news <- gsub(pattern = "\"", replace = "", news) # delete signs
news <- gsub(pattern = ";$", replace = "", news) # delete signs
news <- gsub(pattern = "<", replace = "", news) # delete signs
news <- gsub(pattern = ">", replace = "", news) # delete signs
news <- gsub(pattern = "~", replace = "", news) # delete signs
news <- gsub(pattern = " :", replace = ":", news) # delete signs
news <- gsub(pattern = "^:", replace = "", news) # delete signs
news <- gsub(pattern = ":$", replace = "", news) # delete signs
news <- gsub(pattern = " $", replace = "", news) # delete signs

# Clean emotional symbols
news <- rm_emoticon(news) # emoticons


# Clean extra spaces after cleaning extra signs
news <- stripWhitespace(news) # delete extra open spaces

# replace all text to lower text
news <- tolower(news) 

# remove profane words
badWordsFile <- "bad-words.txt"
news <- removeWords(news, badWordsFile)
rm(badWordsFile)


save(news, file = "news_source_clean_final.rds")



### sample all three data sets - concordances ######

# Creating a table of words
news_token <- news %>% as_tibble() %>% unnest_tokens(words, value)
news_token;

# create data frame with words that are connected
news_ngram_2 <- news_token %>% rename(word1 = words) %>%
  mutate(word2 = c(word1[2:length(word1)], NA)) %>% na.omit();

# Creating a table of pers of words
news_token_2 <- news_ngram_2 %>% 
  mutate(bigram = paste(word1, word2, sep = " ")) %>% na.omit();
news_token_2

# create data frame with words that are connected to the pars
news_ngram_3 <- news_token_2 %>% rename(word2b = bigram) %>%
  mutate(word3 = c(word2b[2:length(word2b)], NA)) %>% na.omit()
rm(news_token_2, news_ngram_2, news_token)


# Creating a table of three words expressions
news_token_3 <- news_ngram_3 %>% 
  mutate(trigram = paste(word1, word3, sep = " ")) %>% na.omit();
news_token_3
rm(news_ngram_3)

# create data frame with words that are connected to three other words
news_ngram_4 <- news_token_3 %>% rename(word3b = trigram) %>%
  mutate(word4 = c(word3b[2:length(word3b)], NA)) %>% na.omit()
rm(news_token_3)
rm(news)
# Creating a table of four word expressions
news_token_4 <- news_ngram_4 %>% 
  mutate(forthgrams = paste(word1, word4, sep = " ")) %>% na.omit();
news_token_4
rm(news_ngram_4)

# Creating a summarized table
news_tokens_1_4 <- news_token_4[,c(1, 3, 5, 7)]
names(news_tokens_1_4) <- c("unigram", "bigrams", "threegrams", "fourgrams")

# Clean Consule
rm(news_token_4)
save(news_tokens_1_4, file = "news_tokens_1_4_final.rds")


### sample all three data sets - concordances ######

# split by frequencies, and delete the non-frequent words - 2gram
news2grams <- news_tokens_1_4 %>% group_by(bigrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
news2grams_top_mid <- news2grams[news2grams$frequency >= 20,]
save(news2grams, file = "news2grams.rds")
save(news2grams_top_mid, file = "news2grams_top_mid.rds")
rm(news2grams, news2grams_top_mid)


# split by frequencies, and delete the non-frequent words - 3gram
news3grams <- news_tokens_1_4 %>% group_by(threegrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
news3grams_top_mid <- news3grams[news3grams$frequency >= 20,]
save(news3grams, file = "news3grams.rds")
save(news3grams_top_mid, file = "news3grams_top_mid.rds")
rm(news3grams, news3grams_top_mid)

# split by frequencies, and delete the non-frequent words - 4gram
news4grams <- news_tokens_1_4 %>% group_by(fourgrams) %>%
  summarise(frequency = n()) %>% arrange(-frequency)
news4grams_top_mid <- news4grams[news4grams$frequency >= 20,]
save(news4grams, file = "news4grams.rds")
save(news4grams_top_mid, file = "news4grams_top_mid.rds")
rm(news4grams, news4grams_top_mid)


