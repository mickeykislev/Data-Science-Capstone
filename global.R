# Load packages
library(stringr)
library(utils)

### The Function out of it ###############   
load("working_data2grams_top_mid.rds")
load("working_data3grams_top_mid.rds")
load("working_data4grams_top_mid.rds")
load("twitter2grams_top_mid.rds")
load("twitter3grams_top_mid.rds")
load("twitter4grams_top_mid.rds")
load("blogs2grams_top_mid.rds")
load("blogs3grams_top_mid.rds")
load("blogs4grams_top_mid.rds")
load("news2grams_top_mid.rds")
load("news3grams_top_mid.rds")
load("news4grams_top_mid.rds")


# Creating the models
Pred_text <- function(input, ngram2, ngram3, ngram4){
  inputc <- input
  inputc <- stringr::str_replace_all(inputc, "\\.|\\?|\\,|\\!", " ") # delete numbers
  inputc <- stringr::str_replace_all(inputc, "_", " ") # delete signs
  inputc <- stringr::str_replace_all(inputc, "\\(", " ") # delete signs
  inputc <- stringr::str_replace_all(inputc, "\\)", " ") # delete signs
  inputc <- stringr::str_replace_all(inputc, "\\d", "") # delete numbers
  inputc <- stringr::str_replace_all(inputc, "\n", " ") # delete new lines
  inputc <- stringr::str_replace_all(inputc, "\t", " ") # delete tabs
  inputc <- stringr::str_replace_all(inputc, "\\s+", " ") # Cleaning two or more open spaces
  
  # level the name of colomns
  names(ngram2) <- c("bigrams", "frequency")
  names(ngram3) <- c("threegrams", "frequency")
  names(ngram4) <- c("fourgrams", "frequency")
  
  # add counter of words
  count <- sapply(strsplit(inputc, " "), length)
  
  # limit the words to the last 3, if there are more
  input1 <- if(count > 3){
    input_x <- unlist(strsplit(inputc, " "))[(count-2):count]
    paste(paste("^", input_x[1], " ", input_x[2], " ", input_x[3], " ", sep = ""))
  }else if (count == 1){
    paste("^", inputc, " ", sep = "")
  }else{
    paste("^", inputc, " ", sep = "")
  }
  input1 <- stringr::str_replace_all(input1, "\\s+", " ") # Cleaning two or more open spaces
  count <- sapply(strsplit(input1, " "), length)
  
  # if there are 1 word in the input, to look in the 2gram
  if(count == 1){
    Pred_text_2 <- utils::head(subset(ngram2,
                                      grepl(input1,
                                            ngram2$bigrams,
                                            ignore.case = TRUE)),
                               n = 3L)
    stringr::word(Pred_text_2$bigrams, 2, 2)
  } else if(count == 2){  # if there are 2 words un the input, to look in the 3gram
    Pred_text_3 <- utils::head(subset(ngram3,
                                      grepl(input1,
                                            ngram3$threegrams,
                                            ignore.case = TRUE)),
                               n = 3L)
    outcome3 <- stringr::word(Pred_text_3$threegrams, 3, 3)
    # if there are no results
    if(all(is.na(outcome3) == TRUE)){
      input3 <- unlist(strsplit(input1, " "))[2] # split the input to word and pick the second word
      input3 <- paste("^", input3, sep = "")
      Pred_text_3b <- utils::head(subset(ngram2,
                                         grepl(input3,
                                               ngram2$bigrams,
                                               ignore.case = TRUE)),
                                  n = 3L)
      stringr::word(Pred_text_3b$bigrams, 2, 2)
    }else{
      print(outcome3)
    }
  }else if(count == 3){
    # If there are more than 2 words in the input
    Pred_text_4 <- utils::head(subset(ngram4,
                                      grepl(input1,
                                            ngram4$fourgrams,
                                            ignore.case = TRUE)),
                               n = 3L)
    outcome4 <- stringr::word(Pred_text_4$fourgrams, 4, 4)
    # if there are no results
    if(all(is.na(outcome4) == TRUE)){
      input4a <- unlist(strsplit(input1, " "))[2:3] # split the input to word and pick the second word
      input4a <- paste("^", input4a[1], " ", input4a[2], sep = "")
      Pred_text_4a <- utils::head(subset(ngram3,
                                         grepl(input4a,
                                               ngram3$threegrams,
                                               ignore.case = TRUE)),
                                  n = 3L)
      outcome4a <- stringr::word(Pred_text_4a$threegrams, 3, 3)
      # if there are no results
      if(all(is.na(outcome4a) == TRUE)){
        input4b <- unlist(strsplit(input1, " "))[3] # split the input to word and pick the second word
        input4b <- paste("^", input4b, sep = "")
        Pred_text_4b <- utils::head(subset(ngram2,
                                           grepl(input4b,
                                                 ngram2$bigrams,
                                                 ignore.case = TRUE)),
                                    n = 3L)
        stringr::word(Pred_text_4b$bigrams, 2, 2)
      }else{
        print(outcome4a)
      }
    }else{
      print(outcome4)
    }}
}

