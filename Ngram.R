#Prediction Model
# This prediction model example was used taking as an example mark-blackmore github page 
#https://github.com/mark-blackmore/JHU-Data-Science-Capstone
#Thanks Mark really learned a lot and was very helpfull

two_Words <- readRDS(".//tree/master//Final//two_Words.rds")
tree_Words  <- readRDS(".//tree/master//Final//tree_Words.rds")
four_Words <- readRDS(".//tree//master//Final//four_Words.rds")



bigram <- function(input_words){
        num <- length(input_words)
        filter(two_Words, 
               word1==input_words[num]) %>% 
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 2)) %>%
                as.character() -> out
        ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
        num <- length(input_words)
        filter(tree_Words, 
               word1==input_words[num-1], 
               word2==input_words[num])  %>% 
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 3)) %>%
                as.character() -> out
        ifelse(out=="character(0)", bigram(input_words), return(out))
}

fourgram <- function(input_words){
        num <- length(input_words)
        filter(four_Words, 
               word1==input_words[num-2], 
               word2==input_words[num-1], 
               word3==input_words[num])  %>% 
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 4)) %>%
                as.character() -> out
        ifelse(out=="character(0)", trigram(input_words), return(out))
}


#Create User Input and Data Cleaning Function; Calls the matching functions

ngrams <- function(input){
        input <- data_frame(text = input)
        replace_reg <- "[^[:alpha:][:space:]]*"
        input <- input %>%
                mutate(text = str_replace_all(text, replace_reg, ""))
        input_count <- str_count(input, boundary("word"))
        input_words <- unlist(str_split(input, boundary("word")))
        input_words <- tolower(input_words)
        out <- ifelse(input_count == 1, bigram(input_words), 
                      ifelse (input_count == 2, trigram(input_words), fourgram(input_words)))
        # Output
        return(out)
}