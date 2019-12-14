#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
suppressPackageStartupMessages({
    library(tidyverse)
    library(stringr)
})

#source("app.r")

#' Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("word Prediction Model"),
    p("This app  takes one or three words in a text box and outputs a prediction of the last word."),
    
    # Sidebar with a slider 
    sidebarLayout(
        sidebarPanel(
            h2("Instructions:"), 
            h5("1. Enter a word or words in the text box."),
            h5("3. No need to hit enter of submit."),
            h5("4. A question mark means no prediction"),
            br(),
            a("Source Code", href = "https://github.com/yohan78/CapstoneNPLCoursera-Final/tree/master/Final")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("predict",
                         textInput("user_input", ("Your Input:"),value = "change this"),
                         ("Predicted Next Word:"),(textOutput("ngram_output")))
                
            )   
        )
    )
)




#' Define server logic required to draw a histogram

library(shiny)
require(stringr)
library(sqldf)

 two_Words <- readRDS("dos_Words.rds")
tree_Words  <- readRDS("tres_Words.rds")
four_Words <- readRDS("cuatro_Words.rds")

server <- function(input, output) {
  
  output$ngram_output <- renderText({ngrams(input$user_input)})
  
  }

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
        out <- #ifelse(input_count == 0, "Please input a phrase",
            ifelse(input_count == 3, fourgram(input_words),
                   ifelse(input_count == 2, trigram(input_words), bigram(input_words)))#)
        # Output
        return(out)
    }
  
    
    # Run the application 
shinyApp(ui = ui, server = server)

