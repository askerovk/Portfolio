library(shiny)
library(quanteda)
library(data.table)
library(stringr)

load("n1grams-final")

load("n2grams-final")

load("n3grams-final")

load("n4grams-final")

load("n5grams-final")

vocab <- n1grams$words

code <- n1grams$ngrams

unigrams <- c("the","and", "to", "a", "in", "of", "is", "for", "that", "on")

rmprefix <- function(x){
        
    str_replace(string = x, pattern = "^[a-z0-9]{3}", replacement = "")    
}

shinyServer(function(input, output, session) {
    
    data <- reactiveValues(predictions = unigrams[1:5])
    
    observeEvent(input$usertext, {
        
        text <- unlist(str_split(string = input$usertext, pattern = "[\\.\\?\\!\\;]"))
        
        text <- as.character(tokens(x = tail(text, 1), what = "word", remove_numbers = TRUE, 
                                    remove_punct = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE))
        
        text <- char_tolower(tail(text, 4))
        
        text <- as.character(lapply(X = text, FUN = function(i){
            
            z <- vocab == i
            
            if(sum(z) == 0) {"zzz"}
            
            else {code[z]}
            
        }))
        
        text <- tail(c("sss", "sss", "sss", "sss", text), 4)
        
        text <- paste(text, collapse = "")
    
        temp <- n5grams$ngram[n5grams$history == text]
        
        temp <- head(temp, 10)
        
        predictions <- temp[temp != "zzz" & temp != "sss"]
        
        if (length(predictions) >= 5) {data$predictions <<- head(predictions, 5)}
        
        else {
            
            text <- rmprefix(text)
            
            temp <- n4grams$ngram[n4grams$history == text]
            
            temp <- head(temp, 10)
            
            temp <- temp[temp != "zzz" & temp != "sss"]
            
            predictions <- unique(c(predictions, temp))
            
            if (length(predictions) >= 5) {data$predictions <<- head(predictions, 5)}
            
            else {
                
                text <- rmprefix(text)
                
                temp <- n3grams$ngram[n3grams$history == text]
                
                temp <- head(temp, 10)
                
                temp <- temp[temp != "zzz" & temp != "sss"]
                
                predictions <- unique(c(predictions, temp))
                
                if (length(predictions) >= 5) {data$predictions <<- head(predictions, 5)}
                
                else {
                    
                    text <- rmprefix(text)
                    
                    temp <- n2grams$ngram[n2grams$history == text]
                    
                    temp <- head(temp, 10)
                    
                    temp <- temp[temp != "zzz" & temp != "sss"]
                    
                    predictions <- unique(c(predictions, temp))
                    
                    if (length(predictions) >= 5) {data$predictions <<- head(predictions, 5)}
                    
                    else {
                        
                        temp <- unigrams
                        
                        predictions <- unique(c(predictions, temp))
                        
                        data$predictions <<- head(predictions, 5)                    }
                    
                } 
            }
        }
        
        data$predictions <- as.character(lapply(X = 1 : 5, FUN = function(i){
            
            z <- str_which(string = n1grams$ngrams, pattern = data$predictions[i])
            
            n1grams$words[z]
            
        }))
        
        updateActionButton(session = session, inputId = "pred1", label = paste("1.", data$predictions[1]))
        
        updateActionButton(session = session, inputId = "pred2", label = paste("2.", data$predictions[2]))
        
        updateActionButton(session = session, inputId = "pred3", label = paste("3.", data$predictions[3]))
        
        updateActionButton(session = session, inputId = "pred4", label = paste("4.", data$predictions[4]))
        
        updateActionButton(session = session, inputId = "pred5", label = paste("5.", data$predictions[5]))
    })
    
    observeEvent(input$pred1,{
     
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[1]))
    })
    
    observeEvent(input$pred2,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[2]))
    })
    
    observeEvent(input$pred3,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[3]))
    })
    
    observeEvent(input$pred4,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[4]))
    })
    
    observeEvent(input$pred5,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[5]))
    })
    
    
})