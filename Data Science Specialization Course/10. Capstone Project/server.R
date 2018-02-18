library(shiny)
library(quanteda)
library(data.table)
library(stringr)

load("n1grams-final")

load("n2grams-final")

load("n3grams-final")

load("n4grams-final")

load("n5grams-final")

vocab <- c(n1grams$words)

unigrams <- c("the","and", "to", "a", "in", "of", "is", "for", "that", "on")

rmprefix <- function(x){
        
    str_replace(string = x, pattern = "^[a-z0-9]{3}", replacement = "")    
}

getpred <- function(x){
    
    str_extract(string = x, pattern = "[a-z0-9]{3}$")
    
}

shinyServer(function(input, output, session) {
    
    data <- reactiveValues(predictions = unigrams)
    
    observeEvent(input$usertext, {
        
        text <- unlist(str_split(string = input$usertext, pattern = "[\\.\\?\\!\\;]"))
        
        text <- as.character(tokens(x = tail(text, 1), what = "word", remove_numbers = TRUE, 
                                    remove_punct = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE))
        
        text <- char_tolower(text)
        
        text <- as.character(lapply(X = 1 : length(text), FUN = function(i){
            
            t <- paste("^", text[i], "$", sep = "")
            
            z <- str_which(string = n1grams$words, pattern = t)
            
            if(length(z) == 0) {"zzz"}
            
            else {n1grams$ngrams[z]}
            
        }))
        
        
        if(length(text) <= 4) {
            
            text <-c("sss", text)
            
            if(length(text) <= 4){
                
                text <-c("sss", text)
                
                if(length(text) <= 4){
                    
                    text <-c("sss", text)
                    
                    if(length(text) <= 4){
                        
                        text <-c("sss", text)
                        }
                    }
                }
            }
        
        text <- paste(text, collapse = " ")
        
        temp <- tokens(x = text, what = "word", n = 4, concatenator = "")$text1
        
        temp <- paste("^", tail(temp, 1), sep = "")
        
        temp <- n5grams[str_detect(string = ngrams, pattern = temp), prob]
        
        temp <- head(temp$ngrams, 15)
        
        temp <- as.character(lapply(temp, getpred))
        
        predictions <- temp[temp != "zzz" & temp != "sss"]
        
        if (length(predictions) >= 10) {data$predictions <<- head(predictions, 10)}
        
        else {
            
            temp <- tokens(x = text, what = "word", n = 3, concatenator = "")$text1
            
            temp <- paste("^", tail(temp, 1), sep = "")
            
            temp <- n4grams[str_detect(string = ngrams, pattern = temp), prob]
            
            temp <- head(temp$ngrams, 15)
            
            temp <- as.character(lapply(temp, getpred))
            
            temp <- temp[temp != "zzz" & temp != "sss"]
            
            predictions <- unique(c(predictions, temp))
            
            if (length(predictions) >= 10) {data$predictions <<- head(predictions, 10)}
            
            else {
                
                temp <- tokens(x = text, what = "word", n = 2, concatenator = "")$text1
                
                temp <- paste("^", tail(temp, 1), sep = "")
                
                temp <- n3grams[str_detect(string = ngrams, pattern = temp), prob]
                
                temp <- head(temp$ngrams, 15)
                
                temp <- as.character(lapply(temp, getpred))
                
                temp <- temp[temp != "zzz" & temp != "sss"]
                
                predictions <- unique(c(predictions, temp))
                
                if (length(predictions) >= 10) {data$predictions <<- head(predictions, 10)}
                
                else {
                    
                    temp <- tokens(x = text, what = "word", n = 1, concatenator = "")$text1
                    
                    temp <- paste("^", tail(temp, 1), sep = "")
                    
                    temp <- n2grams[str_detect(string = ngrams, pattern = temp), prob]
                    
                    temp <- head(temp$ngrams, 15)
                    
                    temp <- as.character(lapply(temp, getpred))
                    
                    temp <- temp[temp != "zzz" & temp != "sss"]
                    
                    predictions <- unique(c(predictions, temp))
                    
                    if (length(predictions) >= 10) {data$predictions <<- head(predictions, 10)}
                    
                    else {
                        
                        temp <- unigrams
                        
                        predictions <- unique(c(predictions, temp))
                        
                        data$predictions <<- head(predictions, 10)                    }
                    
                } 
            }
        }
        
        data$predictions <- as.character(lapply(X = 1 : 10, FUN = function(i){
            
            z <- str_which(string = n1grams$ngrams, pattern = data$predictions[i])
            
            n1grams$words[z]
            
        }))
        
        updateActionButton(session = session, inputId = "pred1", label = paste("1.", data$predictions[1]))
        
        updateActionButton(session = session, inputId = "pred2", label = paste("2.", data$predictions[2]))
        
        updateActionButton(session = session, inputId = "pred3", label = paste("3.", data$predictions[3]))
        
        updateActionButton(session = session, inputId = "pred4", label = paste("4.", data$predictions[4]))
        
        updateActionButton(session = session, inputId = "pred5", label = paste("5.", data$predictions[5]))
        
        updateActionButton(session = session, inputId = "pred6", label = paste("6.", data$predictions[6]))
        
        updateActionButton(session = session, inputId = "pred7", label = paste("7.", data$predictions[7]))
        
        updateActionButton(session = session, inputId = "pred8", label = paste("8.", data$predictions[8]))
        
        updateActionButton(session = session, inputId = "pred9", label = paste("9.", data$predictions[9]))
        
        updateActionButton(session = session, inputId = "pred10", label = paste("10.", data$predictions[10]))
        
        
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
    
    observeEvent(input$pred6,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[6]))
        
    })
    
    observeEvent(input$pred7,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[7]))
        
    })
    
    observeEvent(input$pred8,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[8]))
        
    })
    
    observeEvent(input$pred9,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[9]))
        
    })
    
    observeEvent(input$pred10,{
        
        updateTextInput(session = session, "usertext", value = paste(input$usertext, data$predictions[10]))
        
    })
})