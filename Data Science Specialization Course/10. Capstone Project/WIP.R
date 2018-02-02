library(quanteda)
library(parallel)
library(caret)
library(readr)
library(data.table)
library(stringr)
library(gtools)
library(dplyr)

data <- n2grams

discount <- D2

getprob.1 <- function(data, discount){
    
    data <- data[, -c("ngrams")]
    
    D <- discount
    
    cl <- makeCluster(detectCores()-1)
    
    clusterExport(cl, "D")
    
    result <- parApply(cl = cl, X = data, MARGIN = 1, FUN = function(x){
        
        prefix <- x[1]
        
        history.prefix <- x[2]
        
        history.suffix <- x[3]
        
        history.p.count <- x[4]
        
        backoff.prob <- x[5]
        
        (prefix - D) / history.prefix + (D * history.suffix + history.p.count) / history.prefix * backoff.prob
        
    })
    
    stopCluster(cl)
    
    result
}






