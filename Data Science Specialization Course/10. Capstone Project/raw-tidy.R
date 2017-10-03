setwd("~/Portfolio/Data Science Specialization Course/10. Capstone Project/") ## Remove this line later.

library(data.table)
library(tm)
library(dplyr)
library(parallel)
library(stringi)
library(readr)

URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

download.file(url = URL, destfile = "swift.zip", method = "libcurl")

unzip(zipfile = "swift.zip")

dir.us<-paste(getwd(), "/final/en_US", sep = "")

setwd(dir.us)

raw <- list(1, 2, 3)

raw[1] <- paste(read_lines("en_US.blogs.txt", n_max = -1), sep = " ", collapse = "")

raw[2] <- paste(read_lines("en_US.news.txt", n_max = -1), sep = " ", collapse = "")

raw[3] <- paste(read_lines("en_US.twitter.txt", n_max = -1), sep = " ", collapse = "")

cl<-makeCluster(detectCores()-1)

clusterEvalQ(cl, c(library(tokenizers), library(stringi)))

tok.s <- parLapply(cl = cl, X = raw, fun = tokenize_sentences, lowercase = TRUE, strip_punctuation = TRUE, simplify = TRUE)

profanity <- c("^ass$", "ass(hole|face)", "cunts?", "bitche?(s|z)?", "sluts?", "jackass(es)?", "fuck(s|er|ing|ed)?", "bastards?", "^cocks?$", "shit[sz]?", "piss(ed|er)?", "wankers?", "bollocks?")

clusterExport(cl, "profanity")

tok.s <- parLapply(cl, tok.s, function(x){
    
    for(i in 1:length(profanity)) {
        
        x<-stri_replace_all_regex(str = x, pattern = profanity[i], replacement = "")
    }
    x
})


stopCluster(cl); rm(raw)






eng<-VCorpus(x = DirSource(directory = dir.us), readerControl = list(reader = readPlain, language = "en-US"))

description <- c("Blog posts", "News articles", "Twitter posts")

origin <- c("Misc. Blogs", "Newspapers", "Twitter.com")

i<-0

eng <- tm_map(eng, function(x) {
    
    i <<- i+1
    
    meta(x, tag = "author", type = "local") <- "Members of Public"
    
    meta(x, tag = "id", type = "local") <- i
    
    meta(x, tag = "description", type = "local") <- description[i]
    
    meta(x, tag = "origin", type = "local") <- origin[i]
    
    x
    
}, lazy = FALSE)

eng <- tm_map(eng, content_transformer(paste), sep = " ", collapse = "")

eng <-lapply(eng, function(x) {
    
    x[[1]]<-tokenize_sentences(x[[1]])
    
    x
})




tm_parLapply_engine(cl)

tm_parLapply_engine(new = NULL)



