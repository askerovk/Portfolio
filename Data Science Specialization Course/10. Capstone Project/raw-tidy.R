URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

download.file(url = URL, destfile = "swift.zip", method = "libcurl")

unzip(zipfile = "swift.zip")

library(quanteda)
library(parallel)
library(caret)
library(readr)
library(data.table)
library(stringr)
library(gtools)

dir.us<-paste(getwd(), "/final/en_US", sep = "")

setwd(dir.us)

rm(dir.us)

fun1<- function(x){
    
    read_lines(x, locale = locale(encoding = "UTF-8")) %>% paste(sep = "", collapse = "") ->y ##Because some files read only halfway. 
    
    y
}

cl<-makeCluster(detectCores()-1)

clusterEvalQ(cl, c(library(readr), library(quanteda), library(dplyr)))

clusterExport(cl, "fun1")

tok.s <- parLapply(cl = cl, X = c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt"), fun = fun1)

tok.s <- parLapply(cl = cl, X = tok.s, fun = tokens, what = "sentence")

stopCluster(cl)

setwd(str_replace(string = getwd(), pattern = "/final/en_US", replacement = ""))

rm(fun1)

source <- c(rep(1, length(tok.s[[1]][[1]])), 
            rep(2, length(tok.s[[2]][[1]])), 
            rep(3, length(tok.s[[3]][[1]])))



tok.s <- c(tok.s[[1]][[1]], tok.s[[2]][[1]], tok.s[[3]][[1]])

tok.s <- data.table(text = unlist(tok.s), source = source)

rm(source)

set.seed(768943)

Intrain <- createDataPartition(y = tok.s$source, p = 0.6, list = FALSE) ##Sample from all 3 corpuses

train <- tok.s[Intrain, 1]

test <- tok.s[-Intrain, 1]

test<- char_tolower(test$text)

write(x = test, file = "test.txt")

rm(Intrain, tok.s, test)

train <- char_tolower(train$text)

write(x = train, file = "train.txt")

###############################################
    
temp <- tokens(x = paste(train, collapse = " "), what = "word", remove_punct = TRUE, remove_hyphens = TRUE)

n1grams <- dfm(temp)

rm(temp)

n1grams <- data.table(words = as.character(colnames(n1grams)), count = colSums(n1grams))

profanity <- c("^ass(holes?|face|es|munchers?)?$", "^buttholes?$", "^cunts?$", "^cumm?(ing|shots?|stains?)?$", 
               "^bitche?(s|z)?$", "^sluts?(bags?)$", "^jackass?(es)?$", "^fuck(s|er|ing|ed(up)?|tards?|heads?)$]?", 
               "^bastards?$", "^cock(s|block(s|ers?)?|suckers?)?$", "^whores?$", "^shit(s|z|heads?)?$", 
               "^piss?(ed|er)?$", "^wankers?$", "^bollocks?$", "^turds?$", "^faggots?$", "^nigg?ers?$", 
               "^gooks?$", "^dick(s|heads?|rid(e|ing)?|wads?)?$", "^boob(s|ies)?$")

z <- c(0)

invisible(lapply(X = 1:length(profanity), FUN = function(i){
    
    x <- str_which(string = n1grams$words, pattern = profanity[i])
    
    z <<- c(z, x)
}))

z <- z[-1]

n1grams<-n1grams[-z]

rm(profanity, z)

n1grams <- n1grams[-str_which(string = n1grams$words, pattern = "[^a-z//']+"),]

n1grams <- n1grams[order(count, decreasing = TRUE),]

temp <- n1grams[!(n1grams$words %in% stopwords())]

table <- data.table(i = 0, ratio.count = 0, ratio.length = 0)

invisible(lapply(X = 1:100, FUN = function(i){
    
    z <- sum(temp$count[temp$count >= i]) / sum(temp$count)
    
    l <- length(temp$count[temp$count >= i]) / length(temp$count)
    
    table <<- rbind(table, data.table(i = i, ratio.count = z, ratio.length = l))
    
}))

rm(temp, table)

n1grams <- n1grams[count >= 32]

train <- as.character(lapply(X = train, function(x){
    
    paste("ZdZstrYzX", x, "ZdZendYzX")
    
}))

wordcode <- permutations(n = 36, r = 3, v = c(letters, 0:9))

wordcode <- apply(X = wordcode, MARGIN = 1, paste, collapse = "")

n1grams <- n1grams[, .(words,  ngrams = wordcode[1:length(n1grams$words)], count)]
    
n1grams <- rbind(n1grams, data.table(words = "ZdZendYzX", ngrams = "eee", count = length(train)))

n1grams <- rbind(n1grams, data.table(words = "ZdZstrYzX", ngrams = "sss", count = length(train)))

n1grams <- rbind(n1grams, data.table(words = "UNK", ngrams = "zzz", count = 0))

rm(wordcode)

t <- length(train)

train <- list(train[1 : round(t/3)], train[round(t/3+1) : round(t*2/3)], train[round(t*2/3+1) : t])

train <- lapply(X = train, FUN = function(x){
    
    as.character(tokens(x = paste(x, collapse = " "), what = "word",  remove_numbers = TRUE, 
                        remove_punct = TRUE, remove_hyphens = TRUE))
    
    
})

tocode <- function(data){
    
    l <- length(data)
    
    cl<-makeCluster(detectCores()-1)
    
    clusterExport(cl, c("data", "n1grams"))
    
    clusterEvalQ(cl, library(stringr))
    
    result <- parLapply(cl = cl, X = list(data[1 : round(l/3)], 
                                          data[round(l/3+1) : round(l*2/3)], 
                                          data[round(l*2/3+1) : l]), fun = function(x){
                                              
                                              words <- n1grams$words
                                              
                                              ngrams <- n1grams$ngrams
                                              
                                              lapply(x, function(i){
                                                  
                                                  ngrams[words == i]
                                                  
                                              })
                                          })
    
    
    stopCluster(cl)
    
    result <- as.character(c(result[[1]], result[[2]], result[[3]]))
    
    result[result == "character(0)"] <- "zzz"
    
    paste(result, collapse = " ")
}

train <- lapply(X = train, FUN = tocode)

write(as.character(train), file = "codetrain.txt")

my.dfm <- function(data, n) {
    
    ngrams <- tokens(x = data, what = "word", ngrams = n, concatenator = "")
    
    ngrams <- dfm(ngrams)
    
    data.table(ngrams = as.character(colnames(ngrams)), count = colSums(ngrams))
}
 
n1grams.a1 <- my.dfm(data = train[[1]], n = 1)

n1grams.a2 <- my.dfm(data = train[[2]], n = 1)

n1grams.a3 <- my.dfm(data = train[[3]], n = 1)

n1grams.a <- merge(x = n1grams.a1, y = n1grams.a2, by = "ngrams", all = TRUE)

n1grams.a <- merge(x = n1grams.a, y = n1grams.a3, by = "ngrams", all = TRUE)

rm(n1grams.a1, n1grams.a2, n1grams.a3)

names(n1grams.a) <- c("ngrams", "count1", "count2", "count3")

n1grams.a$count1[is.na(n1grams.a$count1)] <- 0

n1grams.a$count2[is.na(n1grams.a$count2)] <- 0

n1grams.a$count3[is.na(n1grams.a$count3)] <- 0

n1grams.a <- n1grams.a[, .(ngrams, count = rowSums(x = n1grams.a[, -1]))]

n1grams <- merge(x = n1grams, y = n1grams.a, by = "ngrams", all.y = TRUE)

n1grams <- n1grams[, -3]

names(n1grams) <- c("ngrams", "words", "count")

rm(n1grams.a)

write.csv(n1grams, "n1grams.csv")

rm(n1grams)

######################################################
train <- lapply(train, function(x){
    
    str_replace_all(string = x, pattern = "sss", "sss sss")
    
})

n2grams.1 <- my.dfm(data = train[[1]], n = 2)

n2grams.2 <- my.dfm(data = train[[2]], n = 2)

n2grams.3 <- my.dfm(data = train[[3]], n = 2)

n2grams <- merge(x = n2grams.1, y = n2grams.2, by = "ngrams", all = TRUE)

n2grams <- merge(x = n2grams, y = n2grams.3, by = "ngrams", all = TRUE)

rm(n2grams.1, n2grams.2, n2grams.3)

names(n2grams) <- c("ngrams", "count1", "count2", "count3")

n2grams$count1[is.na(n2grams$count1)] <- 0

n2grams$count2[is.na(n2grams$count2)] <- 0

n2grams$count3[is.na(n2grams$count3)] <- 0

n2grams <- n2grams[, .(ngrams, count = rowSums(x = n2grams[, -1]))]

z <- str_which(string = n2grams$ngrams, pattern = "eee")

n2grams <- n2grams[-z]

D2 <- sum(n2grams$count == 1) / (sum(n2grams$count == 1) + 2 * sum(n2grams$count == 2))

write.csv(n2grams, "n2grams.csv")

rm(n2grams)

##########################################################

train <- lapply(train, function(x){
    
    str_replace_all(string = x, pattern = "sss sss", "sss sss sss")
    
})

n3grams.1 <- my.dfm(data = train[[1]], n = 3)

n3grams.2 <- my.dfm(data = train[[2]], n = 3)

n3grams.3 <- my.dfm(data = train[[3]], n = 3)

n3grams <- merge(x = n3grams.1, y = n3grams.2, by = "ngrams", all = TRUE)

n3grams <- merge(x = n3grams, y = n3grams.3, by = "ngrams", all = TRUE)

rm(n3grams.1, n3grams.2, n3grams.3)

names(n3grams) <- c("ngrams", "count1", "count2", "count3")

n3grams$count1[is.na(n3grams$count1)] <- 0

n3grams$count2[is.na(n3grams$count2)] <- 0

n3grams$count3[is.na(n3grams$count3)] <- 0

n3grams <- n3grams[, .(ngrams, count = rowSums(x = n3grams[, -1]))]

z <- str_which(string = n3grams$ngrams, pattern = "eee")

n3grams <- n3grams[-z]

rm(z)

D3 <- sum(n3grams$count == 1) / (sum(n3grams$count == 1) + 2 * sum(n3grams$count == 2))

write.csv(n3grams, "n3grams.csv")

rm(n3grams)

###################################################################

train <- lapply(train, function(x){
    
    str_replace_all(string = x, pattern = "sss sss sss", "sss sss sss sss")
    
})

n4grams.1 <- my.dfm(data = train[[1]], n = 4)

n4grams.2 <- my.dfm(data = train[[2]], n = 4)

n4grams.3 <- my.dfm(data = train[[3]], n = 4)

n4grams <- merge(x = n4grams.1, y = n4grams.2, by = "ngrams", all = TRUE)

rm(n4grams.1, n4grams.2)

names(n4grams) <- c("ngrams", "count1", "count2")

n4grams$count1[is.na(n4grams$count1)] <- 0

n4grams$count2[is.na(n4grams$count2)] <- 0

n4grams <- n4grams[, .(ngrams, count = rowSums(x = n4grams[, -1]))]

z <- str_which(string = n4grams$ngrams, pattern = "eee")

n4grams <- n4grams[-z]

rm(z)

gc()

n4grams <- merge(x = n4grams, y = n4grams.3, by = "ngrams", all = TRUE)

rm(n4grams.3)

gc()

names(n4grams) <- c("ngrams", "count1", "count2")

n4grams$count1[is.na(n4grams$count1)] <- 0

n4grams$count2[is.na(n4grams$count2)] <- 0

n4grams <- data.table(ngrams = n4grams$ngrams, count = rowSums(x = n4grams[, -1]))

z <- str_which(string = n4grams$ngrams, pattern = "eee")

n4grams <- n4grams[-z]

D4 <- sum(n4grams$count == 1) / (sum(n4grams$count == 1) + 2 * sum(n4grams$count == 2))

write.csv(n4grams, "n4grams.csv")

rm(n4grams, z)

########################################################################

train <- lapply(train, function(x){
    
    str_replace_all(string = x, pattern = "sss sss sss sss", "sss sss sss sss sss")
    
})

n5grams.1 <- my.dfm(data = train[[1]], n = 5)

z <- str_which(string = n5grams.1$ngrams, pattern = "eee")

n5grams.1 <- n5grams.1[-z]

n5grams.2 <- my.dfm(data = train[[2]], n = 5)

z <- str_which(string = n5grams.2$ngrams, pattern = "eee")

n5grams.2 <- n5grams.2[-z]

gc()

n5grams <- merge(x = n5grams.1, y = n5grams.2, by = "ngrams", all = TRUE)

rm(n5grams.1, n5grams.2)

gc()

names(n5grams) <- c("ngrams", "count1", "count2")

n5grams$count1[is.na(n5grams$count1)] <- 0

n5grams$count2[is.na(n5grams$count2)] <- 0

n5grams <- n5grams[, .(ngrams, count = rowSums(x = n5grams[, -1]))]

gc()

n5grams.3 <- my.dfm(data = train[[3]], n = 5)

rm(train)

z <- str_which(string = n5grams.3$ngrams, pattern = "eee")

n5grams.3 <- n5grams.3[-z]

n5grams <- merge(x = n5grams, y = n5grams.3, by = "ngrams", all = TRUE)

rm(n5grams.3)

gc()

names(n5grams) <- c("ngrams", "count1", "count2")

l <- length(n5grams$ngrams)

n5grams[1 : round(l/2), 2][is.na(n5grams[1 : round(l/2), 2])] <- 0

n5grams[round(l/2+1) : l, 2][is.na(n5grams[round(l/2+1) : l, 2])] <- 0

n5grams[1 : round(l/2), 3][is.na(n5grams[1 : round(l/2), 3])] <- 0

n5grams[round(l/2+1) : l, 3][is.na(n5grams[round(l/2+1) : l, 3])] <- 0

gc()

temp <- rowSums(x = n5grams[, -1])

n5grams <- n5grams[, 1]

n5grams <- data.table(ngrams = n5grams$ngrams, count = temp)

rm(temp, t, l, z)

write.csv(n5grams, "n5grams.csv")

D5 <- sum(n5grams$count == 1) / (sum(n5grams$count == 1) + 2 * sum(n5grams$count == 2))

############################## Generating N4 Prefix and Suffix ####################################
rmprefix <- function(x){
    
    str_replace(string = x, pattern = "^[a-z0-9]{3}", replacement = "")
}

rmsuffix <- function(x){
    
    str_replace(string = x, pattern = "[a-z0-9]{3}$", replacement = "")
}

ngrams <- n5grams$ngrams

rm(n5grams)

l <- length(ngrams)

ngrams <- list(ngrams[1 : round(l/4)], 
                ngrams[(round(l/4) + 1) : round(l*2/4)], 
                ngrams[(round(l*2/4) + 1) : round(l*3/4)],
                ngrams[(round(l*3/4) + 1) : l])

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

getaddon <- function(data){
    
    cl <- makeCluster(detectCores()-1)
    
    l <- length(data)
    
    data <- list(data[1 : round(l/3)], 
                 data[(round(l/3)+1) : round(l*2/3)], 
                 data[(round(l*2/3)+1) : l])
    
    clusterExport(cl =cl, varlist = c("data", "rmprefix", "rmsuffix"))
    
    clusterEvalQ(cl = cl, expr = library(stringr))
    
    prefix <- parLapply(cl = cl, X = data, fun = function(x){
        
        lapply(X = x, FUN = rmprefix)
        
    })
    
    prefix <- paste(unlist(prefix), collapse = " ")
    
    suffix <- parLapply(cl = cl, X = data, fun = function(x){
        
        lapply(X = x, FUN = rmsuffix)
        
    })
    
    stopCluster(cl)
    
    rm(data)
    
    suffix <- paste(unlist(suffix), collapse = " ")
    
    c(prefix, suffix)
}

raw.1 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.2 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.3 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

raw.4 <- getaddon(temp)

rm(temp, ngrams)

n5prefix <- paste(raw.1[[1]], raw.2[[1]], raw.3[[1]], raw.4[[1]], collapse = " ")

n5suffix <- paste(raw.1[[2]], raw.2[[2]], raw.3[[2]], raw.4[[2]], collapse = " ")

rm(raw.1, raw.2, raw.3, raw.4)

save(n5prefix, file = "n5prefix")

save(n5suffix, file = "n5suffix")

rm(n5suffix)

n5prefix <- unlist(str_split(string = n5prefix, pattern = " "))

n5prefix <- data.table(count = rep(x = 1, time = l), ngrams = n5prefix)

prefix.1 <- n5prefix[1 : round(l/2),]

prefix.2 <- n5prefix[(round(l/2)+1) : l,]

rm(n5prefix)

save(prefix.2, file = "prefix.2")

rm(prefix.2)

gc()

prefix.1 <- group_by(prefix.1, ngrams)

prefix.1 <- summarise(prefix.1, prefix = sum(count))

prefix.1 <- data.table(prefix.1)

save(prefix.1, file = "prefix.1")

rm(prefix.1)

load("prefix.2")

prefix.2 <- group_by(prefix.2, ngrams)

prefix.2 <- summarise(prefix.2, prefix = sum(count))

prefix.2 <- data.table(prefix.2)

load("prefix.1")

prefix.3 <- merge(x = prefix.1, y = prefix.2, by = "ngrams", all = TRUE)

rm(prefix.1, prefix.2)

gc()

prefix.3$prefix.x[is.na(prefix.3$prefix.x)] <- 0 

prefix.3$prefix.y[is.na(prefix.3$prefix.y)] <- 0

gc()

prefix.3 <- data.table(ngrams = prefix.3$ngrams, prefix = rowSums(prefix.3[, -1]))

save(prefix.3, file = "prefix.3")

rm(prefix.3)

load("n5suffix")

n5suffix <- as.character(tokens(n5suffix, "fastestword"))

n5suffix <- data.table(count = rep(x = 1, time = l), ngrams = n5suffix)

suffix.1 <- n5suffix[1 : round(l/2),]

suffix.2 <- n5suffix[(round(l/2)+1) : l,]

rm(n5suffix)

save(suffix.2, file = "suffix.2")

rm(suffix.2)

gc()

suffix.1 <- group_by(suffix.1, ngrams)

suffix.1 <- summarise(suffix.1, suffix = sum(count))

suffix.1 <- data.table(suffix.1)

save(suffix.1, file = "suffix.1")

rm(suffix.1)

load("suffix.2")

suffix.2 <- group_by(suffix.2, ngrams)

suffix.2 <- summarise(suffix.2, suffix = sum(count))

suffix.2 <- data.table(suffix.2)

load("suffix.1")

suffix.3 <- merge(x = suffix.1, y = suffix.2, by = "ngrams", all = TRUE)

rm(suffix.1, suffix.2)

suffix.3$suffix.x[is.na(suffix.3$suffix.x)] <- 0 

suffix.3$suffix.y[is.na(suffix.3$suffix.y)] <- 0

gc()

suffix.3 <- data.table(ngrams = suffix.3$ngrams, suffix = rowSums(suffix.3[, .(suffix.x, suffix.y)]))

save(suffix.3, file = "suffix.3")

load("prefix.3")

n4addon <- merge(x = prefix.3, y = suffix.3, by = "ngrams", all = TRUE)

rm(prefix.3, suffix.3)

n4addon$prefix[is.na(n4addon$prefix)] <- 0

n4addon$suffix[is.na(n4addon$suffix)] <- 0

save(n4addon, file = "n4addon")

rm(n4addon)

load("n3addon.1")

load("n3addon.2")

n3addon <- merge(x = n3addon.1, y = n3addon.2, by = "ngrams", all = TRUE)

rm(n3addon.1, n3addon.2)

n3addon$prefix.x[is.na(n3addon$prefix.x)] <- 0 

n3addon$prefix.y[is.na(n3addon$prefix.y)] <- 0 

n3addon <- data.table(ngrams = n3addon$ngrams, prefix = rowSums(n3addon[, -1]))

temp <- lapply(X = n3addon$ngrams, FUN = rmsuffix)

n3addon$ngrams <- as.character(temp)

rm(temp)

n3addon <- group_by(n3addon, ngrams)

n3addon <- data.table(summarise(n3addon, prefix = sum(prefix)))

save(n3addon, file = "n3addon")

temp <- lapply(X = n3addon$ngrams, FUN = rmsuffix)

n3addon$ngrams <- as.character(temp)

rm(temp)

n2addon <- group_by(n3addon, ngrams)

rm(n3addon)

n2addon <- data.table(summarise(n2addon, prefix = sum(prefix)))

save(n2addon, file = "n2addon")

temp <- lapply(X = n2addon$ngrams, FUN = rmsuffix)

n2addon$ngrams <- as.character(temp)

rm(temp)

n1addon <- group_by(n2addon, ngrams)

rm(n2addon)

n1addon <- data.table(summarise(n1addon, prefix = sum(prefix)))

save(n1addon, file = "n1addon")

rm(n1addon)

load("n5grams")

load("n4addon")

history <- function(data) {
    
    cl <- makeCluster(detectCores()-1)
    
    l <- length(data)
    
    data <- list(data[1 : round(l/3)], 
                 data[(round(l/3)+1) : round(l*2/3)], 
                 data[(round(l*2/3)+1) : l])
    
    clusterExport(cl =cl, varlist = c("data", "rmsuffix"))
    
    clusterEvalQ(cl = cl, expr = library(stringr))
    
    history <- parLapply(cl = cl, X = data, fun = function(x){
        
        lapply(X = x, FUN = rmsuffix)
        
    })
    
    stopCluster(cl)
    
    as.character(unlist(history))
}

temp <- history(n5grams$ngrams)

n5grams <- data.table(n5grams, history = temp)

rm(temp)

n5grams <- merge(x = n5grams, y = n4addon[, -c("prefix")], by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n5grams) <- c("history", "ngrams", "count", "history.suffix", "history.p.count")

n4addon <- n4addon[, c("ngrams", "prefix")]

n4grams <- fread("backup/n4grams.csv")[, -1]

n5grams <- merge(x = n5grams, y = n4grams, by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n5grams) <- c("history", "ngrams", "count", "history.suffix", "history.p.count", "history.count")

n5grams <- n5grams[, c("ngrams", "count", "history.count", "history.suffix", "history.p.count")]

save(n5grams, file = "n5grams")

rm(n5grams)

n4grams <- merge(x = n4grams, y = n4addon, by = "ngrams", all = TRUE)

rm(n4addon)

######################################## Generating N3 Prefix and Suffix###########################

z <- str_which(string = n4grams$ngrams, pattern = "^sss")

n4grams$prefix[z] <- n4grams$count[z]

rm(z)

count <- n4grams[, -c("ngrams")]

save(count, file = "count")

rm(count)

ngrams <- n4grams$ngrams

n4grams <- n4grams[count >=3, ]

n4grams <- n4grams[, -c("count")]

save(n4grams, file = "n4grams")

rm(n4grams)

l <- length(ngrams)

ngrams <- list(ngrams[1 : round(l/4)], 
               ngrams[(round(l/4) + 1) : round(l*2/4)], 
               ngrams[(round(l*2/4) + 1) : round(l*3/4)],
               ngrams[(round(l*3/4) + 1) : l])

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.1 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.2 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.3 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

raw.4 <- getaddon(temp)

rm(temp, ngrams)

prefix <- paste(raw.1[[1]], raw.2[[1]], raw.3[[1]], raw.4[[1]], collapse = " ")

suffix <- paste(raw.1[[2]], raw.2[[2]], raw.3[[2]], raw.4[[2]], collapse = " ")

rm(raw.1, raw.2, raw.3, raw.4)

save(prefix, file = "prefix")

save(suffix, file = "suffix")

rm(suffix)

load("count")

prefix <- unlist(str_split(string = prefix, pattern = " "))

prefix <- data.table(ngrams = prefix, count = count$count, prefix = count$prefix)

rm(count)

l <- length(prefix$count)

prefix.1 <- prefix[1 : round(l/2),]

prefix.2 <- prefix[(round(l/2)+1) : l,]

rm(prefix)

save(prefix.2, file = "prefix.2")

rm(prefix.2)

gc()

prefix.1 <- group_by(prefix.1, ngrams)

prefix.1 <- summarise(prefix.1, count1 = sum(count <= 2 & prefix ==1), 
                      count2 = sum(count <= 2 & prefix == 2 ), "count3+" = sum(count >= 3))

n2addon.1 <- data.table(ngrams = prefix.1$ngrams, prefix = prefix.1$count2)

n2addon.1 <- n2addon.1[prefix != 0]

save(n2addon.1, file = "n2addon.1")

rm(n2addon.1)

prefix.1$count2 <- prefix.1$count2 * 2

prefix.1 <- data.table(ngrams = prefix.1$ngrams, prefix = rowSums(prefix.1[, -1]))

save(prefix.1, file = "prefix.1")

rm(prefix.1)

load("prefix.2")

prefix.2 <- group_by(prefix.2, ngrams)

prefix.2 <- summarise(prefix.2, count1 = sum(count <= 2 & prefix ==1), 
                      count2 = sum(count <= 2 & prefix == 2 ), "count3+" = sum(count >= 3))

n2addon.2 <- data.table(ngrams = prefix.2$ngrams, prefix = prefix.2$count2)

n2addon.2 <- n2addon.2[prefix != 0]

save(n2addon.2, file = "n2addon.2")

rm(n2addon.2)

prefix.2$count2 <- prefix.2$count2 * 2

prefix.2 <- data.table(ngrams = prefix.2$ngrams, prefix = rowSums(prefix.2[, -1]))

load("prefix.1")

prefix.3 <- merge(x = prefix.1, y = prefix.2, by = "ngrams", all = TRUE)

rm(prefix.1, prefix.2)

gc()

prefix.3$prefix.x[is.na(prefix.3$prefix.x)] <- 0 

prefix.3$prefix.y[is.na(prefix.3$prefix.y)] <- 0

gc()

prefix.3 <- data.table(ngrams = prefix.3$ngrams, prefix = rowSums(prefix.3[, -1]))

save(prefix.3, file = "prefix.3")

rm(prefix.3)

load("suffix")

load("count")

suffix <- unlist(str_split(string = suffix, pattern = " "))

suffix <- data.table(ngrams = suffix, count = count$count, prefix = count$prefix)

rm(count)

l <- length(suffix$count)

suffix.1 <- suffix[1 : round(l/2),]

suffix.2 <- suffix[(round(l/2)+1) : l,]

rm(suffix)

save(suffix.2, file = "suffix.2")

rm(suffix.2)

gc()

suffix.1 <- group_by(suffix.1, ngrams)

suffix.1 <-  summarise(suffix.1, count1 = sum(count <= 2 & prefix ==1), 
                       count2 = sum(count <= 2 & prefix == 2 ), "count3+" = sum(count >= 3))

p.count <- suffix.1$count1 + suffix.1$count2*2

suffix.1 <- data.table(ngrams = suffix.1$ngrams, suffix = rowSums(suffix.1[, -1]), p.count = p.count)

save(suffix.1, file = "suffix.1")

rm(suffix.1)

load("suffix.2")

suffix.2 <- group_by(suffix.2, ngrams)

suffix.2 <-  summarise(suffix.2, count1 = sum(count <= 2 & prefix ==1), 
                       count2 = sum(count <= 2 & prefix == 2 ), "count3+" = sum(count >= 3))

p.count <- suffix.2$count1 + suffix.2$count2*2

suffix.2 <- data.table(ngrams = suffix.2$ngrams, suffix = rowSums(suffix.2[, -1]), p.count = p.count)

rm(p.count)

load("suffix.1")

suffix.3 <- merge(x = suffix.1, y = suffix.2, by = "ngrams", all = TRUE)

rm(suffix.1, suffix.2)

suffix.3$suffix.x[is.na(suffix.3$suffix.x)] <- 0 

suffix.3$suffix.y[is.na(suffix.3$suffix.y)] <- 0

suffix.3$p.count.x[is.na(suffix.3$p.count.x)] <- 0

suffix.3$p.count.y[is.na(suffix.3$p.count.y)] <- 0

gc()

suffix.3 <- data.table(ngrams = suffix.3$ngrams, suffix = rowSums(suffix.3[, .(suffix.x, suffix.y)]),
                       p.count =  rowSums(suffix.3[, .(p.count.x, p.count.y)]))

save(suffix.3, file = "suffix.3")

rm(suffix.3)

load("n3addon")

load("prefix.3")

n3addon <- merge(x = n3addon, y = prefix.3, by = "ngrams", all = TRUE)

rm(prefix.3)

n3addon$prefix.x[is.na(n3addon$prefix.x)] <- 0

n3addon$prefix.y[is.na(n3addon$prefix.y)] <- 0

n3addon <- data.table(ngrams = n3addon$ngrams, prefix = rowSums(n3addon[, -1]))

load("suffix.3")

n3addon <- merge(x = n3addon, y = suffix.3, by = "ngrams", all = TRUE)

rm(suffix.3)

n3addon$prefix[is.na(n3addon$prefix)] <- 0

n3addon$suffix[is.na(n3addon$suffix)] <- 0

n3addon$p.count[is.na(n3addon$p.count)] <- 0

save(n3addon, file = "n3addon")

rm(n3addon)

load("n2addon.1")

load("n2addon.2")

n2addon.3 <- merge(x = n2addon.1, y = n2addon.2, by = "ngrams", all = TRUE)

rm(n2addon.1, n2addon.2)

n2addon.3$prefix.x[is.na(n2addon.3$prefix.x)] <- 0 

n2addon.3$prefix.y[is.na(n2addon.3$prefix.y)] <- 0 

n2addon.3 <- data.table(ngrams = n2addon.3$ngrams, prefix = rowSums(n2addon.3[, -1]))

temp <- lapply(X = n2addon.3$ngrams, FUN = rmsuffix)

n2addon.3$ngrams <- as.character(temp)

rm(temp)

n2addon.3 <- group_by(n2addon.3, ngrams)

n2addon.3 <- data.table(summarise(n2addon.3, prefix = sum(prefix)))

load("n2addon")

n2addon <- merge(x = n2addon, y = n2addon.3, by = "ngrams", all = TRUE)

n2addon$prefix.x[is.na(n2addon$prefix.x)] <- 0 

n2addon$prefix.y[is.na(n2addon$prefix.y)] <- 0 

n2addon <- data.table(ngrams = n2addon$ngrams, prefix = rowSums(n2addon[, -1]))

save(n2addon, file = "n2addon")

rm(n2addon)

temp <- lapply(X = n2addon.3$ngrams, FUN = rmsuffix)

n2addon.3$ngrams <- as.character(temp)

rm(temp)

n1addon.1 <- group_by(n2addon.3, ngrams)

rm(n2addon.3)

n1addon.1<- data.table(summarise(n1addon.1, prefix = sum(prefix)))

load("n1addon")

n1addon <- merge(x = n1addon, y = n1addon.1, by = "ngrams", all = TRUE)

n1addon$prefix.x[is.na(n1addon$prefix.x)] <- 0 

n1addon$prefix.y[is.na(n1addon$prefix.y)] <- 0 

n1addon <- data.table(ngrams = n1addon$ngrams, prefix = rowSums(n1addon[, -1]))

save(n1addon, file = "n1addon")

rm(n1addon, n1addon.1)

load("n3addon")

load("n4grams")

temp <- history(n4grams$ngrams)

n4grams <- data.table(n4grams, history = temp)

rm(temp)

n4grams <- merge(x = n4grams, y = n3addon[, c("ngrams", "suffix", "p.count")], by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n4grams) <- c("history", "ngrams", "prefix", "history.suffix", "history.p.count")

n3addon <- n3addon[, c("ngrams", "prefix")]

n3grams <- fread("backup/n3grams.csv")[, -1]

n3grams <- merge(x = n3grams, y = n3addon, by = "ngrams", all = TRUE)

z <- str_which(string = n3grams$ngrams, pattern = "^sss")

n3grams$prefix[z] <- n3grams$count[z]

rm(z)

rm(n3addon)

n4grams <- merge(x = n4grams, y = n3grams, by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n4grams) <- c("history", "ngrams", "prefix", "history.suffix", "history.p.count", "count", "history.prefix")

n4grams <- n4grams[, c("ngrams", "prefix", "history.prefix", "history.suffix", "history.p.count")]

save(n4grams, file = "n4grams")

rm(n4grams)

#################################N2 addon ####################################
count <- n3grams[, -c("ngrams")]

save(count, file = "count")

rm(count)

ngrams <- n3grams$ngrams

n3grams <- n3grams[count >=3, ]

n3grams <- n3grams[, -c("count")]

save(n3grams, file = "n3grams")

rm(n3grams)

l <- length(ngrams)

ngrams <- list(ngrams[1 : round(l/4)], 
               ngrams[(round(l/4) + 1) : round(l*2/4)], 
               ngrams[(round(l*2/4) + 1) : round(l*3/4)],
               ngrams[(round(l*3/4) + 1) : l])

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.1 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.2 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.3 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

raw.4 <- getaddon(temp)

rm(temp, ngrams)

prefix <- paste(raw.1[[1]], raw.2[[1]], raw.3[[1]], raw.4[[1]], collapse = " ")

suffix <- paste(raw.1[[2]], raw.2[[2]], raw.3[[2]], raw.4[[2]], collapse = " ")

rm(raw.1, raw.2, raw.3, raw.4)

save(prefix, file = "prefix")

save(suffix, file = "suffix")

rm(suffix)

load("count")

prefix <- unlist(str_split(string = prefix, pattern = " "))

prefix <- data.table(ngrams = prefix, count = count$count, prefix = count$prefix)

rm(count)

l <- length(prefix$count)

prefix.1 <- prefix[1 : round(l/2),]

prefix.2 <- prefix[(round(l/2)+1) : l,]

rm(prefix)

save(prefix.2, file = "prefix.2")

rm(prefix.2)

gc()

prefix.1 <- group_by(prefix.1, ngrams)

prefix.1 <- summarise(prefix.1, count1 = sum(count <= 2 & prefix ==1), 
                      count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                      "count4+" = sum(count >= 3))

n1addon.1 <- data.table(ngrams = prefix.1$ngrams, prefix = prefix.1$count2 + prefix.1$count3*2)

n1addon.1 <- n1addon.1[prefix != 0]

save(n1addon.1, file = "n1addon.1")

rm(n1addon.1)

prefix.1$count2 <- prefix.1$count2 * 2

prefix.1$count3 <- prefix.1$count3 * 3

prefix.1 <- data.table(ngrams = prefix.1$ngrams, prefix = rowSums(prefix.1[, -1]))

save(prefix.1, file = "prefix.1")

rm(prefix.1)

load("prefix.2")

prefix.2 <- group_by(prefix.2, ngrams)

prefix.2 <- summarise(prefix.2, count1 = sum(count <= 2 & prefix ==1), 
                      count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                      "count4+" = sum(count >= 3))

n1addon.2 <- data.table(ngrams = prefix.2$ngrams, prefix = prefix.2$count2 + prefix.2$count3*2)

n1addon.2 <- n1addon.2[prefix != 0]

save(n1addon.2, file = "n1addon.2")

rm(n1addon.2)

prefix.2$count2 <- prefix.2$count2 * 2

prefix.2$count3 <- prefix.2$count3 * 3

prefix.2 <- data.table(ngrams = prefix.2$ngrams, prefix = rowSums(prefix.2[, -1]))

load("prefix.1")

prefix.3 <- merge(x = prefix.1, y = prefix.2, by = "ngrams", all = TRUE)

rm(prefix.1, prefix.2)

gc()

prefix.3$prefix.x[is.na(prefix.3$prefix.x)] <- 0 

prefix.3$prefix.y[is.na(prefix.3$prefix.y)] <- 0

gc()

prefix.3 <- data.table(ngrams = prefix.3$ngrams, prefix = rowSums(prefix.3[, -1]))

save(prefix.3, file = "prefix.3")

rm(prefix.3)

load("suffix")

load("count")

suffix <- unlist(str_split(string = suffix, pattern = " "))

suffix <- data.table(ngrams = suffix, count = count$count, prefix = count$prefix)

rm(count)

l <- length(suffix$count)

suffix.1 <- suffix[1 : round(l/2),]

suffix.2 <- suffix[(round(l/2)+1) : l,]

rm(suffix)

save(suffix.2, file = "suffix.2")

rm(suffix.2)

gc()

suffix.1 <- group_by(suffix.1, ngrams)

suffix.1 <-  summarise(suffix.1, count1 = sum(count <= 2 & prefix ==1), 
                       count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                       "count4+" = sum(count >= 3))

p.count <- suffix.1$count1 + suffix.1$count2*2 + suffix.1$count3*3

suffix.1 <- data.table(ngrams = suffix.1$ngrams, suffix = rowSums(suffix.1[, -1]), p.count = p.count)

save(suffix.1, file = "suffix.1")

rm(suffix.1)

load("suffix.2")

suffix.2 <- group_by(suffix.2, ngrams)

suffix.2 <-  summarise(suffix.2, count1 = sum(count <= 2 & prefix ==1), 
                       count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                       "count4+" = sum(count >= 3))

p.count <- suffix.2$count1 + suffix.2$count2*2 + suffix.2$count3*3

suffix.2 <- data.table(ngrams = suffix.2$ngrams, suffix = rowSums(suffix.2[, -1]), p.count = p.count)

rm(p.count)

load("suffix.1")

suffix.3 <- merge(x = suffix.1, y = suffix.2, by = "ngrams", all = TRUE)

rm(suffix.1, suffix.2)

suffix.3$suffix.x[is.na(suffix.3$suffix.x)] <- 0 

suffix.3$suffix.y[is.na(suffix.3$suffix.y)] <- 0

suffix.3$p.count.x[is.na(suffix.3$p.count.x)] <- 0

suffix.3$p.count.y[is.na(suffix.3$p.count.y)] <- 0

gc()

suffix.3 <- data.table(ngrams = suffix.3$ngrams, suffix = rowSums(suffix.3[, .(suffix.x, suffix.y)]),
                       p.count =  rowSums(suffix.3[, .(p.count.x, p.count.y)]))

save(suffix.3, file = "suffix.3")

rm(suffix.3)

load("n2addon")

load("prefix.3")

n2addon <- merge(x = n2addon, y = prefix.3, by = "ngrams", all = TRUE)

rm(prefix.3)

n2addon$prefix.x[is.na(n2addon$prefix.x)] <- 0

n2addon$prefix.y[is.na(n2addon$prefix.y)] <- 0

n2addon <- data.table(ngrams = n2addon$ngrams, prefix = rowSums(n2addon[, -1]))

load("suffix.3")

n2addon <- merge(x = n2addon, y = suffix.3, by = "ngrams", all = TRUE)

rm(suffix.3)

n2addon$prefix[is.na(n2addon$prefix)] <- 0

n2addon$suffix[is.na(n2addon$suffix)] <- 0

n2addon$p.count[is.na(n2addon$p.count)] <- 0

save(n2addon, file = "n2addon")

rm(n2addon)

load("n1addon.1")

load("n1addon.2")

n1addon.3 <- merge(x = n1addon.1, y = n1addon.2, by = "ngrams", all = TRUE)

rm(n1addon.1, n1addon.2)

n1addon.3$prefix.x[is.na(n1addon.3$prefix.x)] <- 0 

n1addon.3$prefix.y[is.na(n1addon.3$prefix.y)] <- 0 

n1addon.3 <- data.table(ngrams = n1addon.3$ngrams, prefix = rowSums(n1addon.3[, -1]))

temp <- lapply(X = n1addon.3$ngrams, FUN = rmsuffix)

n1addon.3$ngrams <- as.character(temp)

rm(temp)

n1addon.3 <- group_by(n1addon.3, ngrams)

n1addon.3 <- data.table(summarise(n1addon.3, prefix = sum(prefix)))

load("n1addon")

n1addon <- merge(x = n1addon, y = n1addon.3, by = "ngrams", all = TRUE)

n1addon$prefix.x[is.na(n1addon$prefix.x)] <- 0 

n1addon$prefix.y[is.na(n1addon$prefix.y)] <- 0 

n1addon <- data.table(ngrams = n1addon$ngrams, prefix = rowSums(n1addon[, -1]))

save(n1addon, file = "n1addon")

rm(n1addon, n1addon.3)

load("n2addon")

load("n3grams")

temp <- history(n3grams$ngrams)

n3grams <- data.table(n3grams, history = temp)

rm(temp)

n3grams <- merge(x = n3grams, y = n2addon[, c("ngrams", "suffix", "p.count")], by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n3grams) <- c("history", "ngrams", "prefix", "history.suffix", "history.p.count")

n2addon <- n2addon[, c("ngrams", "prefix")]

n2grams <- fread("backup/n2grams.csv")[, -1]

n2grams <- merge(x = n2grams, y = n2addon, by = "ngrams", all = TRUE)

z <- str_which(string = n2grams$ngrams, pattern = "^sss")

n2grams$prefix[z] <- n2grams$count[z]

rm(z)

rm(n2addon)

n3grams <- merge(x = n3grams, y = n2grams, by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n3grams) <- c("history", "ngrams", "prefix", "history.suffix", "history.p.count", "count", "history.prefix")

n3grams <- n3grams[, c("ngrams", "prefix", "history.prefix", "history.suffix", "history.p.count")]

save(n3grams, file = "n3grams")

rm(n3grams)

################################# n1addon ####################################

count <- n2grams[, -c("ngrams")]

save(count, file = "count")

rm(count)

ngrams <- n2grams$ngrams

n2grams <- n2grams[count >=3, ]

n2grams <- n2grams[, -c("count")]

save(n2grams, file = "n2grams")

rm(n2grams)

l <- length(ngrams)

ngrams <- list(ngrams[1 : round(l/4)], 
               ngrams[(round(l/4) + 1) : round(l*2/4)], 
               ngrams[(round(l*2/4) + 1) : round(l*3/4)],
               ngrams[(round(l*3/4) + 1) : l])

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.1 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.2 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

ngrams <- ngrams[-1]

save(ngrams, file = "ngrams")

rm(ngrams)

raw.3 <- getaddon(temp)

load("ngrams")

temp <- ngrams[[1]]

raw.4 <- getaddon(temp)

rm(temp, ngrams)

prefix <- paste(raw.1[[1]], raw.2[[1]], raw.3[[1]], raw.4[[1]], collapse = " ")

suffix <- paste(raw.1[[2]], raw.2[[2]], raw.3[[2]], raw.4[[2]], collapse = " ")

rm(raw.1, raw.2, raw.3, raw.4)

save(prefix, file = "prefix")

save(suffix, file = "suffix")

rm(suffix)

load("count")

prefix <- unlist(str_split(string = prefix, pattern = " "))

prefix <- data.table(ngrams = prefix, count = count$count, prefix = count$prefix)

rm(count)

l <- length(prefix$count)

prefix.1 <- prefix[1 : round(l/2),]

prefix.2 <- prefix[(round(l/2)+1) : l,]

rm(prefix)

save(prefix.2, file = "prefix.2")

rm(prefix.2)

gc()

prefix.1 <- group_by(prefix.1, ngrams)

prefix.1 <- summarise(prefix.1, count1 = sum(count <= 2 & prefix ==1), 
                      count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                      count4 = sum(count <= 2 & prefix == 4 ), count5 = sum(count <= 2 & prefix == 5 ),
                      "count6+" = sum(count >= 3))

prefix.1$count2 <- prefix.1$count2 * 2

prefix.1$count3 <- prefix.1$count3 * 3

prefix.1$count4 <- prefix.1$count4 * 4

prefix.1$count5 <- prefix.1$count5 * 5

prefix.1 <- data.table(ngrams = prefix.1$ngrams, prefix = rowSums(prefix.1[, -1]))

save(prefix.1, file = "prefix.1")

rm(prefix.1)

load("prefix.2")

prefix.2 <- group_by(prefix.2, ngrams)

prefix.2 <- summarise(prefix.2, count1 = sum(count <= 2 & prefix ==1), 
                      count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                      count4 = sum(count <= 2 & prefix == 4 ), count5 = sum(count <= 2 & prefix == 5 ),
                      "count6+" = sum(count >= 3))

prefix.2$count2 <- prefix.2$count2 * 2

prefix.2$count3 <- prefix.2$count3 * 3

prefix.2$count4 <- prefix.2$count4 * 4

prefix.2$count5 <- prefix.2$count5 * 5

prefix.2 <- data.table(ngrams = prefix.2$ngrams, prefix = rowSums(prefix.2[, -1]))

load("prefix.1")

prefix.3 <- merge(x = prefix.1, y = prefix.2, by = "ngrams", all = TRUE)

rm(prefix.1, prefix.2)

gc()

prefix.3$prefix.x[is.na(prefix.3$prefix.x)] <- 0 

prefix.3$prefix.y[is.na(prefix.3$prefix.y)] <- 0

gc()

prefix.3 <- data.table(ngrams = prefix.3$ngrams, prefix = rowSums(prefix.3[, -1]))

save(prefix.3, file = "prefix.3")

rm(prefix.3)

load("suffix")

load("count")

suffix <- unlist(str_split(string = suffix, pattern = " "))

suffix <- data.table(ngrams = suffix, count = count$count, prefix = count$prefix)

rm(count)

l <- length(suffix$count)

suffix.1 <- suffix[1 : round(l/2),]

suffix.2 <- suffix[(round(l/2)+1) : l,]

rm(suffix)

save(suffix.2, file = "suffix.2")

rm(suffix.2)

gc()

suffix.1 <- group_by(suffix.1, ngrams)

suffix.1 <-  summarise(suffix.1, count1 = sum(count <= 2 & prefix ==1), 
                       count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                       count4 = sum(count <= 2 & prefix == 4 ), count5 = sum(count <= 2 & prefix == 5 ),
                       "count6+" = sum(count >= 3))

p.count <- suffix.1$count1 + suffix.1$count2*2 + suffix.1$count3*3 + suffix.1$count4*4 + suffix.1$count5*5

suffix.1 <- data.table(ngrams = suffix.1$ngrams, suffix = rowSums(suffix.1[, -1]), p.count = p.count)

save(suffix.1, file = "suffix.1")

rm(suffix.1)

load("suffix.2")

suffix.2 <- group_by(suffix.2, ngrams)

suffix.2 <-  summarise(suffix.2, count1 = sum(count <= 2 & prefix ==1), 
                       count2 = sum(count <= 2 & prefix == 2 ), count3 = sum(count <= 2 & prefix == 3 ),
                       count4 = sum(count <= 2 & prefix == 4 ), count5 = sum(count <= 2 & prefix == 5 ),
                       "count6+" = sum(count >= 3))

p.count <- suffix.2$count1 + suffix.2$count2*2 + suffix.2$count3*3 + suffix.2$count4*4 + suffix.2$count5*5

suffix.2 <- data.table(ngrams = suffix.2$ngrams, suffix = rowSums(suffix.2[, -1]), p.count = p.count)

rm(p.count)

load("suffix.1")

suffix.3 <- merge(x = suffix.1, y = suffix.2, by = "ngrams", all = TRUE)

rm(suffix.1, suffix.2)

suffix.3$suffix.x[is.na(suffix.3$suffix.x)] <- 0 

suffix.3$suffix.y[is.na(suffix.3$suffix.y)] <- 0

suffix.3$p.count.x[is.na(suffix.3$p.count.x)] <- 0

suffix.3$p.count.y[is.na(suffix.3$p.count.y)] <- 0

gc()

suffix.3 <- data.table(ngrams = suffix.3$ngrams, suffix = rowSums(suffix.3[, .(suffix.x, suffix.y)]),
                       p.count =  rowSums(suffix.3[, .(p.count.x, p.count.y)]))

save(suffix.3, file = "suffix.3")

rm(suffix.3)

load("n1addon")

load("prefix.3")

n1addon <- merge(x = n1addon, y = prefix.3, by = "ngrams", all = TRUE)

rm(prefix.3)

n1addon$prefix.x[is.na(n1addon$prefix.x)] <- 0

n1addon$prefix.y[is.na(n1addon$prefix.y)] <- 0

n1addon <- data.table(ngrams = n1addon$ngrams, prefix = rowSums(n1addon[, -1]))

load("suffix.3")

n1addon <- merge(x = n1addon, y = suffix.3, by = "ngrams", all = TRUE)

rm(suffix.3)

n1addon$prefix[is.na(n1addon$prefix)] <- 0

n1addon$suffix[is.na(n1addon$suffix)] <- 0

n1addon$p.count[is.na(n1addon$p.count)] <- 0

save(n1addon, file = "n1addon")

load("n2grams")

temp <- history(n2grams$ngrams)

n2grams <- data.table(n2grams, history = temp)

rm(temp)

n2grams <- merge(x = n2grams, y = n1addon[, c("ngrams", "suffix", "p.count")], by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n2grams) <- c("history", "ngrams", "prefix", "history.suffix", "history.p.count")

n1addon <- n1addon[, c("ngrams", "prefix")]

n1grams <- fread("backup/n1grams.csv")[, -1]

n1grams <- merge(x = n1grams, y = n1addon, by = "ngrams", all = TRUE)

z <- str_which(string = n1grams$ngrams, pattern = "^sss")

n1grams$prefix[z] <- n1grams$count[z]

rm(z)

rm(n1addon)

n2grams <- merge(x = n2grams, y = n1grams[, c("ngrams", "prefix")], by.x = "history", by.y = "ngrams", all.x = TRUE)

names(n2grams) <- c("history", "ngrams", "prefix", "history.suffix", "history.p.count", "history.prefix")

n2grams <- n2grams[, c("ngrams", "prefix", "history.prefix", "history.suffix", "history.p.count")]

save(n2grams, file = "n2grams")

rm(n2grams)

save(n1grams, file = "n1grams")

############################## N1 Probability ##################################

z <- str_which(string = n1grams$ngrams, pattern = "eee")

n1grams <- n1grams[-z]

rm(z)

prob <- n1grams$prefix/sum(n1grams$prefix)

n1grams <- cbind(n1grams, prob)

save(n1grams, file = "n1grams")

############################## N2 Probability ##################################

backoff <- function(data) {
    
    cl <- makeCluster(detectCores()-1)
    
    l <- length(data)
    
    data <- list(data[1 : round(l/3)], 
                 data[(round(l/3)+1) : round(l*2/3)], 
                 data[(round(l*2/3)+1) : l])
    
    clusterExport(cl =cl, varlist = c("data", "rmprefix"))
    
    clusterEvalQ(cl = cl, expr = library(stringr))
    
    backoff <- parLapply(cl = cl, X = data, fun = function(x){
        
        lapply(X = x, FUN = rmprefix)
        
    })
    
    stopCluster(cl)
    
    as.character(unlist(backoff))
}

temp <- backoff(n2grams$ngrams)

n2grams <- data.table(n2grams, backoff = temp)

rm(temp)

n2grams <- merge(x = n2grams, y = n1grams[, .(ngrams, prob)], by.x = "backoff", 
                 by.y = "ngrams", all.x = TRUE)

n2grams <- n2grams[, -c("backoff")]

names(n2grams) <- c("ngrams", "prefix", "history.prefix", "history.suffix", 
                    "history.p.count", "backoff.prob")

getprob.1 <- function(data, D){
    
    data <- data[, -c("ngrams")]
    
    cl <- makeCluster(detectCores()-1)
    
    clusterExport(cl = cl, varlist = c("data", "D2", "D3", "D4", "D5"))
    
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

prob <- getprob.1(n2grams, D2)

n2grams <- cbind(n2grams, prob)

n2grams <- n2grams[-(ngrams == "ssssss")]

############################## N3 Probability ##################################
load("n3grams")

temp <- backoff(n3grams$ngrams)

n3grams <- data.table(n3grams, backoff = temp)

rm(temp)

n3grams <- merge(x = n3grams, y = n2grams[, .(ngrams, prob)], by.x = "backoff", 
                 by.y = "ngrams", all.x = TRUE)

n3grams <- n3grams[, -c("backoff")]

names(n3grams) <- c("ngrams", "prefix", "history.prefix", "history.suffix", 
                    "history.p.count", "backoff.prob")

n3grams$backoff.prob[is.na(n3grams$backoff.prob)] <- 0 

prob <- getprob.1(n3grams, D3)

n3grams <- cbind(n3grams, prob)

############################## N4 Probability ##################################


############################## Calculating Perplexity ##################################
n1grams <- n1grams[, c(1, 5)]

n2grams <- n2grams[, c(1, 5)]

n3grams <- n3grams[, c(1, 7, 8)]

n4grams <- n4grams[, c(1, 5, 6)]


write.csv(n1grams, "n1grams.csv")

write.csv(n2grams, "n2grams.csv")

write.csv(n3grams, "n3grams.csv")

write.csv(n3grams, "n4grams.csv")


test <- read_lines("test.txt", locale = locale(encoding = "UTF-8"), n_max = 1000)

test<- as.character(lapply(1:length(test), function(i){
    
    paste(test[i], "zzzzendzzzz")
    
}))

test <- tokens(x = test, what = "word", remove_numbers = TRUE, 
               remove_punct = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

test <- as.character(test)

l <- length(test)

list1 <- list(test[1 : round(l/3)], test[round(l/3+1) : round(l/3*2)], test[round(l/3*2+1) : l])

vocab <- c(n1grams$ngrams, "zzzzendzzzz")

cl<-makeCluster(detectCores()-1)

clusterExport(cl, c("list1", "vocab"))

test <- parLapply(cl = cl, X = list1, fun = function(x){
    
    lapply(1:length(x), function(i){
        
        if(x[i] %in% vocab) {} 
        
        else{x[i] <<- "#@unk@#"}
        
    })
    
    x
    
})

stopCluster(cl)

rm(list1, vocab)

test <- unlist(c(test[[1]], test[[2]], test[[3]]))

test <- paste(test, collapse = " ")

test <- str_split(string = test, pattern = "zzzzendzzzz")[[1]]

test <- test[-str_which(string = test, pattern = "^\\s$")]

ngram.prob <- function(x) {
    
    prob <-  n4grams[n4grams$ngrams == x, ]$prob
    
    if(length(prob) == 0) {
        
        n3 <- rmprefix(x)
        
        prob <- n3grams[n3grams$ngrams == n3, ]$prob
        
        if(length(prob) == 0) {
            
            n2 <- rmprefix(n3)
            
            prob <- n2grams[n2grams$ngrams == n2, ]$prob
            
            if(length(prob) == 0) {
                
                n1 <- rmprefix(n2)
                
                prob <- n1grams[n1grams$ngrams == n1, ]$prob
            }
        }
        
    }
    
    prob
}

test <- as.character(lapply(X = test, function(x){
    
    paste("#@str@# #@str@# #@str@#", x)
    
}))

l <- length(test)

list1 <- list(test[1 : round(l/3)], test[round(l/3+1) : round(l/3*2)], test[round(l/3*2+1) : l])

cl<-makeCluster(detectCores()-1)

clusterEvalQ(cl, c(library(stringr), library(quanteda)))

clusterExport(cl, c("list1", "n1grams", "n2grams", "n3grams", "n4grams", "ngram.prob", "rmprefix"))

prob.test <- parLapply(cl = cl, X = list1, fun = function(x){
    
    lapply(X = x, function(y){
        
        temp <- as.character(tokens(x = y, what = "word", ngrams = 4L))
        
        temp <- as.numeric(lapply(X = temp, FUN = ngram.prob))
        
        prod(temp)
    })
    
})

stopCluster(cl)

prob.test <- as.numeric(c(prob.test[[1]], prob.test[[2]], prob.test[[3]]))

nngrams <- length(as.character(tokens(x = test[1:round(l)], what = "word", ngrams = 4L)))

perlexity <- 2^(sum(log(prob.test, 2))/(-nngrams))

predict <- function(x){
    
    temp <- as.character(tokens(x = char_tolower(x), what = "word", remove_numbers = TRUE, 
                                remove_punct = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE))
    
    if(length(temp) < 2) {temp <- paste("#@str@#", x)}
    
    if(length(temp) < 2) {temp <- paste("#@str@#", x)}
    
    temp <- as.character(tokens(x = x, what = "word", remove_numbers = TRUE, 
                   remove_punct = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE, ngrams = 2L))
    
    search <- paste("^", temp[length(temp)], sep = "")
    
    predn3 <- n3grams[str_which(string = n3grams$ngrams, pattern = search),]
    
    predn3 <- predn3[tail(order(predn3$prob)),]
    
    
}

