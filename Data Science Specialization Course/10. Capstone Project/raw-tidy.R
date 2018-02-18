URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

download.file(url = URL, destfile = "swift.zip", method = "libcurl")

rm(URL)

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

D1 <-  sum(n1grams$count == 1) / (sum(n1grams$count == 1) + 2 * sum(n1grams$count == 2))

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

save(n5grams, file = "n5grams")

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

prefix <- dfm(tokens(x = n5prefix, what = "fastestword"))

prefix <- data.table(ngrams = as.character(colnames(prefix)), prefix = colSums(prefix))

rm(n5prefix)

load("n5suffix")

suffix <- dfm(tokens(x = n5suffix, what = "fastestword"))

suffix <- data.table(ngrams = as.character(colnames(suffix)), suffix = colSums(suffix))

rm(n5suffix)

n4addon <- merge(x = prefix, y = suffix, by = "ngrams", all = TRUE)

rm(prefix, suffix)

n4addon$prefix[is.na(n4addon$prefix)] <- 0

n4addon$suffix[is.na(n4addon$suffix)] <- 0

save(n4addon, file = "n4addon")

n4grams <- fread("backup/n4grams.csv")[, -1]

n4grams <- merge(x = n4grams, y = n4addon, by = "ngrams", all = TRUE)

rm("n4addon")

z <- str_which(string = n4grams$ngrams, pattern = "^sss")

n4grams$prefix[z] <- n4grams$count[z]

rm(z)

save(n4grams, file = "n4grams")

rm(n4grams)

load("n5grams")

temp <- n5grams$ngrams

save(temp, file = "temp")

rm(temp)

n5grams <- n5grams[, -1]

gc()

load("n5suffix")

n5suffix <- as.character(tokens(x = n5suffix, what = "fastestword"))

n5grams <- data.table(n5grams, ngrams = n5suffix)

rm(n5suffix)

gc()

load("n4grams")

n5grams <- left_join(x = n5grams, y = n4grams[, -c("prefix")], by = "ngrams")

names(n5grams) <- c("count", "ngrams", "history.count", "history.suffix")

n5grams <- n5grams[, c("count", "history.count", "history.suffix")]

rm("n4grams")

gc()

load("temp")

n5grams <- data.table(ngrams = temp, n5grams)

rm(temp)

save(n5grams, file = "n5grams")

rm(n5grams)

######################################## Generating N3 Prefix and Suffix###########################

load("n4grams")

ngrams <- n4grams$ngrams

temp <- n4grams$prefix

save(temp, file = "temp")

rm(temp)

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

n4prefix <- paste(raw.1[[1]], raw.2[[1]], raw.3[[1]], raw.4[[1]], collapse = " ")

n4suffix <- paste(raw.1[[2]], raw.2[[2]], raw.3[[2]], raw.4[[2]], collapse = " ")

rm(raw.1, raw.2, raw.3, raw.4)

save(n4prefix, file = "n4prefix")

save(n4suffix, file = "n4suffix")

rm(n4suffix)

prefix <- dfm(tokens(x = n4prefix, what = "fastestword"))

prefix <- data.table(ngrams = as.character(colnames(prefix)), prefix = colSums(prefix))

rm(n4prefix)

load("n4suffix")

suffix <- tokens(x = n4suffix, what = "fastestword")

rm(n4suffix)

history.prefix <- as.character(suffix)

suffix <- dfm(suffix)

suffix <- data.table(ngrams = as.character(colnames(suffix)), suffix = colSums(suffix))

n3addon <- merge(x = prefix, y = suffix, by = "ngrams", all = TRUE)

rm(prefix, suffix)

gc()

n3addon$prefix[is.na(n3addon$prefix)] <- 0

n3addon$suffix[is.na(n3addon$suffix)] <- 0

load("temp")

history.prefix <- data.table(ngrams = history.prefix, history.prefix = temp)

rm(temp)

gc()

history.prefix <- group_by(history.prefix, ngrams)

history.prefix <- summarise(history.prefix, history.prefix = sum(history.prefix))

n3addon <- merge(x = n3addon, y = history.prefix, by = "ngrams", all = TRUE)

rm(history.prefix)

n3addon$history.prefix[is.na(n3addon$history.prefix)] <- 0

save(n3addon, file = "n3addon")

n3grams <- fread("backup/n3grams.csv")[, -1]

n3grams <- merge(x = n3grams, y = n3addon, by = "ngrams", all = TRUE)

rm("n3addon")

z <- str_which(string = n3grams$ngrams, pattern = "^sss")

n3grams$prefix[z] <- n3grams$count[z]

rm(z)

save(n3grams, file = "n3grams")

rm(n3grams)

load("n4grams")

temp <- n4grams$ngrams

save(temp, file = "temp")

rm(temp)

n4grams <- n4grams[, c("count", "prefix")]

gc()

load("n4suffix")

n4suffix <- as.character(tokens(x = n4suffix, what = "fastestword"))

n4grams <- data.table(n4grams, ngrams = n4suffix)

rm(n4suffix)

gc()

load("n3grams")

n4grams <- left_join(x = n4grams, y = n3grams[, -c("count", "prefix")], by = "ngrams")
    
names(n4grams) <- c("count", "prefix", "ngrams", "history.suffix", "history.prefix")

n4grams <- n4grams[, c("count", "prefix", "history.prefix", "history.suffix")]

rm("n3grams")

gc()

load("temp")

n4grams <- data.table(ngrams = temp, n4grams)

rm(temp)

save(n4grams, file = "n4grams")

rm(n4grams)

#################################N2 addon ####################################

load("n3grams")

ngrams <- n3grams$ngrams

temp <- n3grams$prefix

save(temp, file = "temp")

rm(temp)

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

n3prefix <- paste(raw.1[[1]], raw.2[[1]], raw.3[[1]], raw.4[[1]], collapse = " ")

n3suffix <- paste(raw.1[[2]], raw.2[[2]], raw.3[[2]], raw.4[[2]], collapse = " ")

rm(raw.1, raw.2, raw.3, raw.4)

save(n3prefix, file = "n3prefix")

save(n3suffix, file = "n3suffix")

rm(n3suffix)

prefix <- dfm(tokens(x = n3prefix, what = "fastestword"))

prefix <- data.table(ngrams = as.character(colnames(prefix)), prefix = colSums(prefix))

rm(n3prefix)

load("n3suffix")

suffix <- tokens(x = n3suffix, what = "fastestword")

rm(n3suffix)

history.prefix <- as.character(suffix)

suffix <- dfm(suffix)

suffix <- data.table(ngrams = as.character(colnames(suffix)), suffix = colSums(suffix))

n2addon <- merge(x = prefix, y = suffix, by = "ngrams", all = TRUE)

rm(prefix, suffix)

gc()

n2addon$prefix[is.na(n2addon$prefix)] <- 0

n2addon$suffix[is.na(n2addon$suffix)] <- 0

load("temp")

history.prefix <- data.table(ngrams = history.prefix, history.prefix = temp)

rm(temp)

gc()

history.prefix <- group_by(history.prefix, ngrams)

history.prefix <- summarise(history.prefix, history.prefix = sum(history.prefix))

n2addon <- merge(x = n2addon, y = history.prefix, by = "ngrams", all = TRUE)

rm(history.prefix)

n2addon$history.prefix[is.na(n2addon$history.prefix)] <- 0

save(n2addon, file = "n2addon")

n2grams <- fread("backup/n2grams.csv")[, -1]

n2grams <- merge(x = n2grams, y = n2addon, by = "ngrams", all = TRUE)

rm("n2addon")

z <- str_which(string = n2grams$ngrams, pattern = "^sss")

n2grams$prefix[z] <- n2grams$count[z]

rm(z)

save(n2grams, file = "n2grams")

rm(n2grams)

load("n3grams")

temp <- n3grams$ngrams

save(temp, file = "temp")

rm(temp)

n3grams <- n3grams[, c("count", "prefix")]

gc()

load("n3suffix")

n3suffix <- as.character(tokens(x = n3suffix, what = "fastestword"))

n3grams <- data.table(n3grams, ngrams = n3suffix)

rm(n3suffix)

gc()

load("n2grams")

n3grams <- left_join(x = n3grams, y = n2grams[, -c("count", "prefix")], by = "ngrams")

names(n3grams) <- c("count", "prefix", "ngrams", "history.suffix", "history.prefix")

n3grams <- n3grams[, c("count", "prefix", "history.prefix", "history.suffix")]

rm("n2grams")

gc()

load("temp")

n3grams <- data.table(ngrams = temp, n3grams)

rm(temp)

save(n3grams, file = "n3grams")

rm(n3grams)

################################# n1addon ####################################

load("n2grams")

ngrams <- n2grams$ngrams

temp <- n2grams$prefix

save(temp, file = "temp")

rm(temp)

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

n2prefix <- paste(raw.1[[1]], raw.2[[1]], raw.3[[1]], raw.4[[1]], collapse = " ")

n2suffix <- paste(raw.1[[2]], raw.2[[2]], raw.3[[2]], raw.4[[2]], collapse = " ")

rm(raw.1, raw.2, raw.3, raw.4)

save(n2prefix, file = "n2prefix")

save(n2suffix, file = "n2suffix")

rm(n2suffix)

prefix <- dfm(tokens(x = n2prefix, what = "fastestword"))

prefix <- data.table(ngrams = as.character(colnames(prefix)), prefix = colSums(prefix))

rm(n2prefix)

load("n2suffix")

suffix <- tokens(x = n2suffix, what = "fastestword")

rm(n2suffix)

history.prefix <- as.character(suffix)

suffix <- dfm(suffix)

suffix <- data.table(ngrams = as.character(colnames(suffix)), suffix = colSums(suffix))

n1addon <- merge(x = prefix, y = suffix, by = "ngrams", all = TRUE)

rm(prefix, suffix)

gc()

n1addon$prefix[is.na(n1addon$prefix)] <- 0

n1addon$suffix[is.na(n1addon$suffix)] <- 0

load("temp")

history.prefix <- data.table(ngrams = history.prefix, history.prefix = temp)

rm(temp)

gc()

history.prefix <- group_by(history.prefix, ngrams)

history.prefix <- summarise(history.prefix, history.prefix = sum(history.prefix))

n1addon <- merge(x = n1addon, y = history.prefix, by = "ngrams", all = TRUE)

rm(history.prefix)

n1addon$history.prefix[is.na(n1addon$history.prefix)] <- 0

save(n1addon, file = "n1addon")

n1grams <- fread("backup/n1grams.csv")[, -1]

n1grams <- merge(x = n1grams, y = n1addon, by = "ngrams", all = TRUE)

rm("n1addon")

z <- str_which(string = n1grams$ngrams, pattern = "^sss")

n1grams$prefix[z] <- n1grams$count[z]

rm(z)

save(n1grams, file = "n1grams")

rm(n1grams)

load("n2grams")

temp <- n2grams$ngrams

save(temp, file = "temp")

rm(temp)

n2grams <- n2grams[, c("count", "prefix")]

gc()

load("n2suffix")

n2suffix <- as.character(tokens(x = n2suffix, what = "fastestword"))

n2grams <- data.table(n2grams, ngrams = n2suffix)

rm(n2suffix)

gc()

load("n1grams")

n2grams <- left_join(x = n2grams, y = n1grams[, -c("count", "prefix", "words")], by = "ngrams")

names(n2grams) <- c("count", "prefix", "ngrams", "history.suffix", "history.prefix")

n2grams <- n2grams[, c("count", "prefix", "history.prefix", "history.suffix")]

rm("n1grams")

gc()

load("temp")

n2grams <- data.table(ngrams = temp, n2grams)

rm(temp)

save(n2grams, file = "n2grams")

############################## N1 Probability ##################################
load("n1grams")

n1grams <- n1grams[, -c("count")]

z <- str_which(string = n1grams$ngrams, pattern = "eee")

n1grams <- n1grams[-z]

rm(z)

getprob.n1 <- function(data){
    
    prefix.all <- sum(n2grams$prefix)
    
    suffix.all <- sum(data$suffix)
    
    V <- dim(data)[1]
    
    result <- lapply(X = data$prefix, FUN = function(x){
        
        (x - D1) / prefix.all + D1 * suffix.all / prefix.all/ V
        
    })
    
    unlist(result)
    
}

prob <- getprob.n1(n1grams)

n1grams <- cbind(n1grams, prob)

rm(prob)

save(n1grams, file = "n1grams")

############################## N2 Probability ##################################
load("n2grams")

temp <- n2grams[, c(1, 2)]

save(temp, file = "temp")

rm(temp)

n2grams <- n2grams[, -c(1, 2)]

load("n2prefix")

n2prefix <- as.character(tokens(x = n2prefix, what = "fastestword"))

n2grams <- data.table(ngrams = n2prefix, n2grams)

rm(n2prefix)

n2grams <- left_join(x = n2grams, y = n1grams[, c("ngrams", "prob")], by = "ngrams")

rm(n1grams)

n2grams <- n2grams[, -1]

names(n2grams) <- c("prefix", "history.prefix", "history.suffix", "backoff.prob")

getprob <- function(data, D){
    
    cl <- makeCluster(detectCores()-1)
    
    on.exit(stopCluster(cl))
    
    clusterExport(cl = cl, varlist = c("data", "D2", "D3", "D4", "D5"))
    
    result <- parApply(cl = cl, X = data, MARGIN = 1, FUN = function(i){
        
        (i[[1]] - D) / i[[2]] + D * i[[3]] / i[[2]] * i[[4]]
        
        })
    
    as.numeric(result)
}

prob <- getprob(n2grams, D2)

n2grams <- cbind(n2grams, prob)

rm(prob)

load("temp")

n2grams <- data.table(temp, n2grams)

save(n2grams, file = "n2grams")

rm(temp)

############################## N3 Probability ##################################
load("n3grams")

temp <- n3grams[, c(1,2)]

save(temp, file = "temp")

rm(temp)

n3grams <- n3grams[, - c(1,2)]

load("n3prefix")

n3prefix <- as.character(tokens(x = n3prefix, what = "fastestword"))

n3grams <- data.table(ngrams = n3prefix, n3grams)

rm(n3prefix)

n3grams <- left_join(x = n3grams, y = n2grams[, c("ngrams", "prob")], by = "ngrams")

rm(n2grams)

n3grams <- n3grams[, -1]

names(n3grams) <- c("prefix", "history.prefix", "history.suffix", "backoff.prob")

l <- length(n3grams$prefix)

prob1 <- getprob(n3grams[1 : round(l/3),], D3)

prob2 <- getprob(n3grams[(round(l/3) + 1) : round(l/3*2),], D3)

prob3 <- getprob(n3grams[(round(l/3*2) + 1) : l,], D3)

prob <- c(prob1, prob2, prob3)

rm(prob1, prob2, prob3)

n3grams <- cbind(n3grams, prob)

rm(prob)

load("temp")

n3grams <- data.table(temp, n3grams)

rm(temp)

save(n3grams, file = "n3grams")
############################## N4 Probability ##################################
n3grams <- n3grams[, c("ngrams", "prob")]

load("n4grams")

temp <- n4grams[, c(1,2)]

save(temp, file = "temp")

rm(temp)

gc()

n4grams <- n4grams[, - c(1,2)]

load("n4prefix")

n4prefix <- as.character(tokens(x = n4prefix, what = "fastestword"))

n4grams <- data.table(ngrams = n4prefix, n4grams)

rm(n4prefix)

n4grams <- left_join(x = n4grams, y = n3grams, by = "ngrams")

rm(n3grams)

n4grams <- n4grams[, -1]

names(n4grams) <- c("prefix", "history.prefix", "history.suffix", "backoff.prob")

gc()

l <- length(n4grams$prefix)

prob1 <- getprob(n4grams[1 : round(l/3),], D4)

prob2 <- getprob(n4grams[(round(l/3) + 1) : round(l/3*2),], D4)

prob3 <- getprob(n4grams[(round(l/3*2) + 1) : l,], D4)

prob <- c(prob1, prob2, prob3)

rm(prob1, prob2, prob3)

n4grams <- data.table(n4grams, prob = prob)

rm(prob)

gc()

load("temp")

temp <- data.table(temp, n4grams[, c(4,5)])

n4grams <- temp

rm(temp)

save(n4grams, file = "n4grams")

##################################### N5 Probability ###################################
n4grams <- n4grams[, c("ngrams", "prob")]

load("n5grams")

temp <- n5grams[, 1]

save(temp, file = "temp")

rm(temp)

n5grams <- n5grams[, - 1]

gc()

load("n5prefix")

n5prefix <- as.character(tokens(x = n5prefix, what = "fastestword"))

n5grams <- data.table(ngrams = n5prefix, n5grams)

rm(n5prefix)

n5grams <- left_join(x = n5grams, y = n4grams, by = "ngrams")

rm(n4grams)

n5grams <- n5grams[, -1]

names(n5grams) <- c("count", "history.count", "history.suffix", "backoff.prob")

gc()

l <- length(n5grams$count)

prob1 <- getprob(n5grams[1 : round(l/4),], D5)

prob2 <- getprob(n5grams[(round(l/4) + 1) : round(l/4*2),], D5)

prob3 <- getprob(n5grams[(round(l/4*2) + 1) : round(l/4*3),], D5)

prob4 <- getprob(n5grams[(round(l/4*3) + 1) : l,], D5)

prob <- c(prob1, prob2, prob3, prob4)

rm(prob1, prob2, prob3, prob4)

n5grams <- data.table(n5grams, prob = prob)

rm(prob)

gc()

load("temp")

temp <- data.table(temp, n5grams[, c(1, 4, 5)])

n5grams <- temp

rm(temp)

n5grams <- n5grams[-str_which(n5grams$ngrams, "s{15}"), ]

save(n5grams, file = "n5grams")

################################# Prune N5 #############################################

temp <- n5grams$count

rm(n5grams)

temp <- data.table(table(temp))

names(temp) <- c("r", "Nr")

temp$r <- as.numeric(temp$r)

gt.count <- function(data){
    
    Zr <- sapply(X = 2 : (dim(data)[1] - 1), FUN = function(i){
        
        data$Nr[i] / (0.5 * (data$r[i+1]-data$r[i-1]))
        
    })
    
    last <- data$Nr[dim(data)[1]]/(0.5 * data$r[dim(data)[1]])
    
    Zr <- c(data$Nr[1], Zr, last)
    
    data <- data.table(data, Zr = Zr)
    
    fit <- lm(formula = log10(Zr) ~ log10(r), data = data)
    
    r <- 1 : (max(data$r)+1)
    
    data <- merge(x = data, y = data.table(r), by = "r", all = TRUE)
    
    data$Nr[is.na(data$Nr)] <- 0 
    
    data$Zr[is.na(data$Zr)] <- 0 
    
    smooth <- predict(fit, newdata = data.table(r = r))
    
    smooth <- 10^as.numeric(smooth)
    
    data <- data.table(data, smooth = smooth)
    
    gt.count <- as.numeric(lapply(X = 1 : dim(data)[1], FUN = function(i){
        
        (data$r[i]+1)*data$Nr[i+1]/data$Nr[i]
        
    }))
    
    sgt.count <- as.numeric(lapply(X = 1 : dim(data)[1], FUN = function(i){
        
        (data$r[i] + 1) * data$smooth[i+1] / data$smooth[i]
        
    }))
    
    gt.var <- as.numeric(lapply(X = 1 : dim(data)[1], FUN = function(i){
        
        (data$r[i]+1)^2 * data$Nr[i+1] / data$Nr[i]^2 * (1 + data$Nr[i+1] / data$Nr[i])
        
    }))
    
    data <- data.table(data, gt.count = gt.count, sgt.count = sgt.count, gt.var = gt.var)
    
    for(i in 1 : dim(data)[1]){
        
        if(abs(data$gt.count[i] - data$sgt.count[i]) <= 1.65*sqrt(data$gt.var[i])) {q <- i; break}
        
    }
    
    final.count <- c(data$gt.count[1 : (q-1)], data$sgt.count[q : dim(data)[1]]) 
    
    data.table(count = data$r, sgt.count = final.count)
}

temp <- gt.count(temp)

load("n5grams")

n5grams <- left_join(x = n5grams, y = temp, by = "count")

rm(temp)

wdf <- n5grams$sgt.count * (log(n5grams$prob) - log(n5grams$backoff.prob))

n5grams <- n5grams[, c("ngrams", "prob")]

n5grams <- data.table(n5grams)

gc()

n5grams <- n5grams[wdf >= 0.12 ,]

save(n5grams, file = "n5grams-final")

rm(n5grams, wdf)
############################## Prune N4 ################################################

load("n4grams")

temp <- n4grams$count

rm(n4grams)

temp <- data.table(table(temp))

names(temp) <- c("r", "Nr")

temp$r <- as.numeric(temp$r)

temp <- gt.count(temp)

load("n4grams")

n4grams <- left_join(x = n4grams, y = temp, by = "count")

rm(temp)

wdf <- n4grams$sgt.count * (log(n4grams$prob) - log(n4grams$backoff.prob))

n4grams <- n4grams[, c("ngrams", "prob")]

n4grams <- data.table(n4grams)

gc()

n4grams <- n4grams[wdf >= 0.53 ,]

save(n4grams, file = "n4grams-final")

rm(n4grams, wdf)

############################## Prune N3 ################################################

load("n3grams")

temp <- n3grams$count

rm(n3grams)

temp <- data.table(table(temp))

names(temp) <- c("r", "Nr")

temp$r <- as.numeric(temp$r)

temp <- gt.count(temp)

load("n3grams")

n3grams <- left_join(x = n3grams, y = temp, by = "count")

rm(temp)

wdf <- n3grams$sgt.count * (log(n3grams$prob) - log(n3grams$backoff.prob))

n3grams <- n3grams[, c("ngrams", "prob")]

n3grams <- data.table(n3grams)

gc()

n3grams <- n3grams[wdf >= 2 ,]

save(n3grams, file = "n3grams-final")

rm(n3grams, wdf)

############################## Prune N2 ################################################

load("n2grams")

temp <- n2grams$count

rm(n2grams)

temp <- data.table(table(temp))

names(temp) <- c("r", "Nr")

temp$r <- as.numeric(temp$r)

temp <- gt.count(temp)

load("n2grams")

n2grams <- left_join(x = n2grams, y = temp, by = "count")

rm(temp)

wdf <- n2grams$sgt.count * (log(n2grams$prob) - log(n2grams$backoff.prob))

n2grams <- n2grams[, c("ngrams", "prob")]

n2grams <- data.table(n2grams)

gc()

n2grams <- n2grams[wdf >= 14.5 ,]

save(n2grams, file = "n2grams-final")

rm(wdf)

############################## Calculating Perplexity ##################################
load("n1grams-final")

n1grams <- n1grams[, c("ngrams", "words", "prob")]

n1grams[ngrams == "sss",]$words <- "sss"

save(n1grams, file = "n1grams-final")

load("n3grams-final")

load("n4grams-final")

load("n5grams-final")

n1grams <- n1grams[order(prob, decreasing = TRUE)]

n2grams <- n2grams[order(prob, decreasing = TRUE)]

n3grams <- n3grams[order(prob, decreasing = TRUE)]

n4grams <- n4grams[order(prob, decreasing = TRUE)]

n5grams <- n5grams[order(prob, decreasing = TRUE)]

save(n1grams, file = "n1grams-final")

save(n2grams, file = "n2grams-final")

save(n3grams, file = "n3grams-final")

save(n4grams, file = "n4grams-final")

save(n5grams, file = "n5grams-final")

test <- read_lines("test.txt", locale = locale(encoding = "UTF-8"), n_max = 100000)

test<- as.character(lapply(1:length(test), function(i){
    
    paste("sss sss sss sss", test[i])
    
}))

test <- tokens(x = test, what = "word", remove_numbers = TRUE, 
               remove_punct = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

test <- as.list(test)

cl<-makeCluster(detectCores()-1)

clusterExport(cl, c("test", "n1grams"))

clusterEvalQ(cl = cl, expr = c(library(stringr), library(data.table), library(quanteda)))

test <- parLapply(cl = cl, X = test, fun = function(x){
    
    as.character(lapply(X = x, FUN = function(i){
        
        temp <- n1grams[words == i, ngrams]
        
        if(length(temp) == 0) {temp <- "zzz"}
        
        temp
        }))
    })

ngrams <- parLapply(cl = cl, X = test, fun = function(x){
    
    as.character(tokens(x = paste(x, collapse = " "), what = "fastestword", concatenator = "", ngrams = 5L))
    
})

stopCluster(cl)

ngram.prob <- function(x) {
    
    prob <- n5grams[ngrams == x, prob]
    
    if(length(prob) == 0) {
        
        x <- rmprefix(x)
        
        prob <- n4grams[ngrams == x, prob]
        
        if(length(prob) == 0) {
            
            x <- rmprefix(x)
            
            prob <- n3grams[ngrams == x, prob]
            
            if(length(prob) == 0) {
                
                x <- rmprefix(x)
                
                prob <- n2grams[ngrams == x, prob]
                
                if(length(prob) == 0) {
                    
                    x <- rmprefix(x)
                    
                    prob <- n1grams[ngrams == x, prob]       
                    
                }
                
            }
        }
    }
    prob
}

cl<-makeCluster(detectCores()-1)

clusterEvalQ(cl, c(library(stringr), library(quanteda)))

clusterExport(cl, c("ngrams", "n1grams", "n2grams", "n3grams", "n4grams", "n5grams", "ngram.prob", "rmprefix"))

prob.test <- parLapply(cl = cl, X = ngrams, fun = function(x){
    
   temp <- as.numeric(lapply(X = x, FUN = ngram.prob))
   
   sum(log(temp, 2))
})

stopCluster(cl)

prob.test <- as.numeric(prob.test)

2^-(sum(prob.test)/length(unlist(ngrams)))