#package is not available; 
#get data here: http://www.stat.berkeley.edu/~nolan/data/spam/SpamAssassinMessages.zip

setwd("C:/MyGitRepos/identifying-spam/data")

head(list.files(path = "easy_ham"))
head(list.files(path = "spam_2"))

dirNames <- list.files()
length(list.files(dirNames))
sapply(dirNames, function(dir) length(list.files(dir)))

fileNames <- list.files(dirNames, full.names = T)

#read first message in easy_ham
msg <- readLines(fileNames[1])

#select small subset of ham emails msgs to use as test cases
indx <- c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
fn <- list.files(dirNames[1], full.names = T)[indx]
sampleEmail <- sapply(fn, readLines, USE.NAMES = F)
#sampleEmail2 <- sapply(fn, readLines, USE.NAMES = T)

###Splitting message into header and body
msg <- sampleEmail[[1]]
splitPoint <- match("", msg)

#look at lines before and after splitpoint
msg[(splitPoint - 2):(splitPoint + 6)]

#determine header and footer
header <- msg[1:(splitPoint-1)]
body <- msg[-(1:splitPoint)]

#create function to split message into header and body
splitMessage <- function(msg){
  splitPoint <- match("", msg)
  header <- msg[1:(splitPoint-1)]
  body <- msg[-(1:splitPoint)]
  return(list(header = header, body = body))  
}

#apply function to list of 15 sample messages
sampleSplit <- lapply(sampleEmail, splitMessage)



###Removing attachments from the message body
header <- sampleSplit[[1]]$header
grep("Content-Type", header)
grep("multi", tolower(header[46]))

headerList <- lapply(sampleSplit, function(msg) msg$header)
CTloc <- sapply(headerList, grep, pattern = "Content-Type")

hasAttachment <- function(header){
  CTloc <- grep("Content-Type", header)
  if (length(CTloc) == 0) return(FALSE)
  grepl("multi", tolower(header[CTloc]))
}

hasAttach <- sapply(headerList, hasAttachment)

hasAttach


###Find boundary string
header <- sampleSplit[[6]]$header
boundaryIdx <- grep("boundary=", header)
header[boundaryIdx]
sub(".*boundary=\"(.*)\";.*", "\\1", header[boundaryIdx])

#that regex doesn't work with all boundary strings, as shown here
header2 <- headerList[[9]]
boundaryIdx2 <- grep("boundary=", header2)
header2[boundaryIdx2]
sub(".*boundary=\"(.*)\";.*", "\\1", header2[boundaryIdx2])

#one solution is to remove quotation marks from string, then we won't need to search
#for them at all in the regex
#also make semicolon optional at end with a ?
boundary2 <- gsub('"', "", header2[boundaryIdx2])
sub(".*boundary= *(.*);?.*", "\\1", boundary2)

#test new regex on first boundary string example
#it now incorrectly includes semicolon at end
boundary <- gsub('"', "", header[boundaryIdx])
sub(".*boundary= *(.*);?.*", "\\1", boundary)

#this is b/c of greedy matching; semicolon is found by the . in the group
#and not by the optional semicolon outside the parenthesis
#fix this by using [^;] instead of ., ie anything but a semicolon
sub(".*boundary= *([^;]*);?.*", "\\1", boundary)

#create function to retrieve boundary string from header
getBoundary <- function(header){
  boundaryIdx <- grep("boundary=", header)
  boundary <- gsub('"', "", header[boundaryIdx])
  sub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}

#last msg of sample
boundary <- getBoundary(headerList[[15]])
body <- sampleSplit[[15]]$body
bString <- paste0("--", boundary)
bStringLocs <- which(bString == body)

eString <- paste0("--", boundary, "--")
eStringLoc <- which(eString == body)

msg <- body[(bStringLocs[1] + 1):(bStringLocs[2] - 1)]
msg <- c(msg, body[(eStringLoc + 1):length(body)])


###Write dropAttach function to do the following:
#drop blank lines before 1st boundary string
#keep lines after the closing string as part of the 1st portion of the body and not the attachments
#use last line of email as end of attachment if we find no closing boundary string

#pass in header section and body


boundary <- getBoundary(headerList[[11]])
body <- sampleSplit[[11]]$body

bString <- paste0("--", boundary)
bStringLocs <- which(bString == body)

eString <- paste0("--", boundary, "--")
eStringLoc <- which(eString == body)


msg <- body[(bStringLocs[1] + 1):(bStringLocs[2] - 1)]
msg <- c(msg, body[(eStringLoc + 1):length(body)])
attach <- body[(bStringLocs[2] + 1):(eStringLoc - 1)]


dropAttach <- function(body, boundary){
  
  bString <- paste0("--", boundary)
  bStringLocs <- which(bString == body)
  eString <- paste0("--", boundary, "--")
  eStringLoc <- which(eString == body)
  
  n <- length(body)
  
  if (length(bStringLocs) == 0){
    return (body)
  } else if (length(bStringLocs) == 1){
    if (length(eStringLoc) == 0) {
      return (body[(bStringLocs + 1):n])
    } else if (eStringLoc < n) {
      return (body[c((bStringLocs+1):(eStringLoc-1), (eStringLoc+1):n)])
    } else {
      return (body[(bStringLocs + 1):n])
    }
  } else {
    if (length(eStringLoc) == 0) {
      return (body[(bStringLocs[1] + 1):(bStringLocs[2] - 1)])
    } else if (eStringLoc < n) {
      return (c(body[(bStringLocs[1] + 1):(bStringLocs[2] - 1)], body[(eStringLoc + 1):n]))
    } else {
      return (body[(bStringLocs[1] + 1):(bStringLocs[2] - 1)])
    }  
  }
  
}


###Exercise Q.1: sample msgs from each of the 5 directories and 
###test getBoundary() and dropAttach() on them
testSamp <- sapply(dirNames, function(x) {sample(list.files(x, full.names = T), 5)})

testMsgs <- sapply(testSamp, readLines)
testSplit <- lapply(testMsgs, splitMessage)

testHeads <- lapply(testSplit, function(msg) msg$header)
testBods <- lapply(testSplit, function(msg) msg$body)

testBoundStrs <- sapply(testHeads, getBoundary)
testBodsNoAttach <- mapply(dropAttach, testBods, testBoundStrs)


###3.5.3 Extracting Words from the Message Body
head(sampleSplit[[1]]$body)

msg <- sampleSplit[[3]]$body
head(msg)

cleanMsg <- tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", msg))
words <- unlist(strsplit(cleanMsg, "[[:blank:]]+"))
words <- words[nchar(words) > 1]

require(tm)
stopWords <- stopwords("en")
words <- words[!(words %in% stopWords)]


###Exercise Q.5: write the function findMsgWords()
findMsgWords <- function(msg, stopWords){
  cleanMsg <- tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", msg))
  words <- unlist(strsplit(cleanMsg, "[[:blank:]]+"))
  words <- words[nchar(words) > 1]
  unique(words[!(words %in% stopWords)])
}

require(tm)
stopWords <- stopwords("en")
findMsgWords(msg, stopWords)


###3.5.4: Completing the Data Preparation Process
processAllWords <- function(dirName, stopWords){
  #read all files in the dir
  fileNames <- list.files(dirName, full.names = T)
  
  #drop files that are not email, i.e., cmds
  notEmail <- grep("cmds$", fileNames)
  if (length(notEmail) > 0) fileNames <- fileNames[-notEmail]
  
  messages <- lapply(fileNames, readLines, encoding = "latin1")  
  
  #split header and body
  emailSplit <- lapply(messages, splitMessage)
  
  #put body and header in own lists
  bodyList <- lapply(emailSplit, function(msg) msg$body)
  headerList <- lapply(emailSplit, function(msg) msg$header)
  rm(emailSplit)
  
  #determine which messages have attachments
  hasAttach <- sapply(headerList, function(header){
    CTloc <- grep("Content-Type", header)
    if (length(CTloc) == 0) return(0)
    multi <- grep("multi", tolower(header[CTloc]))
    if (length(multi) == 0) return(0)
    multi
  })
  
  hasAttach <- which(hasAttach > 0)

  #find boundary strings for messages with attachments
  boundaries <- sapply(headerList[hasAttach], getBoundary)
  
  #drop attachments from message body
  bodyList[hasAttach] <- mapply(dropAttach, bodyList[hasAttach], boundaries, SIMPLIFY = F)
  
  #extract words from body
  msgWordsList <- lapply(bodyList, findMsgWords, stopWords)
  
  invisible(msgWordsList)
}

fullDirNames <- list.files(full.names = T)
msgWordsList <- lapply(fullDirNames, processAllWords, stopWords = stopWords)

numMsgs <- sapply(msgWordsList, length)

isSpam <- rep(c(F, F, F, T, T), numMsgs)

msgWordsList <- unlist(msgWordsList, recursive = F)

###3.6.1: Test and Training Data
numEmail <- length(isSpam)
numSpam <- sum(isSpam)
numHam <- numEmail - numSpam

set.seed(418910)

testSpamIdx <- sample(numSpam, size=floor(numSpam/3))
testHamIdx <- sample(numHam, size=floor(numHam/3))

testMsgWords <- c(msgWordsList[isSpam][testSpamIdx], msgWordsList[!isSpam][testHamIdx])
trainMsgWords <- c(msgWordsList[isSpam][-testSpamIdx], msgWordsList[!isSpam][-testHamIdx])

testIsSpam <- rep(c(T, F), c(length(testSpamIdx), length(testHamIdx)))
trainIsSpam <- rep(c(T, F), c(numSpam - length(testSpamIdx), numHam - length(testHamIdx)))


###3.6.1: Probability Estimates from Training Data
bow <- unique(unlist(trainMsgWords))
spamWordCounts <- rep(0, length(bow))
names(spamWordCounts) <- bow

tmp <- lapply(trainMsgWords[trainIsSpam], unique)
tt <- table(unlist(tmp))
spamWordCounts[names(tt)] <- tt
spamWordProbs <- (spamWordCounts + 0.5)/(sum(trainIsSpam) + 0.5)

hamWordCounts <- rep(0, length(bow))
names(hamWordCounts) <- bow

tmp <- lapply(trainMsgWords[!trainIsSpam], unique)
tt <- table(unlist(tmp))
hamWordCounts[names(tt)] <- tt
hamWordProbs <- (hamWordCounts + 0.5)/(sum(!trainIsSpam) + 0.5)

computeFreqs <- function(wordsList, spam, bow = unique(unlist(wordsList))){
  
  #create a matrix for spam, ham, and log odds
  wordTable <- matrix(0.5, nrow = 4, ncol = length(bow), 
                      dimnames = list(c("spam", "ham", "presentLogOdds", "absentLogOdds"), bow))
  
  #Populate spam counts row of matrix
  counts.spam <- table(unlist(lapply(wordsList[spam], unique)))
  wordTable["spam", names(counts.spam)] <- counts.spam + 0.5
  
  #Populate ham counts row of matrix
  counts.ham <- table(unlist(lapply(wordsList[!spam], unique)))
  wordTable["ham", names(counts.ham)] <- counts.ham + 0.5
  
  #Find the total number of spam and ham
  numSpam <- sum(spam)
  numHam <- length(spam) - numSpam
  
  #Prob(word|spam) and Prob(word|ham)
  wordTable["spam", ] <- wordTable["spam", ]/(numSpam + 0.5)
  wordTable["ham", ] <- wordTable["ham", ]/(numHam + 0.5)
  
  #log odds
  wordTable["presentLogOdds", ] <- log(wordTable["spam", ]) - log(wordTable["ham", ])
  wordTable["absentLogOdds", ] <- log(1 - wordTable["spam", ]) - log(1- wordTable["ham", ])
  
  invisible(wordTable)
}

trainTable <- computeFreqs(trainMsgWords, trainIsSpam)


##3.6.3: Classifying New Messages
newMsg <- testMsgWords[[1]]
newMsg <- newMsg[!is.na(match(newMsg, colnames(trainTable)))]
present <- colnames(trainTable) %in% newMsg
sum(trainTable["presentLogOdds", present]) + sum(trainTable["absentLogOdds", !present])

newMsg <- testMsgWords[[which(!testIsSpam)[1]]]
newMsg <- newMsg[!is.na(match(newMsg, colnames(trainTable)))]
present <- colnames(trainTable) %in% newMsg
sum(trainTable["presentLogOdds", present]) + sum(trainTable["absentLogOdds", !present])

computeMsgLLR <- function(words, freqTable){
  
  #Discard words not in training data
  words <- words[!is.na(match(words, colnames(freqTable)))]
  
  #Find which words are present
  present <- colnames(freqTable) %in% words
  
  sum(freqTable["presentLogOdds", present]) + sum(freqTable["absentLogOdds", !present])
}

testLLR <- sapply(testMsgWords, computeMsgLLR, trainTable)
tapply(testLLR, testIsSpam, summary)

typeIErrorRate <- function(tau, llrVals, spam) {
  classify <- llrVals > tau
  sum(classify & !spam)/sum(!spam)
}

typeIErrorRate(0, testLLR, testIsSpam)
typeIErrorRate(-20, testLLR, testIsSpam)