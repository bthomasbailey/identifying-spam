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


###Naive Bayesian analysis

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
  #cleanMsg <- tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", msg))
  cleanMsg <- tolower(gsub("[^[:alpha:]]+", " ", msg))
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

typeIErrorRates <- function(llrVals, isSpam){
  o <- order(llrVals)
  llrVals <- llrVals[o]
  isSpam <- isSpam[o]
  
  idx <- which(!isSpam)
  N <- length(idx)
  list(error = (N:1)/N, values = llrVals[idx])
}


###Classification Tree Approach
#3.8.1: Processing the Header
header <- sampleSplit[[1]]$header
header[1] <- sub("^From", "Top-From:", header[1])

headerPieces <- read.dcf(textConnection(header), all = T)
headerVec <- unlist(headerPieces)
dupKeys <- sapply(headerPieces, function(x) length(unlist(x)))
names(headerVec) <- rep(colnames(headerPieces), dupKeys)

processHeader <- function(header){
  
  #modify first line to create a key:value pair
  header[1] <- sub("^From", "Top-From:", header[1])
  
  conn <- textConnection(header)
  headerMat <- read.dcf(conn, all = T)
  headerVec <- unlist(headerMat)
  
  dupKeys <- sapply(headerMat, function(x) length(unlist(x)))
  names(headerVec) <- rep(colnames(headerMat), dupKeys)
  
  close(conn)
  
  return(headerVec)      
}

headerList <- lapply(sampleSplit, function(msg) processHeader(msg$header))

contentTypes <- sapply(headerList, function(header) header["Content-Type"])
names(contentTypes) <- NULL

#3.8.2: Processing Attachments
hasAttach <- grep("^ *multi", tolower(contentTypes))
boundaries <- getBoundary(contentTypes[hasAttach])

boundary <- boundaries[9]
body <- sampleSplit[[15]]$body

bString <- paste0("--", boundary)
bStringLocs <- which(bString == body)

eString <- paste0("--", boundary, "--")
eStringLoc <- which(eString == body)


processAttach <- function(body, contentType){
  
  #use contentType to get boundary string
  boundary <- getBoundary(contentType)
  
  bString <- paste0("--", boundary)
  bStringLocs <- which(bString == body)
  
  eString <- paste0("--", boundary, "--")
  eStringLoc <- which(eString == body)
  if (length(eStringLoc) == 0) {eStringLoc <- length(body)}
  
  #account for emails that have a boundary string saying they have attachments, but actually don't
  if (length(bStringLocs) > 1) {
    attStartLocs <- bStringLocs[-1] + 1
    attEndLocs <- c(bStringLocs[-1], eStringLoc)[-1] - 1
    
    attLens <- attEndLocs - attStartLocs + 1
    # attLines <- unlist(mapply(function(x, y) x:(x + y - 1), attStartLocs, attLens))
    # attsBodyArea <- body[attLines]
    # attContTypeLines <- attsBodyArea[grep("content-type:", tolower(attsBodyArea))]
    
    attContTypeLines <- body[attStartLocs]
    contTypeVals <- sapply(strsplit(attContTypeLines, ":"), 
                           function(x) trimws(strsplit(x[2], ";")[[1]][1]))
    
    attDF <- data.frame("aLen" = attLens, "aType" = contTypeVals, stringsAsFactors = F)
  
  } else{
    
    attDF <- data.frame("aLen" = NA, "aType" = NA)
  
  }
  
  list(body = dropAttach(body, boundary), attachDF = attDF)
  
}

#3.8.3: Testing Our Code on More email Data
bodyList <- lapply(sampleSplit, function(msg) msg$body)
attList <- mapply(processAttach, bodyList[hasAttach], contentTypes[hasAttach], SIMPLIFY = F)

lens <- lapply(attList, function(processedA) processedA$attachDF$aLen)


#3.8.4: Completing the Process

readEmail <- function(dirName){
  #retrieve the name of file in directory
  fileNames <- list.files(dirName, full.names = T)
  
  #drop files that are not email, i.e., cmds
  notEmail <- grep("cmds$", fileNames)
  if (length(notEmail) > 0) fileNames <- fileNames[-notEmail]
  
  sapply(fileNames, readLines, encoding = "latin1", USE.NAMES = T)  
}

processAllEmail <- function(dirName, isSpam = F){
  #read all files in the directory
  messages <- readEmail(dirName)
  fileNames <- names(messages)
  n <- length(messages)
  
  #split header from body
  eSplit <- lapply(messages, splitMessage)
  rm(messages)

  #process header as named character vector
  headerList <- lapply(eSplit, function(msg) processHeader(msg$header))
  
  #extract content-type key
  contentTypes <- sapply(headerList, function(header) header["Content-Type"])

  #extract the body
  bodyList <- lapply(eSplit, function(msg) msg$body)
  rm(eSplit)
  
  #which emails have attachments
  hasAttach <- grep("^ *multi", tolower(contentTypes))
  
  #get summary stats for attachments and the shorter body
  attList <- mapply(processAttach, bodyList[hasAttach], contentTypes[hasAttach], SIMPLIFY = F)
  
  bodyList[hasAttach] <- lapply(attList, function(attEl) attEl$body)

  attachInfo <- vector("list", length = n)
  attachInfo[hasAttach] <- lapply(attList, function(attEl) attEl$attachDF)
  
  #prepare return structure
  emailList <- mapply(function(header, body, attach, isSpam) {
                        list(isSpam = isSpam, header = header, body = body, attach = attach)
                      },
                      headerList, bodyList, attachInfo, rep(isSpam, n), SIMPLIFY = F)

  names(emailList) <- fileNames
  
  invisible(emailList)  
  
}


emailStruct <- mapply(processAllEmail, fullDirNames, isSpam = rep(c(F, T), 3:2))
emailStruct <- unlist(emailStruct, recursive = F)

sampleStruct <- emailStruct[indx]


#3.9: Deriving Variables from the email Message
header <- sampleStruct[[1]]$header
subject <- header["Subject"]

els <- strsplit(subject, "")$Subject
all(els %in% LETTERS)

testSubject <- c("DEAR MADAME", "WINNER!", "")

els <- strsplit(testSubject, "")
sapply(els, function(subject) all(subject %in% LETTERS))

gsub("[^[:alpha:]]", "", testSubject)


isYelling <- function(msg){
  if ("Subject" %in% names(msg$header)){
    el <- gsub("[^[:alpha:]]", "", msg$header["Subject"])  
    if (nchar(el) > 0){
      gsub("[A-Z]", "", el) < 1  
    } else {
      FALSE
    }
  } else {
    NA
  }
}

perCaps <- function(msg){
  body <- paste(msg$body, collapse = "")
  
  #Return NA if the body of the message is "empty"
  if (length(body) == 0 || nchar(body) == 0) {return(NA)}
  
  #Eliminate non-alpha characters
  body <- gsub("[^[:alpha:]]", "", body)
  capText <- gsub("[^A-Z]", "", body)
  100*nchar(capText)/nchar(body)
}

sapply(sampleStruct, perCaps)


funcList <- list(
  
  isRe = function(msg) {
    "Subject" %in% names(msg$header) && length(grep("^[ ]*Re:", msg$header[["Subject"]])) > 0
  },
  
  numLines = function(msg) length(msg$body),
  
  isYelling = function(msg){
    if ("Subject" %in% names(msg$header)){
      el <- gsub("[^[:alpha:]]", "", msg$header["Subject"])  
      if (nchar(el) > 0){
        gsub("[A-Z]", "", el) < 1  
      } else {
        FALSE
      }
    } else {
      NA
    }
  },
  
  perCaps = function(msg){
    body <- paste(msg$body, collapse = "")
    
    #Return NA if the body of the message is "empty"
    if (length(body) == 0 || nchar(body) == 0) {return(NA)}
    
    #Eliminate non-alpha characters
    body <- gsub("[^[:alpha:]]", "", body)
    capText <- gsub("[^A-Z]", "", body)
    100*nchar(capText)/nchar(body)
  },
  
  
  #Number of characters in the body of the msg
  bodyCharCt = function(msg){
    body <- paste(msg$body, collapse = "")
    
    #Return NA if the body of the message is "empty"
    if (length(body) == 0 || nchar(body) == 0) {
      NA    
    } else {
      nchar(body)
    }
  },
  
  #TRUE if email address in the From field of the header contains an underscore
  underscore = function(msg){
    fromEmail <- strsplit(msg$header[["Top-From"]], " ")[[1]][1]
    regexpr("_", fromEmail) != -1
  },
  
  #Number of exclamation marks in the subject
  subExcCt = function(msg){
    if("Subject" %in% names(msg$header)){
      exPtLoc <- gregexpr("!", msg$header[["Subject"]])[[1]]
      ifelse(exPtLoc == -1, 0, length(exPtLoc))
    } else {
      0
    }
  },
  
  #Number of question marks in the subect
  subQuesCt = function(msg){
    if("Subject" %in% names(msg$header)){
      quesLoc <- gregexpr("\\?", msg$header[["Subject"]])[[1]]
      ifelse(quesLoc == -1, 0, length(quesLoc))
    } else {
      0
    }  
  },
  
  #TRUE if a Priority key is present in the header
  priority = function(msg){
    "Priority" %in% names(msg$header)
  },
  
  #Number of recipients of the message, including CCs
  numRec = function(msg){
    if(!("To" %in% names(msg$header))){
      NA
    } else{
      length(strsplit(msg$header[["To"]], ",")[[1]]) + 
        ifelse("CC" %in% names(msg$header), length(strsplit(msg$header[["CC"]], ",")[[1]]), 0)
    }
  },
  
  #TRUE if the In-Reply-To key is present in the header
  isInReplyTo = function(msg){
    "In-Reply-To" %in% names(msg$header)
  },
  
  #TRUE if words in the subject have punctuation or numbers embedded in them, e.g., w!se
  subPunc = function(msg){
    subjWords <- strsplit(msg$header[["Subject"]], " ")[[1]]
    subjWords <- gsub("^.(.*).", "\\1", subjWords)
    hasPunc <- gregexpr("[^[:alpha:]]", subjWords)
    any(hasPunc!=-1)
  },
  
  #Hour of the day in the Date field
  hour = function(msg){
    if("Date" %in% names(msg$header)){
      as.numeric(gsub(".*\\s([0-9]{2}):.*", "\\1", msg$header[["Date"]]))
    } else{
      NA
    }
  },
  
  #TRUE if the MIME type is multipart/text
  multipartText = function(msg){
    if("Content-Type" %in% names(msg$header)){
      length(grep("multipart/text", tolower(msg$header[["Content-Type"]]))) > 0
    } else{
      NA
    }
  },
  
  #TRUE if the message contains a PGP signature
  isPGPsigned = function(msg){
    if("Content-Type" %in% names(msg$header)){
      length(grep("pgp-signature", tolower(msg$header[["Content-Type"]]))) > 0
    } else{
      NA
    }
  },
  
  #Percentage of blanks in the subject
  subBlanks = function(msg){
    if("Subject" %in% names(msg$header)){
      length(gregexpr(" ", msg$header[["Subject"]])[[1]])/nchar(msg$header[["Subject"]])
    } else{
      NA
    }
  },
  
  #TRUE if there is no hostname in the Message-Id key in the header
  noHost = function(msg){
    if("Message-Id" %in% names(msg$header)){
      #make sure to strip off surrounding <  >
      gsub("<(.*)>", "\\1", msg$header[["Message-Id"]]) == "" 
    } else {
      NA
    }
  },
  
  #TRUE if the email sender's address (before the @) ends in a number
  numEnd = function(msg){
    lastChr <- gsub("(.*)(.)@.*", "\\2", msg$header[["From"]])
    suppressWarnings(!is.na(as.numeric(lastChr))) 
  },
  
  #TRUE if the message body contains the phrase "original message"
  isOrigMsg = function(msg){
    length(grep("original message", tolower(msg$body))) > 0  
  },
  
  #TRUE if the message body contains the word "dear"
  isDear = function(msg){
    length(grep("dear", tolower(msg$body))) > 0  
  },
  
  #TRUE if the message body contains the phrase "wrote:"
  isWrote = function(msg){
    length(grep("wrote:", tolower(msg$body))) > 0  
  },
  
  #Number of dollar signs in the message body
  numDlr = function(msg){
    dlrLocs <- unlist(sapply(msg$body, gregexpr, pattern = "\\$"))
    sum(dlrLocs != -1)
  },
  
  #The average length of words in a message
  avgWordLen = function(msg){
    words <- unlist(sapply(msg$body, strsplit, split = " ", USE.NAMES = F))
    sum(nchar(words))/length(words)
  }
  
)


lapply(funcList, function(func) sapply(sampleStruct, function(msg) func(msg)))

createDerivedDF <- function(email = emailStruct, operations = funcList, verbose = F){
  els <- lapply(names(operations), function(id) {
    if(verbose){print(id)}
    e <- operations[[id]]
    v <- if(is.function(e)) {
      sapply(email, e)
    } else {
      sapply(email, function(msg) eval(e))
    }
    v
  })
  
  df <- as.data.frame(els)
  names(df) <- names(operations)
  invisible(df)
}

sampleDF <- createDerivedDF(sampleStruct)


###Exercise Q.15: write more functions to add to funcList
#see additions to funcList above


# #TRUE if the recipients' email addresses are sorted
# sortedRec <- function(msg){
#   if(!("To" %in% names(msg$header))){
#     FALSE 
#   } else{
#     recs <- gsub("[^[:alpha:]]+", "", strsplit(msg$header[["To"]], ",")[[1]])
#     
#     if(length(recs) == 1){
#       FALSE
#     }else if(!("CC" %in% names(msg$header))){
#       all(sort(recs) == recs)
#     } else{
#       ccRecs <- gsub("[^[:alpha:]]+", "", strsplit(msg$header[["CC"]], ",")[[1]])
#       all(sort(recs) == recs) && all(sort(ccRecs) == ccRecs)
#     }
#   }
# }

