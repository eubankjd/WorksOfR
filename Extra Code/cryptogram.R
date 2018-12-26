# Daniel Eubanks
# October 2018
# WorksOfR.wordpress.com

######################################################################
# FUNCTIONS
######################################################################

# Generate numeric word patterns for dictionary pruning
genPattern = function(word) {
    chars = unlist(strsplit(word, split=""))
    uniqueChars = unique(chars)
    pattern = sapply(chars, function(c) which(uniqueChars == c))
    pattern = paste(pattern, collapse="")
    return(pattern)
}

# Prune dictionary based on current encoding
pruneDictionary = function(encoding) {
    
    # Chars currently in encoding
    encodedChars = paste(unlist(encoding),collapse="")
    if(length(encoding) == 0) {
        blankSymbol = "[a-z]"
    } else {
        blankSymbol = paste0("[^",encodedChars,"]")
    }
    
    lens = unique(nchar(cryptowords))
    for(wordlen in lens) {
        
        # Get list of words that are of length wordlen
        words = cryptowords[nchar(cryptowords) == wordlen]
        
        # Generate regex for each cryptoword of length wordlen
        regexes = c()
        for(word in words) {
            wordchars = unlist(strsplit(word,split=""))
            decoded = c()
            for(char in wordchars) {
                symbol = encoding[[char]]
                if(is.null(symbol)) {
                    symbol = blankSymbol
                }
                decoded = c(decoded,symbol)
            }
            regex = paste(decoded, collapse="")
            regexes = c(regexes, regex)
        }
        
        regexes = paste(regexes,collapse="|")
        
        wordlen = as.character(wordlen)
        dictionary[[wordlen]] = dictionary[[wordlen]][grepl(regexes,dictionary[[wordlen]])]
        
    }
    
    return(dictionary)
    
}

# Check if current encoding is feasible
checkEncoding = function(encoding) {
    # Infeasible if letters are repeated in encoding
    lettersInEncoding = length(unique(unlist(encoding)))
    if(lettersInEncoding < length(names(encoding))) {
        return(FALSE)
    }
    
    # Infeasible if encoding eliminates all candidates for a later word
    lens = unique(nchar(cryptowords))
    for(wl in lens) {
        wl = as.character(wl)
        if(length(pruneDict[[wl]]) == 0) {
            return(FALSE)
        }
    }
    # Infeasible if encoding creates an invalid word
    decodedWords = unlist(strsplit(decode(encoding),split=" "))
    for(word in decodedWords) {
        if(grepl("-",word,fixed=TRUE)) {
            next
        } else {
            wl = as.character(nchar(word))
            if(!word %in% dictionary[[wl]]) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

# Get all cryptochars in cryptoquip up to and including word i
getCryptochars = function(i) {
    chars = c()
    for(word in cryptowords[1:i]) {
        chars = c(chars, unlist(strsplit(word,split="")))
    }
    return(unique(chars))
}

# Decode based on current encoding
decode = function(encoding) {
    cryptochars = unlist(strsplit(cryptoquip,split=""))
    solution = c()
    for(c in cryptochars) {
        if(c == " ") {
            solution = c(solution," ")
        } else {
            
            if(!is.null(encoding[[c]])) {
                solution = c(solution,encoding[[c]])
            } else {
                solution = c(solution,"-")
            }
        }
    }
    return(paste(solution,collapse=""))
}

######################################################################
# SET UP
######################################################################

# cryptoquip = "bucifbp fb cifl rujze nob coty ciy kzony uq kyjlflcybny 
# bucifbp fl wujy nuwwub ciob dbldnnyllqdz kyukzy rfci cozybc"
cryptoquip = "iq rwzq rl dlzw olwy dlzw olwp nep dlzw gwsqep"

# Load dictionary
print("Loading dictionary . . .")
dictionary = readLines("C:/Users/Daniel/Documents/R Scripts/clean_wordlist.txt", warn=FALSE)
# dictionary = readLines("C:/Users/Daniel/Documents/R Scripts/wordlist_20k.txt", warn=FALSE)

# Split cryptoquip into words and order by length
cryptowords = unlist(strsplit(cryptoquip, split=" "))
cryptowords = cryptowords[order(nchar(cryptowords),decreasing=TRUE)]
cryptowords = unique(cryptowords)

# Generate patterns and prune dictionary
print("Pruning dictionary on patterns and word length . . .")
wordlens = unique(nchar(cryptowords))
dictionary = dictionary[nchar(dictionary) %in% wordlens]
dictpatterns = sapply(dictionary, genPattern)
cryptopatterns = sapply(cryptowords, genPattern)
dictionary = dictionary[dictpatterns %in% cryptopatterns]
rm(dictpatterns)
rm(cryptopatterns)

# Partition dictionary by word length
print("Partitioning dictionary . . .")
dictionary = split(dictionary,nchar(dictionary))

pruneDict = dictionary
encoding = list()
clueLetters = list("q"="e","l"="o","z"="u","r"="t")

# Store index of last word picked for each word length
lastPicked = list()
for(w in cryptowords) {
    lastPicked[[w]] = 0
}

######################################################################
# BACKTRACK TO SOLVE
######################################################################

t = 0
wi = 1
while(wi <= length(cryptowords)) {
    
    cryptoword = cryptowords[wi]
    cryptochars = unlist(strsplit(cryptoword, split=""))
    cryptolen = nchar(cryptoword)
    
    # Pick word from dictionary, assign encoding
    # Any word picked will be consistent with encoding
    # Pick next word in order
    idx = lastPicked[[cryptoword]] + 1
    word = pruneDict[[as.character(cryptolen)]][idx]
    
    # If no more words available, remove previous encoding
    # Reset lastPicked, go to previous word
    if(is.na(word)) {
        
        # Erase last encoding and go back to previous word
        if(wi > 1) {
            # Reset lastPicked for current word
            lastPicked[[cryptoword]] = 0
            
            wi = wi - 1
            lastWord = cryptowords[wi]
            lastChars = unlist(strsplit(lastWord, split=""))
            charsToRemove = lastChars[!lastChars %in% getCryptochars(wi-1)]
            encoding[charsToRemove] = NULL
        } else {
            encoding = clueLetters
        }
        # Set dictionary to previous state
        pruneDict = pruneDictionary(encoding)
        
    } else {
        
        lastPicked[[cryptoword]] = idx
        wordchars = unlist(strsplit(word, split=""))
        charsJustAdded = c()
        for(i in 1:cryptolen) {
            # Don't overwrite letters in encoding
            if(!cryptochars[i] %in% names(encoding)) {
                encoding[cryptochars[i]] = wordchars[i]
                charsJustAdded = c(charsJustAdded,cryptochars[i])
            }
        }
        
        pruneDict = pruneDictionary(encoding)
        
        valid = checkEncoding(encoding)
        # If not valid, remove encoding due to current word
        # Restore pruneDict to previous state
        # Go to next available word in dictionary
        if(!valid) {
            encoding[charsJustAdded] = NULL
            pruneDict = pruneDictionary(encoding)
        } else {
            wi = wi + 1
        }
        
    }
    
    t=t+1
    
    if(t %% 1000 == 0) {
        print(c(wi,word,valid,decode(encoding)))
    }
    
    if(t > 10000) {
        print("Reached max number of iterations")
        break
    }
    
}

# Print final solution
print(c(wi,word,valid,decode(encoding)))


