# Daniel Eubanks
# December 2018
# WorksOfR.wordpress.com

########## TRAINING

# Initialize list to store word combinations
wordList <- list()

# Process each file
for(filename in dir(pattern="*.txt")) {
    
    filetext <- paste(readLines(filename), collapse=" ")
    
    # Separate punctuation from the preceeding word
    # i.e., Consider punctuation marks as their own words
    filetext <- gsub("([A-Za-z0-9])([\\.|?|!|:|;|,]) ", "\\1 \\2 ", filetext)
    
    # Get vector of words in order
    filewords <- unlist(strsplit(filetext, split="[ ]+"))
    for(i in 1:(length(filewords)-2)) {
        pair <- paste(filewords[i],filewords[i+1])
        third <- filewords[i+2]
        if(is.null(wordList[[pair]]) || is.na(wordList[[pair]][third])) {
            # If third word doesn't exist yet for pair, add it
            wordList[[pair]][third] <- 1
        } else {
            # Otherwise, increment by 1
            wordList[[pair]][third] <- wordList[[pair]][third] + 1
        }
    }
    
}

########## CREATE NEW TEXT
gen_text <- function(max_words=200,stop_prob=0.5) {
    
    outwords <- c()
    
    puncs <- c(",","\\.","?","!",";",":")
    
    # First pair; pick pair that starts with a capital letter and has no punctuation
    first_words <- grep("^[A-Z][a-z]* [a-z]*",names(wordList),value=TRUE)
    pair <- sample(first_words,1)
    
    outwords <- c(outwords, pair)
    
    word_num <- 1
    for(i in 1:max_words) {
        
        # Get counts of candidate third words
        candidate_thirds <- wordList[[pair]]
        
        # Get probabilities for third word candidates
        candidate_thirds_probs <- candidate_thirds/sum(candidate_thirds)
        
        # If there is no candidate third, break
        if(is.null(wordList[[pair]])) {
            break
        } else {
            third <- sample(names(candidate_thirds),1,prob=candidate_thirds_probs)
            outwords <- c(outwords, third)
        }
        
        # If third is end of sentence, end with probability stop_prob
        if(third %in% c(".","?","!")) {
            p <- runif(1)
            if(p < stop_prob) {
                break
            }
        }

        # Create new pair using second and third words
        pair <- unlist(strsplit(paste(pair,third),split="[ ]+"))[2:3]
        pair <- paste(pair, collapse=" ")
    }
    
    ##### FORMAT TEXT
    
    # Reassemble output
    output <- paste(outwords,collapse=" ")
    
    # Remove extra spaces around punctuation
    output <- gsub(" ([\\.|,|?|!|;|:]) ","\\1 ",output)
    output <- gsub(" ([\\.|?|!])","\\1",output)
    
    return(output)
    
}

gen_text(150,stop_prob=0.2)
