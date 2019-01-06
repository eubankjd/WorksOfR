# Daniel Eubanks
# January 2019
# WorksOfR.wordpress.com

library(rvest)
library(jsonlite)

# Initialize list to store word combinations
wordList <- list()

########## SCRAPE DATA FROM NYTIMES

urls <- c("https://www.nytimes.com/2018/12/17/opinion/republican-apparatchiks-deep-state.html",
          "https://www.nytimes.com/2018/12/13/opinion/trump-wall-trade-tariffs.html",
          "https://www.nytimes.com/2018/12/10/opinion/trump-gop-authoritarian-states-power-grab.html",
          "https://www.nytimes.com/2018/12/06/opinion/columnists/trade-tariffs-trump.html",
          "https://www.nytimes.com/2018/12/03/opinion/climate-denial-trump-gop.html",
          "https://www.nytimes.com/2018/11/30/opinion/brexit-borders-and-the-bank-of-england-wonkish.html",
          "https://www.nytimes.com/2018/11/29/opinion/maga-trump-manufacturing.html",
          "https://www.nytimes.com/2018/11/26/opinion/climate-change-denial-republican.html",
          "https://www.nytimes.com/2018/11/22/opinion/democrats-obamacare-states.html",
          "https://www.nytimes.com/2018/11/19/opinion/economy-trump-red-blue-states.html",
          "https://www.nytimes.com/2018/11/15/opinion/tax-cut-fail-trump.html",
          "https://www.nytimes.com/2018/11/14/opinion/the-tax-cut-and-the-balance-of-payments-wonkish.html",
          "https://www.nytimes.com/2018/11/12/opinion/truth-virtue-trump-loyalty.html",
          "https://www.nytimes.com/2018/11/09/opinion/what-the-hell-happened-to-brazil-wonkish.html",
          "https://www.nytimes.com/2018/11/08/opinion/midterms-senate-rural-urban.html",
          "https://www.nytimes.com/2018/11/05/opinion/midterms-trump-republicans-autocracy.html",
          "https://www.nytimes.com/2018/11/02/opinion/the-perversion-of-fiscal-policy-slightly-wonkish.html",
          "https://www.nytimes.com/2018/11/01/opinion/republican-party-lies.html",
          "https://www.nytimes.com/2018/10/31/opinion/the-great-center-right-delusion.html",
          "https://www.nytimes.com/2018/10/29/opinion/hate-is-on-the-ballot-next-week.html",
          "https://www.nytimes.com/2018/10/27/opinion/are-the-danes-melancholy-are-the-swedes-sad.html",
          "https://www.nytimes.com/2018/10/25/opinion/trump-republican-hate.html",
          "https://www.nytimes.com/2018/10/22/opinion/khashoggi-saudi-trump-arms-sales.html",
          "https://www.nytimes.com/2018/10/20/opinion/notes-on-global-convergence-wonkish-and-off-point.html",
          "https://www.nytimes.com/2018/10/18/opinion/taxes-medicare-social-security-midterms.html",
          "https://www.nytimes.com/2018/10/11/opinion/republicans-lies-medicare-pre-existing-conditions.html",
          "https://www.nytimes.com/2018/10/08/opinion/gop-trump-kavanaugh-conspiracies-partisan.html",
          "https://www.nytimes.com/2018/10/04/opinion/donald-trump-fred-taxes-fraud.html",
          "https://www.nytimes.com/2018/10/01/opinion/kavanaugh-white-male-privilege.html",
          "https://www.nytimes.com/2018/09/30/opinion/the-economic-future-isnt-what-it-used-to-be-wonkish.html")

for(url in urls) {
    
    print(paste("Reading",url))
    
    # Get the article text
    html <- read_html(url)
    paragraphs <- html_nodes(html,"p.css-1ygdjhk.e2kc3sl0")
    articletext <- paste(html_text(paragraphs), collapse=" ")
    
    # Deal with encoding issues
    articletext <- iconv(articletext,from="utf8",to="utf8")
    articletext <- gsub("\u201c|\u201d","\"",articletext)
    articletext <- gsub("\u2019","'",articletext)
    articletext <- gsub("\u0096|\u0097","-",articletext)
    
    # Separate punctuation from the preceeding word
    # i.e., Consider punctuation marks as their own words
    articletext <- gsub("([A-Za-z0-9])([\\.|?|!|:|;|,]) ", "\\1 \\2 ", articletext)
    
    # Get vector of words in order
    articlewords <- unlist(strsplit(articletext, split="[ ]+"))
    for(i in 1:(length(articlewords)-2)) {
        pair <- paste(articlewords[i],articlewords[i+1])
        third <- articlewords[i+2]
        if(is.null(wordList[[pair]]) || is.na(wordList[[pair]][third])) {
            # If third word doesn't exist yet for pair, add it
            wordList[[pair]][third] <- 1
        } else {
            # Otherwise, increment by 1
            wordList[[pair]][third] <- wordList[[pair]][third] + 1
        }
    }
}

# Convert to JSON; use text editor to adjust encoding
wordListJSON <- toJSON(lapply(wordList, as.list),pretty=TRUE,unbox=TRUE)
write(wordListJSON,"nytimes_wordlist.json")

# Save RData version
saveRDS(wordList,"nytimes_wordlist.RData")