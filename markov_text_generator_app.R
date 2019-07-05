# Daniel Eubanks
# January 2019
# WorksOfR.wordpress.com

library(shiny)
library(jsonlite)

########## TEXT GENERATOR DATA AND FUNCTIONS

wordList <- paste(readLines(url("https://github.com/eubankjd/WorksOfR/raw/master/nytimes_wordlist.json"),
                            encoding="latin1"),
                  collapse="")
wordList <- fromJSON(wordList)
wordList <- lapply(wordList,unlist)

gen_text <- function(max_words=200,stop_prob=0.5) {
    
    outwords <- c()
    
    puncs <- c(",","\\.","?","!",";",":")
    
    # First pair; pick one that starts with a capital letter
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
    
    # Reassemble output
    output <- paste(outwords,collapse=" ")
    
    # Remove extra spaces around punctuation
    output <- gsub(" ([\\.|,|?|!|;|:]) ","\\1 ",output)
    output <- gsub(" ([\\.|?|!])","\\1",output)
    
    # Catch remaining encoding issues
    output <- gsub("\U0097|\U0096","-",output)
    
    return(output)
    
}

########## SHINY FUNCTIONS

# Define UI for text generator app 
ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                        @import url('//fonts.googleapis.com/css?family=Literota|Cabin:400,700');
                        
                        h1 {
                        font-family: 'Literota', serif;
                        font-weight: bold;
                        font-size: 48pt;
                        }
                        
                        p {
                        font-family: 'Literota', serif;
                        font-size: 12pt;
                        }

                        #text {
                        font-family: 'Literota', serif;
                        font-size: 14pt;
                        }
                        
                        "))
    ),
    
    # App title 
    headerPanel("New York Times Column Generator"),
    
    # Sidebar layout with a input and output definitions 
    sidebarLayout(
        
        # Sidebar panel for inputs 
        sidebarPanel(
            
            # Input: Numeric entry for maximum number of words 
            numericInput(inputId = "max_words",
                         label = "Maximum number of words to generate:",
                         value = 200,
                         step=10),
            
            # Input: Numeric entry for probability of stopping 
            sliderInput(inputId = "stop_prob",
                        label = "Stopping probability:",
                        min = 0,
                        max = 1,
                        value = 0.2,
                        step=0.1),
            
            actionButton(inputId="generate", label="Generate"),
            
            # Adding the new div tag to the sidebar            
            tags$div(tags$br(),
                     tags$p("The text generator uses a second order Markov chain. For more information,
                            see",
                            tags$a(href = "https://worksofr.wordpress.com/2018/12/24/markov-chain-text-generator/", "WorksOfR.")),
                     style="font-size:11px;")
            
        ),
        
        # Main panel for displaying outputs 
        mainPanel(
            
            # Output: generated text
            textOutput("text"),
            
            # Adding the new div tag to the sidebar            
            tags$div(tags$br(),
                     tags$p("These texts are randomly generated and 
                            should not be interpreted as an endorsement of any particular idealogy.
                            The source columns used to generate the text can be found at",
                            tags$a(href = "https://www.nytimes.com/column/paul-krugman", "nytimes.com.")),
                     style="color:gray;font-size:11 px;")
            
        )
    )
)

# Define server logic to call gen_text
server <- function(input, output, session) {
    
    text <- eventReactive(input$generate, {
        stop_prob <- input$stop_prob
        max_words <- input$max_words
        gen_text(max_words,stop_prob)
    })
    
    output$text <- renderText(text())
    
}

shinyApp(ui, server)