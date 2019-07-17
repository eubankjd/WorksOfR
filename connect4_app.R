# Daniel Eubanks
# July 2019
# WorksOfR.wordpress.com

library(shiny)
library(data.table)
library(ggplot2)

# Minimax algorithm ---------------------------------------------------------------------

COL <- 7
ROW <- 5
WIN <- 4

# Get a list of all successor states
get_successor_states <- function(state, agent) {
    
    open_cols <- which(apply(state, 2, function(col) sum(col == "_") > 0))
    open_row <- sapply(open_cols, function(col) max(which(state[, col] == "_")))
    successor_states <- mapply(function(row, col) {
        ss <- state
        ss[row, col] <- agent
        return(list(ss))
    }, row = open_row, col = open_cols)
    return(successor_states)
}

# Determine if current state is a win state for agent by iterating over WIN x WIN subgrids
is_win <- function(state, agent) {
    
    max_i <- ROW - WIN + 1
    max_j <- COL - WIN + 1
    for (i in 1:max_i) {
        for (j in 1:max_j) {
            subgrid <- state[i:(i + WIN - 1), j:(j + WIN - 1)]
            if (sum(subgrid != "_") < WIN) {
                next
            }
            row_win <- any(apply(subgrid, 1, function(row) all(row == agent)))
            col_win <- any(apply(subgrid, 2, function(col) all(col == agent)))
            diag_win <- all(diag(subgrid) == agent) | all(diag(subgrid[WIN:1,]) == agent)
            win <- row_win | col_win | diag_win
    
            if (win) {
                return(TRUE)
            }
        }
    }
    return(FALSE)
}

# Return the value of a state
get_state_value <- function(state) {
    
    if (is_win(state, "X")) {
        return(1000)
    }
    
    if (is_win(state, "O")) {
        return(-1000)
    }
    
    # Heuristic
    max_hz_run <- 0
    for (row in 1:ROW) {
        for (offset in 0:(COL - WIN)) {
            num_X <- sum(state[row, (1 + offset):(WIN + offset)] == "X")
            num_O <- sum(state[row, (1 + offset):(WIN + offset)] == "O")
            if (num_O != 0) {
                num_X <- 0
            }
            if (num_X == WIN - 1) {
                return(WIN - 1)
            }
            max_hz_run <- max(max_hz_run, num_X)
            
        }
    }
    
    max_vt_run <- 0
    for (col in 1:COL) {
        for (offset in 0:(ROW - WIN)) {
            num_X <- sum(state[(1 + offset):(WIN + offset), col] == "X")
            num_O <- sum(state[(1 + offset):(WIN + offset), col] == "O")
            if (num_O != 0) {
                num_X <- 0
            }
            if (num_X == WIN - 1) {
                return(WIN - 1)
            }
            max_vt_run <- max(max_vt_run, num_X)
        }
    }
    return(max(max_hz_run, max_vt_run))
}

# Get minimax value for a state using alpha-beta pruning
get_minimax_value <- function(state, agent, depth, min, max) {
    
    game_over <- is_win(state, "X") | is_win(state, "O") | (sum(state == "_") == 0)
    if (game_over | depth == 0) {
        state_value <- get_state_value(state)
        return(state_value)
    }
    
    if (agent == "X") {
        v <- min
        successor_states <- get_successor_states(state, "X")
        for (state in successor_states) {
            vprime <- (1 - 0.5 ^ depth) * get_minimax_value(state, "O", depth - 1, v, max)
            if (vprime > v) {
                v <- vprime
            }
            if (v > max) {
                return(max)
            }
        }
        return(v)
    }
    
    if (agent == "O") {
        v <- max
        successor_states <- get_successor_states(state, "O")
        for (state in successor_states) {
            vprime <- (1 - 0.5 ^ depth) * get_minimax_value(state, "X", depth - 1, min, v)
            if (vprime < v) {
                v <- vprime
            }
            if (v < min) {
                return(min)
            }
        }
        return(v)
    }
}

# Update state given X column selection
drop_in_col <- function(state, col) {
    open_row <- sapply(col, function(col) max(which(state[, col] == "_")))
    state[open_row, col] <- "X"
    return(state)
}

# Shiny code ----------------------------------------------------------------------------

# Define UI for application
ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("
            @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
            
            h1 {
            font-family: 'Lobster', cursive;
            font-weight: 500;
            font-size: 48pt;
            line-height: 1.1;
            color: #006495;
            }

            #win_msg {
            font-family: 'Lobster', cursive;
            font-size: 36pt;
            color: #006495;
            }

        "))
    ),
    
    fluidRow(align = "center", 
             headerPanel("Connect Four"),
             helpText("Click the button above the column you want to play in, 
                      then wait for the computer to make its move."),
             HTML("<br />")),
    
    fluidRow(
        column(12, align = "center",
               lapply(1:COL, function(i) {
                   actionButton(inputId = paste0('col', i),
                                label = paste('Column', i))
               })
        )
    ),
    
    fluidRow(
        column(12,
               plotOutput("grid"))
    ),
    
    fluidRow(
        column(12, align = "center", textOutput("win_msg"))
    ),
    
    fluidRow(
        column(12, align = "center", 
               div(br(),
                   p("For more information, see",
                     a(href = "https://worksofr.wordpress.com/2019/07/05/connect-four-minimax-algorithm/", "WorksOfR.")),
                   style="font-size:11px; color: gray;"))
    )
    
)



# Define server logic
server <- function(input, output) {
    
    # Intialize variables
    state <- matrix("_", nrow = ROW, ncol = COL)
    game_over <- FALSE
    output$win_msg <- renderText("")
    blueTurn <- FALSE
    
    # Show empty board at startup
    output$grid <- renderPlot(plot_state(state))
    
    # Observe each button; plot red move
    lapply(1:COL, function(i) {
        
        observeEvent(input[[paste0("col", i)]], {
            state <<- drop_in_col(state, i)
            output$grid <- renderPlot(plot_state(state))
            x_win <- is_win(state, "X") 
            if (!x_win) {
                blueTurn <<- TRUE
            } else {
                output$win_msg <- renderText("Red wins!")
            }
        })
        
    })
    
    # Check if it's blue's turn, then plot blue move
    observe({
        
        invalidateLater(200)
        if (blueTurn) {
            
            withProgress(message = "Computing . . .", {
                
                successor_states <- get_successor_states(state, "O")
                incProgress(0.25)
                state_values <- lapply(successor_states, get_minimax_value, 
                                       agent = "X", depth = 4, min = -Inf, max = Inf)
                incProgress(0.50)
                state <<- successor_states[[which.min(state_values)]]
                incProgress(0.25)
                output$grid <- renderPlot(plot_state(state))
                blueTurn <<- FALSE
                
                o_win <- is_win(state, "O") 
                if (o_win) {
                    output$win_msg <- renderText("Blue wins!")
                }
                
            })
        }
    })
}

plot_state <- function(state) {
    
    plot_df <- as.data.frame(state)
    names(plot_df) <- 1:COL
    plot_df$row <- 1:nrow(state)
    plot_df <- melt(plot_df,
                    id.vars = "row",
                    variable.name = "col",
                    value.name = "agent")
    
    plot_df$col <- as.numeric(as.character(plot_df$col))
    
    ggplot(subset(plot_df, agent != "_"), aes(col, row)) +
        geom_point(aes(color = agent), size = 20) +
        geom_vline(xintercept = seq(0.5, COL + 0.5, by = 1), 
                   size = 2, color = "#006495") +
        geom_hline(yintercept = seq(0.5, ROW + 0.5, by = 1), 
                   size = 2, color = "#006495") +
        scale_y_reverse() +
        scale_color_manual(values = c("#006495", "#F2635F"),
                           limits = c("O", "X")) +
        theme(panel.grid = element_blank(),
              panel.background = element_rect(fill = "#ffeb7d"),
              panel.border = element_rect(color = "#006495", fill = NA, size = 4),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = "none") +
        coord_fixed(.90, expand = FALSE)
}


# Run the application
shinyApp(ui = ui, server = server)

