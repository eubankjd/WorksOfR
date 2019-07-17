# Daniel Eubanks
# July 2019
# WorksOfR.wordpress.com

# Functions -----------------------------------------------------------------------------
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

# Run game ------------------------------------------------------------------------------

# Initialize state
state <- matrix(c("_", "_", "_", "_", "_",
                  "_", "_", "_", "_", "_",
                  "_", "_", "_", "_", "_",
                  "_", "_", "_", "_", "_",
                  "_", "_", "_", "_", "_"), nrow = ROW, byrow = TRUE)
state <- matrix("_", nrow = ROW, ncol = COL)

# Make X move in column 4; change column to move elsewhere
state <- drop_in_col(state, 4)

# Get successor states for O
successor_states <- get_successor_states(state, "O")
# Get minimax value for succesor state
state_values <- lapply(successor_states, get_minimax_value, 
                       agent = "X", depth = 3, min = -Inf, max = Inf)
# Pick the move that minimizes the value of state
state <- successor_states[[which.min(state_values)]]
# Print state and check if O has won
print(state)
if (is_win(state, "O")) print("O Wins!")