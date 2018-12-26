library(ggplot2)
start = Sys.time()

########## FUNCTIONS FOR CHECKING VALUES
checkRow = function(rowNum, value) {
    for(colNum in 1:9) {
        if(workingPuzzle[rowNum,colNum] == value) {
            return(FALSE)
        }
    }
    return(TRUE)
}

checkCol = function(colNum, value) {
    for(rowNum in 1:9) {
        if(workingPuzzle[rowNum,colNum] == value) {
            return(FALSE)
        }
    }
    return(TRUE)
}

checkBox = function(rowNum, colNum, value) {
    rowNum = rowNum - (rowNum - 1) %% 3
    colNum = colNum - (colNum - 1) %% 3
    for(rowOffset in 0:2) {
        for(colOffset in 0:2) {
            if(workingPuzzle[rowNum + rowOffset, colNum + colOffset] == value) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

checkValue = function(rowNum, colNum, value) {
    row = checkRow(rowNum, value)
    if(!row) {
        return(FALSE)
    }
    column = checkCol(colNum, value)
    if(!column) {
        return(FALSE)
    }
    box = checkBox(rowNum, colNum, value)
    if(!box) {
        return(FALSE)
    }
    return(TRUE)
}

########## FUNCTIONS FOR MAINTAINING DOMAINS

hasEmptyDomains = function() {
    return(!all(apply(domains, c(1,2), any)))
}

updateDomain = function(updateRow, updateCol) {
    for(rowNum in 1:9) {
        if(puzzle[rowNum,updateCol] != 0) {
            next
        }
        for(value in 1:9) {
            domains[rowNum,updateCol,value] <<- checkValue(rowNum,updateCol,value)
        }
        domains[rowNum,updateCol,workingPuzzle[rowNum,updateCol]] <<- TRUE
    }
    
    for(colNum in 1:9) {
        if(puzzle[updateRow,colNum] != 0) {
            next
        }
        for(value in 1:9) {
            domains[updateRow,colNum,value] <<- checkValue(updateRow,colNum,value)
        }
        domains[updateRow,colNum,workingPuzzle[updateRow,colNum]] <<- TRUE
    }
    
    updateRow = updateRow - (updateRow - 1) %% 3
    updateCol = updateCol - (updateCol - 1) %% 3
    for(rowOffset in 0:2) {
        for(colOffset in 0:2) {
            rowNum = updateRow + rowOffset
            colNum = updateCol + colOffset
            if(puzzle[rowNum,colNum] != 0) {
                next
            }
            for(value in 1:9) {
                domains[rowNum,colNum,value] <<- checkValue(rowNum,colNum,value)
            }
            domains[rowNum,colNum,workingPuzzle[rowNum,colNum]] <<- TRUE
        }
    }
}

########## FUNCTIONS TO SOLVE CELL
logicFill = function() {
    needSweep = TRUE
    while(needSweep) {
        
        needSweep = FALSE
        for(rowNum in 1:9) {
            for(colNum in 1:9) {
                # Don't try to fill a cell that comes pre-filled
                if(puzzle[rowNum,colNum] != 0) {
                    next
                }
                # This means there is only one possibility for the cell
                if(sum(domains[rowNum,colNum,]) == 1) {
                    puzzle[rowNum,colNum] <<- which(domains[rowNum,colNum,])
                    workingPuzzle[rowNum,colNum] <<- which(domains[rowNum,colNum,])
                    print(paste("Domain logic fill!", rowNum, colNum))
                    updateDomain(rowNum,colNum)
                    # Any time we make a change, we need to sweep again
                    needSweep = TRUE
                    
                } else {
                    # This checks if the current cell is the only place a value could go in a row, column, or box
                    for(value in which(domains[rowNum,colNum,])) {
                        boxRow = rowNum - (rowNum - 1) %% 3
                        boxCol = colNum - (colNum - 1) %% 3
                        
                        uniqueBox = (sum(domains[boxRow:(boxRow+2),boxCol:(boxCol+2),value]) == 1)
                        uniqueRow = (sum(domains[rowNum,,value]) == 1)
                        uniqueCol = (sum(domains[,colNum,value]) == 1)
                        
                        if(uniqueBox | uniqueRow | uniqueCol) {
                            puzzle[rowNum,colNum] <<- value
                            workingPuzzle[rowNum,colNum] <<- value
                            domains[rowNum,colNum,] <<- FALSE
                            domains[rowNum,colNum, value] <<- TRUE
                            print(paste("Box/Row/Column logic fill!", rowNum, colNum))
                            updateDomain(rowNum,colNum)
                            needSweep = TRUE
                            # No need to check the other values; if the condition is true, we're done with this cell
                            break
                        }
                    }
                    
                }
            }
        }
    }
}

solveCell = function(rowNum, colNum) {
    cellDomain = which(domains[rowNum,colNum,])
    currValue = workingPuzzle[rowNum, colNum]
    
    guesses = cellDomain[cellDomain > currValue]
    
    for(guess in guesses) {
        workingPuzzle[rowNum,colNum] <<- guess
        updateDomain(rowNum,colNum)
        
        if(!hasEmptyDomains()) {
            return(TRUE)
        }
    }
    
    workingPuzzle[rowNum,colNum] <<- 0
    updateDomain(rowNum,colNum)
    return(FALSE)
}


########## INPUT

# Puzzle to solve. 0 represents empty.
puzzle = rbind(   c(0, 6, 0,     0, 0, 5,     0, 0, 7),
                  c(0, 5, 1,     0, 0, 0,     4, 8, 0),
                  c(0, 7, 0,     0, 0, 4,     0, 0, 0),
                  
                  c(7, 0, 0,     0, 3, 0,     1, 0, 0),
                  c(0, 0, 0,     9, 0, 6,     0, 0, 0),
                  c(0, 0, 9,     0, 7, 0,     0, 0, 8),
                  
                  c(0, 0, 0,     2, 0, 0,     0, 9, 0),
                  c(0, 1, 8,     0, 0, 0,     6, 5, 0),
                  c(2, 0, 0,     6, 0, 0,     0, 7, 0))

# rbind(   c(0, 0, 0,     0, 0, 0,     0, 0, 0),
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0),
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0),
# 
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0),
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0),
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0),
# 
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0),
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0),
#          c(0, 0, 0,     0, 0, 0,     0, 0, 0))

workingPuzzle = puzzle
origPuzzle = puzzle


########## SETUP

# Initialize domains
domains = array(TRUE, dim=c(9,9,9))

for(rowNum in 1:9) {
    for(colNum in 1:9) {
        # If not blank, only one value is TRUE in domain
        if(puzzle[rowNum,colNum] != 0) {
            domains[rowNum,colNum,] = FALSE
            domains[rowNum,colNum,puzzle[rowNum,colNum]] = TRUE
        } else {
            updateDomain(rowNum,colNum)
        }
    }
}

# Fill as much as possible with logic
logicFill()

# Then use backtracking
blankCells = which(puzzle == 0, arr.ind=TRUE)
if(length(blankCells) != 0) {
    
    # Order by domain size
    domainSize = sapply(1:nrow(blankCells), function(i) {
        sum(domains[blankCells[i,1],blankCells[i,2],])
    })
    toSolve = blankCells[order(domainSize),]
    
    i = 1
    numGuesses = 0
    while(i <= nrow(toSolve)) {
        numGuesses = numGuesses +1
        rowNum = toSolve[i,1]
        colNum = toSolve[i,2]
        success = solveCell(rowNum,colNum)
        if(success) {
            i = i+1
        } else {
            i = i-1
        }
    }
    
    print(paste("Guesses:",numGuesses))
}


end = Sys.time()

print(end-start)

# Plot result

type = rep("Backtracking",81)
type[origPuzzle!=0] = "Given"
type[as.numeric(puzzle)!=0 & origPuzzle == 0] = "Logic"

df = data.frame(row=rep(9:1,times=9),
                col=rep(1:9,each=9),
                num=as.numeric(workingPuzzle),
                type=type)
df$type = factor(df$type,levels=c("Backtracking","Given","Logic"))

ggplot(df, aes(y=row, x=col)) + 
    geom_point(aes(color=type),shape=15) +
    geom_raster(fill="beige") + 
    geom_text(aes(label=num,color=type), size=10, show.legend = FALSE) +
    coord_fixed(xlim=c(0.5,9.5), ylim=c(0.5,9.5)) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
    geom_vline(xintercept=0.5:9.5, size=.5, color="grey") + 
    geom_hline(yintercept=0.5:9.5, size=.5, color="grey") +
    geom_vline(xintercept=c(0.5,3.5,6.5,9.5), size=1, color="sienna") + 
    geom_hline(yintercept=c(0.5,3.5,6.5,9.5), size=1, color="sienna") +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_color_manual(values=c("slateblue","firebrick","black"),
                       breaks=c("Given","Logic","Backtracking"),
                       drop=FALSE,
                       name="Solution Type") +
    xlab(NULL) + 
    ylab(NULL)

ggsave("./R Scripts/WorksOfR/Graphics/solved_enhanced_sudoku.png", height=5,width=6)

