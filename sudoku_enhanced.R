# Daniel Eubanks
# September 2018
# WorksOfR.wordpress.com

start = Sys.time()

########## FUNCTIONS FOR CHECKING VALUES

checkRow = function(rowNum, value) {
    for(colNum in 1:9) {
        if(puzzle[rowNum,colNum] == value) {
            return(FALSE)
        }
    }
    return(TRUE)
}

checkCol = function(colNum, value) {
    for(rowNum in 1:9) {
        if(puzzle[rowNum,colNum] == value) {
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
            if(puzzle[rowNum + rowOffset, colNum + colOffset] == value) {
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

updateDomain = function(updateRow, updateCol) {
    
    for(rowNum in 1:9) {
        if(puzzle[rowNum,updateCol] != 0) {
            next
        }
        for(value in 1:9) {
            domains[rowNum,updateCol,value] <<- checkValue(rowNum,updateCol,value)
        }
    }
    
    for(colNum in 1:9) {
        if(puzzle[updateRow,colNum] != 0) {
            next
        }
        for(value in 1:9) {
            domains[updateRow,colNum,value] <<- checkValue(updateRow,colNum,value)
        }
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
        }
    }
}

########## FUNCTIONS TO SOLVE CELLS

logicFill = function() {
    needSweep = TRUE
    while(needSweep) {
        needSweep = FALSE
        for(rowNum in 1:9) {
            for(colNum in 1:9) {

                if(puzzle[rowNum,colNum] != 0) {
                    next
                }

                if(sum(domains[rowNum,colNum,]) == 1) {
                    puzzle[rowNum,colNum] <<- which(domains[rowNum,colNum,])
                    updateDomain(rowNum,colNum)
                    needSweep = TRUE
                    
                } else {

                    for(value in which(domains[rowNum,colNum,])) {
                        boxRow = rowNum - (rowNum - 1) %% 3
                        boxCol = colNum - (colNum - 1) %% 3
                        
                        uniqueBox = (sum(domains[boxRow:(boxRow+2),boxCol:(boxCol+2),value]) == 1)
                        uniqueRow = (sum(domains[rowNum,,value]) == 1)
                        uniqueCol = (sum(domains[,colNum,value]) == 1)
                        
                        if(uniqueBox | uniqueRow | uniqueCol) {
                            puzzle[rowNum,colNum] <<- value
                            domains[rowNum,colNum,] <<- FALSE
                            domains[rowNum,colNum, value] <<- TRUE
                            updateDomain(rowNum,colNum)
                            needSweep = TRUE
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
    currValue = puzzle[rowNum, colNum]
    
    guesses = cellDomain[cellDomain > currValue]
    
    for(guess in guesses) {
        puzzle[rowNum,colNum] <<- guess
        updateDomain(rowNum,colNum)
        
        hasEmptyDomains = !all(apply(domains, c(1,2), any))
        if(!hasEmptyDomains) {
            return(TRUE)
        }
    }
    
    puzzle[rowNum,colNum] <<- 0
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

########## SETUP AND SOLVE

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

logicFill()

toSolve = which(puzzle == 0, arr.ind=TRUE)
if(length(toSolve) != 0) {
    
    # Order by domain size
    domainSize = sapply(1:nrow(toSolve), function(i) {
        sum(domains[toSolve[i,1],toSolve[i,2],])
    })
    toSolve = toSolve[order(domainSize),]
    
    i = 1
    while(i <= nrow(toSolve)) {
        rowNum = toSolve[i,1]
        colNum = toSolve[i,2]
        success = solveCell(rowNum,colNum)
        if(success) {
            i = i+1
        } else {
            i = i-1
        }
    }
}

end = Sys.time()

print(end-start)