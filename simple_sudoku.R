# Daniel Eubanks
# August 2018
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

########## FUNCTIONS TO SOLVE CELL


solveCell = function(rowNum, colNum) {
    
    currValue = puzzle[rowNum, colNum]
    
    if(currValue == 9) {
        puzzle[rowNum,colNum] <<- 0
        return(FALSE)
    }
    
    for(guess in (currValue+1):9) {
        isSolved = checkValue(rowNum,colNum,guess)
        
        if(isSolved) {
            puzzle[rowNum,colNum] <<- guess
            return(TRUE)
        }
    }
    
    puzzle[rowNum,colNum] <<- 0
    return(FALSE)
}


########## INPUT

# Puzzle to solve. 0 represents empty.
puzzle = rbind(   c(4, 9, 0,     0, 0, 0,     2, 3, 0),
                  c(5, 0, 0,     0, 4, 3,     6, 0, 0),
                  c(2, 0, 0,     0, 0, 0,     0, 0, 0),
                  
                  c(9, 0, 6,     7, 8, 0,     0, 0, 0),
                  c(1, 0, 0,     9, 3, 6,     0, 0, 5),
                  c(0, 0, 0,     0, 1, 2,     9, 0, 3),
                  
                  c(0, 0, 0,     0, 0, 0,     0, 0, 6),
                  c(0, 0, 9,     3, 5, 0,     0, 0, 4),
                  c(0, 4, 5,     0, 0, 0,     0, 1, 7))

########## SOLVE

toSolve = which(puzzle == 0, arr.ind=TRUE)
i = 1
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


end = Sys.time()

print(end-start)

