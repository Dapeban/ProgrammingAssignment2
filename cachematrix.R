## This set of functions is used to create and store a square, invertable matrix
## of which we are going to calculate the inverse

## This first function (makeCacheMatrix) stores the matrix that we wish to invert,
## also caches the inverted matrix's result to be used further on:

makeCacheMatrix <- function(x = matrix()) {
    
    ## Step 1: Verifies that the matrix is square (equal number of rows and collumns)
    
    if (nrow(x)!=ncol(x)) {
        print('Not a square matrix, please try again')
        stop()
        
        ## Step 2: Verifies that the matrix is invertible (determinant not equal to Zero)
        
    } else if (det(x)==0) {
        print("Not an invertible matrix, please try again")
        stop()
    }
    
    ## Step 3: Creates a list of 4 functions to be used in the next function.
    ## Function 1 (set): Sets the value of the matrix to be inverted 
    ## and makes sure that anything that was cached previously is reset to NULL
    ## Function 2 (get): Allows the user to get the non inverted matrix
    ## Function 3 (setinv): Stores the inverted matrix that will be created later on
    ## Function 4 (getinv): Allows the user to get the inverted matrix
    
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(solve) invx <<- solve
    getinv <- function() invx
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Creates the inverted matrix and stores it in cache (memory).

cacheSolve <- function(x, ...) {
    
    ## Step 1: Checks if there is any cached matrix previously stored
    
    invx <- x$getinv()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    ## Step 2: If there is no cached matrix, 
    ## we proceed to calculate the inverted matrix with the help of the solve() function:
    
    ## Getting the non inverted matrix and store it in the 'data' object
    data <- x$get()
    ## Solving the matrix, that is, obtaining the inverse of the matrix   
    invx <- solve(data)
    ## We store (or cache) the inverted matrix in the 3rd position of the list designed for this purpose
    x$setinv(invx)
    ## Finally, we get to see our inverted matrix
    invx
}