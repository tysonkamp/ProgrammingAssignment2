## This is a helper function that encapsulates a matrix and a function to 
## perform on that matrix.  It stores the result of the function "F", which 
## can be null, and is null, until set by a caller.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setF <- function(f) m <<- f
    getF <- function() m
    list(set = set, get = get,
         setF = setF,
         getF = getF)
}


## cacehSolve uses the matrix cache helper function to store the 
## result of the matrix inversion.  This way, we only have to calulate the
## actual inverse once and subsequently can refer to the saved value.
##
## Sample usage: second call returns cached value
##    >myMat <- matrix(c(1,1,1,2,0,2,2,1,1),3,3)
##    >mcm <- makeCacheMatrix(myMat)
##    >cacheSolve(mcm)
##    >cacheSolve(mcm)
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getF()
    if(!is.null(m)) {
        message("Getting cached matrix inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setF(m)
    m
}
