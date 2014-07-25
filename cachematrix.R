## The functions below add cache features above the 
## original "solve" function, which is to calculate
## the inversion of an inversible square matrix.

## The first function builds special "vector", which 
## is represented as a list of functions.
## (The input matrix is assumed to be inversible.)

makeCacheMatrix <- function(x = matrix()) {
    rev <- NULL
    set <- function(y) {
        x <<- y
        rev <<- NULL
    }
    get <- function() x
    setrev <- function(reversed) rev <<- reversed
    getrev <- function() rev
    list(set = set, get = get,
         setrev = setrev,
         getrev = getrev)
}


## The second function returns the cached inversed matrix,
## if there's non, it will first calculate and update the 
## cached value with the new result, then return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    rev <- x$getrev()
    if(!is.null(rev)) {
        message("getting cached data")
        return(rev)
    }
    data <- x$get()
    rev <- solve(data, ...)
    x$setrev(rev)
    rev
}
