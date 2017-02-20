## The below functions take a matrix as an input, calculates
## the inverse of a matrix and outputs the inverse. Before calculating
## the inverse the function checks if the inverse was already calculated 
## and saved in cache, If the inverse exists in cache the function gets the 
## inverse of the matrix from cache and outputs the result

## makeCacheMatrix is a function that creates a list of functions
## to set value of a matrix, get value of a matrix, set value of inverse, 
## get value of inverse

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve is a function that calculates the inverse of matrix created by makeCacheMatrix
## It checks if the result is already available in cache, if available, it gets the result from cache
## if not available in cache it recalculates
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
    
    
}
ss <- matrix(rnorm(25), nrow = 5, ncol = 5, byrow = FALSE,
             dimnames = NULL)

MC <- makeCacheMatrix(ss)
cacheSolve (MC)

