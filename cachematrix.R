## The functions here solve for the inverse of a matrix that is stored in the
## cache, and can be recalled from cache if they have already been solved.

## this function creates a list with set, get, setinverse, and getinverse

## set creates the matrix, you can either call it yourself, or allow the function
## to call it from the original "makeCacheMatrix" function
## get calls the matrix
## setinverse solves for the inverse of the matrix
## getinverse calls the inverse, that setinverse solived

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function calculates the inverse (using solve function) 
## and also stores the result in "m"
## if the function is run and the inverse has already been calculated
## it just reads from the cache "m"

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m   ## Return a matrix that is the inverse of 'x'
}