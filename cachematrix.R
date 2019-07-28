## Put comments here that give an overall description of what your
## functions do

## Create a matrix object, that store it reverse in cache

makeCacheMatrix <- function(x = matrix()) {
    revMatrix <- NULL

    set <- function(y) {
        x <<- y
        revMatrix <<- NULL
    }

    get <- function() x
    
    setReverse <- function(rev) revMatrix <<- rev
    
    getReverse <- function() revMatrix
    
    list(set = set, get = get,  setReverse = setReverse, getReverse = getReverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    revMatrix <- x$getReverse()
    if(!is.null(revMatrix)) {
        message("getting cached data")
        return(revMatrix)
    }

    data <- x$get()
    revMatrix <- solve(data)

    x$setReverse(revMatrix)
    revMatrix
}

