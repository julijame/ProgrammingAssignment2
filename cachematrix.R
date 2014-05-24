## The function creates an object of specific functions to create calculating 
## inverse of the matrix easier

## There will be 4 functions created: set, get, setinverse, getinverse. 
## Functions are planned to be called from cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {  ## used to reset the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x  ## returns the initial matrix itself
    setinverse <- function(inverse) m <<- inverse ## caches the inverse matrix
    getinverse <- function() m ## returns the inverse matrix itself
    list(set = set, get = get,      ## creates a special object to be used
        setinverse = setinverse,    ## by cacheSolve
        getinverse = getinverse)
}


## This function calculates the inverse for the matrix.
## However if the inverse is already found in cache, it is not being
## recalculated, but reused.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()  ## assigns NULL or previously calculated inverse
    if(!is.null(m)) {    ## checks if inverse has already been calculated
        message("getting cached data")     ## and can be reused
        return(m)
    }
    data <- x$get()       ## assigns initial matrix to data
    m <- solve(data, ...) ## calculates inverse matrix
    x$setinverse(m)       ## caches inverse matrix
    m
    
}
