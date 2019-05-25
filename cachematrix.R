## the functions both create a special matrix that have functions inside them to get and set values of the matrix and the inverse.
## the first one sets up the cache in which the values of the matrix and the inverse will be stored. The second actually calculates it
## if it hasn't yet been calculated. It does this by using values from the cache.

## creates a special matrix that has functions to get and set the values of the matrix as well as the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## calculates the inverse of the matrix "x" by checking first if the inverse has already been calculated. If so, then it simply 
##retrieves the mean from the cache without doing any calculations. Otherwise, it calculates the inverse and sets its value in the
##cache using the "setInverse" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    result <- x$get()
    inv <- solve(result, ...)
    x$setInverse(inv)
    inv
}
