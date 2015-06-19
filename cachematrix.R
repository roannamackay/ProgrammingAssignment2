## Calculate, store and retrieve the inverse of a "special matrix"

## Stores the inverse of the "special matrix"

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    invisible(list(set = set, get = get,setinverse=setinverse,getinverse=getinverse))
}


## Calculates the inverse of the "special matrix"- note not suitable for normal matrices

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
