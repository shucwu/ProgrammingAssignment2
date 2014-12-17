## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverted matrix
    setinv <- function(inverse) inv <<- inverse 
    
    # get the value of the inverted matrix
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached inverted matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)

    ## Return a matrix that is the inverse of 'x'
    inv
}
