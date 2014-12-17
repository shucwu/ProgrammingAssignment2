# Caching the inversion of a matrix

## makeCacheMatrix creates a list containing a function for each of
## the following operations
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverted matrix
##  4. get the value of the inverted matrix


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


## cacheSolve calcuates the value of the inverted matrix. 
## return cached inverted matrix if already calculated
## otherwise, calculate the inverted matrix and put into cache
## via the setinv function

cacheSolve <- function(x, ...) {
    
    # check to see if an inversion is in the cache
    # if so, return it
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached inverted matrix")
        return(inv)
    }
    
    # get the matrix and calculates inversion
    # and store the result in cache
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)

    ## Return the inverted matrix
    inv
}
