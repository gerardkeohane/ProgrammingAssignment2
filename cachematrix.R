## The function makeCacheMatrix() is used to set and get a matrix, and to 
## store and retrieve the inverse of the matrix via setinverse() and getinverse()
## The function cacheSolve() is used to return an inverse of the input matrix and
## to store that inverse matrix if it has not already been calculated. 

## This function is used to set and get a matrix and the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function is used to return the inverse of a matrix, if the inverse
## has not already been calculated, the calculation is made and the matrix is 
## cached using the setinverse() matrix in the makeCacheMatrix() function.
## If the matrix is already calculated it is simply retrieved using the getinverse() 
## function and returned. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
