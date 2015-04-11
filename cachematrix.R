## These functions create a storage container for a matrix and its inverse.
## To be honest, I have a difficult time understanding what use this function is.
## If I don't want to recalculate, I just store it. All that this is doing is making
## that storage into a new property of an object rather than a separate object.

## This function is an object that contains the matrix and the inverse of the matrix, 
## and it has functions that set/return the values.
## Use with: 
## x<-makeCacheMatrix() to make a blank cached matrix object.
## x$set(matrix(data,nrows,ncols)) to set the matrix
## x$get() to return the matrix
## x$setinv(matrix) to set a matrix that is treated as the inverse of the matrix
## x$getinv() to retrieve the inverse of the matrix
## However, since there is nothing to force the getinv() to actually return the inverse,
## in fact, it could be absolutely any OTHER matrix (makeCacheMatrix just stores 2 matrices
## and allows retreival), it is impossible to guarantee that it is what it is doing.
## This is silly.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invmat) inv <<- invmat
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function works solely with the makeCacheMatrix function to store the inverse
## of the matrix.
## Use it with cacheSolve(makeCacheMatrix())

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
