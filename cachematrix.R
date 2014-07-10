## This routine includes two functions for the purpose of constructing an
## object which stores a matrix along with its inverse. Both functions 
## included here were adapted from Roger Peng's 'makeVector' and 'cachemean'
## functions.

## The function 'makeCacheMatrix' initializes the object. The object itself
## is a list of four functions which 1) set the value of the matrix, 2)
## retrieve the value of the matrix, 3) solve for the inverse of the matrix
## and store it's value, and 4) retrieve the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function 'cacheSolve' check's to see if an inverse matrix has been
## calculated for a 'makeCacheMatrix' object. If the inverse has been 
## calculated the function returns the value, otherwise it is calculated and 
## stored in the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
