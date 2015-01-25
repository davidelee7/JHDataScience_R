## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix function is designed to be nested within the cacheSolve
## function.  It creates four functions and puts them into a list so they can be 
## called by the cacheSolve function.  The four functions created within the 
## makeCacheMatrix function are used by the wrapper cacheSolve function to invert 
## matrix.  They are also used to see if the inverted matrix has already been 
## created and retained in the cache.  If so, the inverted matrix in the cache is 
## retrieved rather than inverting the matrix, which reduces processing resources.
## The use of the functions is: cacheSolve(makeCacheMatrix(m)) where m is a matrix
## to be inverted.

## Write a short comment describing this function
## The makeCacheMatrix function defines four functions for use in the above
## described process:
##      - set, which will create the x and im objects
##      - get, which will retrieve the x object
##      - setinverse, which will instantiate the matrix object, im, in the parent
##        scope of the function, which will usually be the global enviornment.
##      - getinverse, which will retrieve the im object


makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## The cacheSolve function returns a matrix that is the inverse of 'x'.
## It uses the functions created in the makeCacheMatrix function above
## to do the processing in the following steps:
##      1. Get the im object.
##      2. If the im object is not null then use the im object (inverted matrix).
##      3. If the im object is null then retrieve the x object (original matix).
##      4. Create the inverted matrix (im) from the matrix (x) object.
##      5. Instantiate the inverted matrix in the parent scope (global enviornment).
##      6. Return the inverted matrix (im object).


cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}