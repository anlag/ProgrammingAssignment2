## Solving for the inverse of a given (non-singular) matrix
## Subsequent calls will use a cached value for the inverse, if available

## Initialize a matrix environment with caching, define interface functions
makeCacheMatrix <- function(x = matrix()) {
    # the inverse starts out undefined
    inverse <- NULL
    
    # save a new matrix
    set <- function(y) {
        x <<- y
        # the inverse of a new matrix will begin undefined
        inverse <<- NULL
    }
    
    # return the currently loaded matrix
    get <- function() x
    
    # save the inverse of the current matrix
    setinv <- function(inv) inverse <<- inv
    
    # return the inverse
    getinv <- function() inverse
    
    # when initialized, return the four subfunctions of this environment
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Takes a predefined matrix (from makeCacheMatrix) and returns its inverse
## Will use a cached value if available, if not it will be solved for and saved
cacheSolve <- function(x, ...) {
    # ask the matrix environment for a cached inverse
    inverse <- x$getinv()
    
    # check if the inverse is cached already, if so just return it
    if(!is.null(inverse)) {
        message("matrix inverse found in cache! let's use it")
        return(inverse)
    }
    
    # no cached value found, solve for the inverse
    message("matrix inverse NOT found in cache! we'll have to solve for it")
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    
    # save the newly solved for inverse to the matrix environment
    x$setinv(inverse)
    
    # return the inverse as requested
    inverse
}
