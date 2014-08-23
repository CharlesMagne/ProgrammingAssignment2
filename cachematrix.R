## Given an invertible square matrix, write functions that will calculate
## and store in cache the inverse matrix (inv) if the inverse matrix is not
## already stored in cache. If inv is already in cache, the function will
## retrieve and return it.

## makeCacheMatrix takes an invertible square matrix and returns a list
## of method objects allowing the function cacheSolve to get the matrix, 
## to determine if the inverse matrix is already stored in cache, and if not,
## , to calculate the inverse matrix and superassign it to cache.

makeCacheMatrix <- function(x = matrix()) { # must be invertible matrix
        inv <- NULL # initialize inverse object to NULL
        get <- function() {x} # get function returns the original matrix

# takes value of inv as determined by cacheSolve and supperassigns it 
## to cache
        
        setinv <- function(inv) { 
                inv <<- inv 
                } 
        getinv <- function() { inv } # getinv returns inv from cache
        list(get = get, setinv = setinv, getinv = getinv) # list of method
                                # objects that will be called by cacheSolve
}


## cacheSolve will calculate and store in cache the inverse matrix (inv) 
## if the inverse matrix is not already stored in cache. 
# If inv is already in cache, the function will retrieve and return it.

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
