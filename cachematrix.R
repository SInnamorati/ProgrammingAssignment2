## These functions are used to:
##  (1) cache the inverse of a matrix, and
##  (2) retrieve the cached value

##-----------------------------------------------------------------------------
## The 'makeCacheMatrix' function creates a special matrix object for which its
##  inverse can be cached.
##  Input:  a square, invertible matrix
##  Output: a list containing functions to:
##      o Set the value of the matrix
##      o Get the value of the matrix
##      o Set the value of the matrix's inverse
##      o Get the value of the matrix's inverse

makeCacheMatrix <- function(MatrixIn = matrix()) {
    
    inverse <- NULL                    # resets inverse every time
    
    set <- function(y){                # sets the value of the matrix
        MatrixIn <<- y
        inverse <<- NULL
    }
    
    get <- function() {MatrixIn}       # returns the original matrix
    
    setinverse <- function(solve) {    # calculates & stores the inverse
        inverse <<- solve
    }
    
    getinverse <- function() {inverse} # returns the cached inverse value
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
##-----------------------------------------------------------------------------

## The 'cacheSolve' function computes the inverse of the special matrix
##      returned by the 'makeCacheMatrix' above.
##      o If the matrix has not changed and the inverse has already been
##        calculated, the inverse value is retrieved from the cache;
##      o If the inverse has not yet been calculated or the matrix has changed, 
##        the inverse is calculated.
##  Input:  a matrix whose inverse is cacheable, created by the
##      'makeCacheMatrix' function above
##  Output: the inverse of the matrix. If the inverse is retrieved from the 
##      cache instead of being calculated, a message "Retrieving cached data"
##      is also printed.

cacheSolve <- function(SpecMatrix, ...) {    # input = matrix
    
    inverse <- SpecMatrix$getinverse()      # access input object & get inverse
    
    if(!is.null(inverse)) {                 # only called if value in cache
        message ("getting cached data")
        return(inverse)
    }
                                          # only called if no value in cache:
    data <- SpecMatrix$get()              # gets the matrix value
    inverse <- solve(data,...)            # calculate inverse
    SpecMatrix$setinverse(inverse)        # stores the calculated inverse
    
    inverse                               # returns the inverse
        
}
