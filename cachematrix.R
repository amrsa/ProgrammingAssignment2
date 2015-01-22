## These functions allow to cache a matrix and its inverse.
## The matrix must be square and invertible.
## If they're not, errors will occur in the second function.

## This function accepts a matrix as argument and provides
## the means of keeping its inverse when it is computed.
makeCacheMatrix <- function(x = matrix()) {
    # inv <- NULL is a flag to tell that the inverse was not 
    # calculated since the matrix was initialized or changed.
    inv <- NULL
    # set allows to change the matrix, including its size.
    set <- function(y = matrix()) {
        inv <<- NULL    # Makes 'inv' in the environment above NULL. 
        x <<- y         # Assings new value to the matrix.
    }
    get <- function() x             # Gets the matrix.
    setInv <- function(m) inv <<- m # Sets the inverse of the matrix
    getInv <- function() inv        # Gets the inverse of the matrix
    # Returns the list of functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function checks whether or not the inverse of the matrix
## which is passed as an argument was already calculated.
## If it was, returns its inverse; otherwise, 
## calculates it, sets it for future usage and returns it.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()   # Get the recorded value of the inverse of 'x'
    if(!is.null(inv)) { # If it is not NULL, it was calculated before.
        message("Getting cached inverse...")
        return(inv)
    }
    # If 'inv' is NULL then we get to this part of the function.    
    data <- x$get()             # We get the data of the matrix.
    inv <- solve(data)          # Compute its inverse.
    x$setInv(inv)               # Record the computed inverse, for future use.  
    inv                         # Return the inverse of the matrix.
}
