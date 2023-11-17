## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # Initialize inverse matrix as NULL
        inv <- NULL
        
        # Function to set the matrix and reset the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Function to get the current matrix
        get <- function() x
        
        # Function to set the inverse matrix
        setInverse <- function(inverse) inv <<- inverse
        
        # Function to get the inverse matrix
        getInverse <- function() inv
        
        # Return a list of functions
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Check if the inverse is already cached
        inv <- x$getInverse()
        if (!is.null(inv)) {
                # If cached, print a message and return the cached inverse
                message("getting cached data")
                return(inv)
        }
        
        # If not cached, calculate the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        
        # Cache the calculated inverse
        x$setInverse(inv)
        
        # Return the calculated inverse
        inv
}
