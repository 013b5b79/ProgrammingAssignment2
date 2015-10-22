## This function, makeCacheMatrix, creates a special "matrix", which is a list containing 
## functions to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of a matrix
##      4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                # assign a value to an object in an environment different from the current environment
                x <<- y 
                # restore to null the value of matinv since the old value is no longer needed
                matinv <<- NULL 
        }
        get <- function() x
        set_matrix_inverse <- function(inverse) matinv <<- inverse
        get_matrix_inverse <- function() matinv
        
        ## Store the four functions in a list
        ## This list is used as the input to cacheSolve()
        list(set = set, get = get,
             set_matrix_inverse = set_matrix_inverse,
             get_matrix_inverse = get_matrix_inverse)
}

## This function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache.
## 
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$get_matrix_inverse()
        
        # Check if the inverse has already been calculated
        # If so, get it from the cache and skip the computation
        if(!is.null(matinv)) {
                message("Getting cached data")
                return(matinv)
        }
        
        # If not chached, calculate the inverse
        data <- x$get()
        matinv <- solve(data, ...)
        
        # Set the value of the inverse in the cache
        x$set_matrix_inverse(matinv)
        matinv        
}
