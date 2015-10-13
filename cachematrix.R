## This function, makeCacheMatrix, creates a special "matrix", which is a list containing 
## a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of a matrix
##      4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        set_matrix_inverse <- function(inverse) matinv <<- inverse
        get_matrix_inverse <- function() matinv
        list(set = set, get = get,
             set_matrix_inverse = set_matrix_inverse,
             get_matrix_inverse = get_matrix_inverse)
}


## This function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$get_matrix_inverse()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$set_matrix_inverse(matinv)
        matinv        
}
