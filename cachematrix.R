## These functions cache the inverse of a matrix to save computation time

## Creates a special object (list of functions) to store a matrix 
## and cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        mi <- NULL   # m_atrix i_nverse

        # 'set' functions
        setMatrix  <- function(y) {
                x  <<- y
                mi <<- NULL
        }
        setInverse <- function(inverse) mi <<- inverse

        # 'get' functions
        getMatrix  <- function() x
        getInverse <- function() mi
        
        # create list
        list(setMatrix = setMatrix, 
                getMatrix  = getMatrix,
                setInverse = setInverse,
                getInverse = getInverse
        )
  
}


## Calculates the inverse of a matrix 'x', if it has not yet been calculated.
## Otherwise, uses its cached inverse to skip the computation.
## ASSUMPTIONS:
##   i.  The matrix is solveable
##   ii. The matrix has not changed since it was cached

cacheSolve <- function(x, ...) {

        # get cached inverse
        mi <- x$getInverse()
        
        # assuming matrix is unchanged, if inverse is cached, don't recalculate
        if (!is.null(mi)) {
                message("getting cached data")
        }
        # otherwise, get matrix, calculate the new inverse and cache it 
        # (assuming inverse exists)
        else {                
                data <- x$getMatrix()
                mi   <- solve(data, ...)
                x$setInverse(mi)
        }

        # return inverse
        mi
        
}
