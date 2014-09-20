## makeCacheMatrix takes an invertible matrix as input
## and returns a  list of functions that can calculate,
## cache and return the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        set <- function(y) {
                x <<- y
                invers <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invers <<- solve
        getinv <- function() invers
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## cacheSolve uses the components of the output from makeCacheMatrix
## to compute and cache the matrix's inverse the first time it encounters
## it, but simply returns the cached inverse in subsequent runs.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getinv()
        if(!is.null(invers)) {
                message("getting cached data")
                return(invers)
        }
        data <- x$get()
        invers <- solve(data, ...)
        x$setinv(invers)
        invers
}
