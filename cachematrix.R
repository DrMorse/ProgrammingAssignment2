## The functions together create a class object that enables the caching 
## of the inverse of a matrix since this can be a computationnaly significant
## task

## This function (makeCacheMatrix) is a modified matrix object that can 
## store both the matrix and it's inverse.  Getters and
## setters are provided for both the matrix and it's inverse
## ideally we would limit access to some of all of the setters and getters 
## to only the cacheSolve function below to prevent user from misusing this
## object

makeCacheMatrix <- function(x = matrix()) {
        slv <- NULL		## initiates the inverse to NULL
        set <- function(y) {
                x <<- y		## sets matrix to provided value
                slv <<- NULL	## resets inverse to NULL if a new matrix is set
        }
        get <- function() x	## returns matrix
        setSolve <- function(solve) slv <<- solve
					## sets the matrix inverse
					## should only be accessed by cacheSolve function
        getSolve <- function() slv
					## returns the matrix inverse
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function (cacheSolve) returns the inverse of a cacheMatrix object.
## However, it first checks to see if an inverse has already been computed
## and cached.  If a cached inverse exists, it is returned.  Otherwise,
## an inverse is computed, cached and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        slv <- x$getSolve()	## grabbed the cached matrix inverse
        if(!is.null(slv)) {	## if it is not NULL, return
                message("getting cached data")
                return(slv)
        }				## otherwise, compute inverse, cache then return
        data <- x$get()		## grab original matrix
        slv <- solve(data, ...)
					## compute inverse
        x$setSolve(slv)		## cache inverse matrix
        slv				## return inverse matrix
}
