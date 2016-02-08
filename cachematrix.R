## This second programming assignment requires us to write an R function that 
## is able to cache potentially time-consuming computations.
## Following functions are used to create a special "vector" and 
## cache the inverse of a matrix.

## The first function(makeCacheMatrix) creates a special "vector", 
## which is really a list containing a function to
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        mtrxinv <- NULL
        set <- function(mtrxinp) {
                x <<- mtrxinp
                mtrxinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mtrxinv <<- inverse
    getinverse <- function() mtrxinv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the mean of the special "vector" 
## created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mtrxinv <- x$getInverse()
    if (!is.null(mtrxinv)) {
        message("getting cached data")
        return(mtrxinv)
    }
    mat <- x$get()
    mtrxinv <- solve(mat, ...)
    x$setInverse(mtrxinv)
    mtrxinv
}
