## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## implements new object which can cache calculated
## inverse of a matrix. Once calculated the inverse is
## kept in memory and might be used at any moment without
## new calculation.

## for non-square matrices we will use ginv function to get inverse
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    dataDim <- dim(data)
    if(dataDim[1] == dataDim[2]) {
        i <- solve(data)
    }
    else {
        i <- ginv(data)
    }
    x$setinverse(i)
    i
}
