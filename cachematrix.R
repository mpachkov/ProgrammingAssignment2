## Put comments here that give an overall description of what your
## functions do


## for non-square matrices we will use ginv function
## from MASS library to get inverse
library(MASS)

## implements new object which can cache calculated
## inverse of a matrix. Once calculated the inverse is
## kept in memory and might be used at any moment without
## new calculation.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # i keeps inverse of original matrix
    # object methods definitions
    set <- function(y) {
        x <<- y # original matrix
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns inverse for cached matrix.
## If cached matrix does not contain its inverse
## than the function calculates and saves it to the object.
## If matrix iverse was already calculated than cached
## inverse is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    # if inverse is cached return it without recalculation
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    
    ## if matrix is square use solve()
    ## else use ginv() which can work with nonsquare matrices
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
