#The following functions "makeCacheMatrix" and "cacheSolve" are used to cache the inverse of a matrix.

#To cache the inverse of a function, it takes four steps for the function "makeCacheMatrix" to execute:
#1:set the value of the matrix;
#2:get the value of the matrix;
#3:set the inverse of the matrix;
#4:get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The following function "cacheSolve" returns the inverse of the matrix:
#1:checks if the inverse function has already been computed;
#2:if the inverse has already been computed, it skips the execution of the function and returns the result;
#3:if not, the function computes the inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
