## Cachhing the Inverse of a Matrix:
## 
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
##Below are two functions that cache the inverse of a matrix. 

## Create a speacial "matrix" object, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
          x <<- y
          inv <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list( set=set, get=get,
          setInverse=setInverse, 
          getInverse=getInverse
    )
}


## Below is a function that computes the inverse of the speacial
## "matrix" object that can cache its inverse

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setInverse(inv)
        inv
}
