## Put comments here that give an overall description of what your
## functions do
#  Both functions, makeCacheMatrix and cacheSolve calculates the
#  inverse of matrix given


## Write a short comment describing this function
#  This funtion creates a list of functions that will be used by
#  the function cacheSolve
#  First a matrix in a variable must be created

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) m <<-inverse
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Write a short comment describing this function
#  This function calculates the inverse of a 
#  matrix given
#  First we must run the function makeCacheMatrix on 
#  a matrix

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInv(m)
    m
}
