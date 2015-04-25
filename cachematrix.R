## the MakeCacheMatrix stores the functions required to create the inverse 
## of a matrix and stores that inverse if the function is called again

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}

## this function calls functions from makeCacheMatrix. It first checks if
## there is already an inverse of the matrix stored in m in makeCacheMatrix
## and recalls that result, rather than recalculating

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("retriving from cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m

}