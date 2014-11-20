## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x ) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(mdata) { m <<- mdata }
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- t(data, ...)
    x$setinv(m)
    m
}

#### Test cases 1

m <- matrix(rnorm(16), nrow = 4)   ## Test with rnorm
cm <- makeCacheMatrix(m) 
cm$get() 
cacheSolve(cm)  

#### Test cases 2
m <- matrix(data = 1:96 , nrow = 12 , ncol = 8)  ## Test with regrural matrix 
cm <- makeCacheMatrix(m) 
cm$get() 
cacheSolve(cm)  
