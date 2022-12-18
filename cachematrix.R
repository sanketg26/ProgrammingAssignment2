# Below are two functions that are used to create a special matrix that
# stores a matrix and cache's its inverse.


# makeCacheMatrix creates a special "matrix", which returns a list containing a function to
# 1] set the value of the matrix
# 2] get the value of the matrix
# 3] set the value of the inverse of the matrix
# 4] get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve calculates the inverse of the special "matrix" created with the makeCacheSolve function.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse using the data and 
# sets the inverse of the matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m       
    ## Return a matrix that is the inverse of 'x'
}

