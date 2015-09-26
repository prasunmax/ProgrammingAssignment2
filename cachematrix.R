#---------------------------------------------------------------------------
#makeCacheMatrix: This function creates a special 
#"matrix" object that can cache its inverse.
#---------------------------------------------------------------------------
#cacheSolve: This function computes the inverse 
#of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
#---------------------------------------------------------------------------

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #take the details from sample CacheMean
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## CacheSolve would use solve function to do inverse of Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    #if the matrix is singular inverse is not possible so add a trycatch
    tryCatch(
        {
            m <- qr.solve(data, ...)
            x$setinv(m)
            m
        }
        ,error= function(c)
        {
            msg <- "Inverse of Matrix is not possible"
            invisible(print(msg))
        }
    );
    
    
}
