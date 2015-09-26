#---------------------------------------------------------------------------
#makeCacheMatrix: This function creates a special 
#"matrix" object that can cache its inverse.
#---------------------------------------------------------------------------
#cacheSolve: This function computes the inverse 
#of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
#---------------------------------------------------------------------------

## This function would prepare take the matrix as input and create 
# necessary helper functions which would help in 
# caching the required matrix inverse

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


## CacheSolve : If the inverse of the matrix is present in cache 
# then the function would display it
# if the inverse is not present then the function 
# would use solve function to do inverse of Matrix and store it in cache for
# future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        message("Inverse of matrix is:")
        return(m)
    }
    data <- x$get()
    #if the matrix is singular inverse is not possible so add a trycatch
    # Just checked again after completing the assignment though to assume
    # the matrix is always invertible
    tryCatch(
        {
            m <- qr.solve(data, ...)
            x$setinv(m)
            message("Inverse of matrix is:")
            m
        }
        ,error= function(c)
        {
            msg <- "Matrix is not invertible."
            message(msg)
        }
    );
    
    
}
