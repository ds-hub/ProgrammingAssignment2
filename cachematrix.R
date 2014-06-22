## makeCacheMatrix is a wraper matrix function to store matrix and its inverse
## cacheSolve is a function that returns the matrix inverse either from cache or on-the-fly calculation

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ### initiate inverse variable
    inv <- NULL
    
    ### set matrix data
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ### get matrix data
    get <- function() x
    
    ### set matrix inverse
    setinverse <- function(inverseMatrix) inv <<- inverseMatrix
    
    ### get matrix inverse, NULL if not set
    getinverse <- function() inv
    
    ### list of available methods for this function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ### check for pre-calculated matrix inverse
    m <- x$getinverse()
    if(!is.null(m)) {
        #### inverse exists, no need for on-the-fly calculations
        message("getting cached data")
        return(m)
    }
    
    ### matrix inverse is NULL, calculate it now    
    data <- x$get()
    m <- solve(data, ...)
    
    ### set inverse into cache for future retrieval
    x$setinverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m        
}
