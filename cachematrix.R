## The makeCacheMatrix and cacheSolve functions work together to return
## the inverse of a matrix and store the result in the cache.
## If the inverse has already been calculated, the conversion is skipped
## and the cached matrix returned.

## makeCacheMatrix function creates a list of functions to set a matrix,
## get a matrix, set the inverse of a matrix, and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {  ## Input to 'makeCacheMatrix' function is matrix 'x'
        i <- NULL                            ## 'i' will be the inverse matrix and is set to NULL
        set <- function(y) {                 ## Defines a function to set a matrix by global assignment
                x <<- y
                i <<- NULL
        }
        get <- function() x                  ## Defines a function to retrieve a matrix
        setinverse <- function(inverse) i <<- inverse  ## Defines a function to cache the inverse
        getinverse <- function() i           ## Defines a function to retrieve the inverse matrix
        list(set = set, get = get,           ## Creates a list of the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function returns the inverse of matrix 'x'
## If the inverse has already been calculated, the cached matrix is returned

cacheSolve <- function(x, ...) {              ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()                   ## getinverse function is called, result assigned to 'i'
        if(!is.null(i)) {                     ## If 'i' is not NULL, the cached matrix is returned
                message("getting cached data")
                return(i)
        }
        data <- x$get()                       ## get function called and result assigned to 'data'
        i <- solve(data, ...)                 ## Inverse of matrix 'data' is calculated and set to 'i'
        x$setinverse(i)                       ## setinverse function called to cache matrix 'i'
        i                                     ## Inverse matrix is returned
}