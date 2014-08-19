### Matrix Inverse Caching
## These functions can be used to cache the inverse to a matrix

## makeCacheMatrix 
## creates a special Matrix, which is really a list containing a function to
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - the value of the inverse to the matrix 
## getinverse - the value of the inverse to the matrix 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


### cacheSolve 
## Function to solve/find the inverse to a matrix. It will first check if there is
## a cached solution from cacheMatrixSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
