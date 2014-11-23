## Functions that allow the compute and caching of the inverse of a matrix 
## for repeatedly use rather than compute it every time.
## 
## Examples of use:
## 
## A <- matrix(c(2,4,3,1),nrow=2,ncol=2,byrow=TRUE)
## 
## B <- makeCacheMatrix(A)      # create the "matrix" object
## B$get()                      # return the "matrix" object
## B$getinverse()               # return NULL because inverse has not yet been calculated
## 
## cacheSolve(B)                # use cacheSolve() to compute and store the inverse of the matrix
##
## B$getinverse()               # return the inverse of the matrix
##
## cacheSolve(B)                # return the cached object
##


## Function responsible to creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cache_inverse <- NULL
    set <- function(y) {
            x <<- y
            cache_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cache_inverse <<- inverse
    getinverse <- function() cache_inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Function responsible to computes the inverse of the object returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inverse_matrix <- x$getinverse()
    ## check if the computed inverse of matrix is already in cache
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    ## if inverse of matrix is not cached then calculate and store in cache
    data <- x$get()
    message("computing inverse")
    inverse_matrix <- solve(data, ...)
    
    ## set the calculate value for use in cache 
    x$setinverse(inverse_matrix)
    
    ## return a matrix that is the inverse of 'x'
    inverse_matrix
}
