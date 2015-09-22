
## To take advantage of the caching first convert your matrix and then call CacheSolve 
## eg: cacheSolve(makeCacheMatrix(myMatrix))
## the tricky part is getting test cases that are invertable so here are 2
##
## mm <- matrix(rnorm(16), 4)
## mm <- seq((1:4),2,2)
## calling cacheSolve(makeCacheMatrix(mm)) and slove(mm) should return identical results


## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## The vector returned contains 4 functions: set, get, setInverse, and getInverse

## assume that the matrix supplied is always invertible


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
            x <<- y                 
            m <<- NULL
        }
        get <- function() x         
        setInverse<- function(getInverse) m  <<- solve 
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  
          invFunc <- x$getInverse()
          if(!is.null(invFunc)) {
            message("getting cached data")
            return(invFunc)
          }
          data <- x$get()
          invFunc <- solve(data, ...)
          x$setInverse(invFunc)
          invFunc
          
  
}
