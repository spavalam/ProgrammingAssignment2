

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

                inv <- NULL
                set <- function(y) {
                                    x <<- y
                                    inv <<- NULL
                                   }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set=set, get=get, 
                     setinverse=setinverse, 
                     getinverse=getinverse)
}

# The following function returns the inverse of the matrix. 
# It is Assumed that Matrix is always invertible. 
# The function checks if the inverse has already been computed. 
# If the inverse is computed, the result is returned and the computation is skipped.
# If it the inverse is not computed, it computes the inverse, 
# sets the value in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
      
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                           message("Getting Data From Cache.")
                           return(inv)
                          }
        message("Data NOT in Cache")
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
}

## Sample Run
## The following call creates a two dimensional matrix
## mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
## mat2 <- makeCacheMatrix(mat)
## mat2$get ()
## The following function can be called thrice.
## In the first run, Data would NOT be in cache.
## In the second and third run, Data would be taken from cache and appropriate message would be displayed..
## cacheSolve(mat2)
## Inverse of c (4,2,7,6) is c(0.6, -0.7, -0.2, 0.4). 

