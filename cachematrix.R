## Since matrix inversion is usually a costly procedure, there may be 
## a benefit to caching a matrix. The two functions below will either
## calculate the inverse matrix, or if the inverse has been calculated, 
## retrieve the inverse matrix from the cache. 

## This function takes x, an invertible matrix, and creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		 x <<- y
		 inv <<- NULL	
	}
     
    get <- function() x
    setInverse <- function (solve)  inv <<- solve
    getInverse <- function () inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the matrix object created by makeCacheMatrix
## above. If the inverse has already been calculated, and there has been no change to 
## the matrix, then this function retrieves the inverse from the cache instead.

cacheSolve <- function(x, ...) {	
	## Return a matrix that is the inverse of 'x'
	      inv <- x$getInverse()
	      if(!is.null(inv)){
	      	message("retrieving cached data")
	      	return(inv)
	      }
	      
	      matr <- x$get()
	      inv <- solve(matr,...)
	      x$setInverse(inv)
	      inv       
}
