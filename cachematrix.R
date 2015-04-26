# The two functions in this file will cache a copy of the inverse 
# of a given matrix. By caching the inverse, computation time may 
# be saved when dealing with a large matrix.


# makeCacheMatrix() stores a list of functions for caching a copy 
# of the inverse of a given matrix
# set() sets a new matrix and get() grabs the set matrix
# setInv() sets the cached copy of the inverse
# getInv() grabs the cached copy of the inverse
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setInv <- function(inverse) inv <<- inverse
     getInv <- function() inv
     list( set = set, get = get, setInv = setInv, getInv = getInv)
}


# the function cacheSolve() calculates and saves a copy of the inverse 
# of a given matrix using the functions in makeCacheMatrix()
# if the inverse has already been calculated, the function
# will return the cached copy to save computation time
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)){
      message("getting cached data...")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv

}

