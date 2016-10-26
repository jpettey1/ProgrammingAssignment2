## Matrix inversion calculations and coding can be very tricky and time consuming.
## The purpose of these functions is to create a matrix that can be cached,
## or stored, then solve for the inverse of the matrix which can also be cached,
## which allows for a quick and easy retrieval of it when needed in the future.
## Note: The assignment is designated for matrices that can be inverted, so if an
## uninvertible matrix is created and attempted to be solved, an error will occur. 


## The "makeCachematrix" function creates a special matrix that can be cached.
## Within the main function are 4 subfunctions: set, find, setInverse, and
## findInverse. The makeCachematrix creates the matrix, find is used to retrieve
## the cached matrix, and set can be used to create a new matrix once the initial
## one has be retrieved.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(z){
         x <<- z
         inv <<- NULL
     }
     find <- function() x
     setInverse <- function(solveMatrix) inv <<- solveMatrix
     findInverse <- function() inv
     list(set = set, find = find, setInverse = setInverse, findInverse = findInverse)
     
}

## The cacheSolve function solves the inverse of the previously cached matrix from
## the makeCachematrix function above. The findInverse function within the cacheSolve
## function is used to retrieve the solved inverse that got cached.
 
cacheSolve <- function(x, ...) {
       inv <- x$findInverse()
       if (!is.null(inv)) {
           message("Retrieving stored data")
           return(inv)
         }
       mat <- x$find()
       inv <- solve(mat, ...)
       x$setInverse(inv)
       inv
             ## Return a matrix that is the inverse of 'x'
}
