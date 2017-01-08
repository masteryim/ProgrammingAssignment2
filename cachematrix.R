## Creating a special "matrix" object that can cache its inverse.  
## This function containts the following functions 
## - get			get the value of a matrix 
## - set			set the value of a matrix 
## - getInverseMatrix		get the cached value (inverse of the matrix) 
## - setInverseMatrix		set the cached value (inverse of the matrix) 
 
 
makeCacheMatrix <- function(x = matrix()) {
    invVal <- NULL
    set <- function(y) {
        x <<- y
        invVal <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inv) invVal <<- inv
    getInverseMatrix <- function() invVal
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


 
## Calculating the inverse of a "special" matrix created with makeCacheMatrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invVal <- x$getInverseMatrix()
    if(!is.null(invVal)) {
        message("getting cached data")
        return(invVal)
    }
    data <- x$get()
    invVal <- solve(data, ...)
    x$setInverseMatrix(invVal)
    invVal
}