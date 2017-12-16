# Matrix inversion is a costly calculation thus
# caching the inverse of a matrix rather than R 
# calculating repeatedly will most likely be 
# beneficial. The functions below are used to 
# cache the inverse of a matrix.

# MakeCacheMatrix - creates a list containing function to do the ff:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

MakeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    SetInverse <- function(inverse) inv <<- inverse
    GetInverse <- function() inv
    list(set=set, get=get, 
         SetInverse=SetInverse, 
         GetInverse=GetInverse)
}


# CacheSolve - returns the inverse of the matrix by doing the ff:
# 1.Checks if the inverse has already been computed. 
# 2.If so, it gets the result and skips the computation. 
# 3.If not, it computes the inverse, sets the value in the cache thru 
#   the SetInverse function.

# The function below assumes that the matrix will always be invertible.

CacheSolve <- function(x, ...) {
    inv <- x$GetInverse()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$SetInverse(inv)
    inv
}

