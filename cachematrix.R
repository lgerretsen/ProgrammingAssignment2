## Assignment: Caching the Inverse of a Matrix
## The following functions makes it possible to cache the inverse of a matrix. This has the advantage
## that R doesn't need to calculate the inverse everytime, but can retrieve a previously calculated
## inverse out of cache saving costly calculations. 

## please try the functions with this test matrix:
## test_matrix <- makeCacheMatrix(matrix(2:5, 2, 2))

################################################################################################

## 1.   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}

################################################################################################

## 2.   cacheSolve: This function computes the inverse of the special "matrix" returned by 
##      makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##      has not changed), then cacheSolve retrieves the inverse from the cache. You will get 
##      the special message "getting cached data" when R retrieves the inverse from cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}
