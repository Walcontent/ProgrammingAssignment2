## In this assignment I've written two functions that caching the inverse of a matrix
## rather than compute it each time

## The first function, makeCacheMatrix(), creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        set.inverse <- function(inv) inverse <<- inv
        get.inverse <- function() inverse
        list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}

## The following, cacheSolve(), function computes the inverse of the special "matrix"
## returned by makeCacheMatrix() above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve()
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       inverse <- x$get.inverse()
       if(!is.null(inverse)) {
           message("Get cached data:")
           return(inverse)
       }
       matrix <- x$get()
       inverse <- solve(matrix)
       x$set.inverse(inverse)
       inverse
}

## Testing
test_matrix <- matrix(c(3, 4, 5 ,7), 2)
m <- makeCacheMatrix(test_matrix)
cacheSolve(m)

test_matrix %*% cacheSolve(m)
identical(test_matrix %*% cacheSolve(m), diag(1, 2, 2))
