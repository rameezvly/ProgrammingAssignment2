## The 'makeCacheMatrix' function will create a special "matrix" object that 
## can cahce its inverse

## In the following function actually returns a list that contain four functions
## 1. 'set_matrix'  - to set the matrix
## 2. 'get_matrix'  - to get the matrix
## 3. 'set_inverse' - to cache the inverse of matrix
## 4. 'get_inverse' - to get the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set_matrix <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set_matrix, get = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## The 'cacheSolve' function will return the inverse of the matrix, created by
## 'makeCacheMatrix' function. If the inverse already calculated, then it will
## return the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$set_inverse(m)
        m
}
