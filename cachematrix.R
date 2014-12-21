## These functions returns the inverse of a matrix with a cache solution.

# Library MASS contains the function ginv.
# ginv return the inverse matrix for squared and non-squared matrixes
library(MASS)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                matrix <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(minv) inverse <<- minv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        minv <- ginv(data, ...)
        x$setinverse(minv)
        minv
}

## Test
A = matrix( 
  c(2, 4, 3, 1, 5, 7, 5, 6, 7),
  nrow=3,              
  ncol=3,               
  byrow = TRUE
)

# Makes CacheMatrix from matrix A
matrix <- makeCacheMatrix(A)

# Show Matrix
print("Matrix A:")
print(matrix$get())

# Calculates de inverse of A without Cache
print("Inverse of A:")
inverse <- cacheSolve(matrix)
print(inverse)


# Calculates de inverse of A with Cache
print("Inverse of A:")
inverse <- cacheSolve(matrix)
print(inverse)
