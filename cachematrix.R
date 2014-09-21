# makeCacheMatrix creates a list with four elements:
# > set the value of the matrix
# > get the value of the matrix
# > set the value of inverse of the matrix
# > get the value of inverse of the matrix

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

# The following function calculates the inverse of the matrix.
# It first checks if the inverse has already been calculated.
# If so, it gets the result from the cache and skips the calculation.
# Otherwise, it calculates the inverse and sets the value in the cache via setinverse function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

# Test
# 
# > x <- matrix(1:4, 2, 2)
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m <- makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 