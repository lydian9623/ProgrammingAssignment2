## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache the inverse of matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   ##Cache inverse of matrix 'x' :Initialization
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
