## These functions are used to Cache the Inverse of a Matrix to reduce
## costly computation by cutting the need to compute it repeatedly

## The first function is to create a list containing a function to set the
## value of the vector, get the value of the vector, set the value of the
## inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
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


## The second function checks to see if the inverse has been calculated.
## If so, it will return the inverse. If not it will calculate the inverse
## and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
