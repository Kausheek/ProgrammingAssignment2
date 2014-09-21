## The "makeCacheMatrix" is a function which receives a matrix input
## and puts its inverse to the cache.

## The "cacheSolve" is a function which computes the inverse of the 
## matrix send by "makeCacheMatrix" function. If the inverse is already 
## available in the cache memory then this function retrieves that from the cache.

## Puts the inverse of a matrix into cache

makeCacheMatrix <- function(x = numeric()) {
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


## Returns a matrix that is the inverse of 'x'or fetches the inverse matrix from Cache.

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







