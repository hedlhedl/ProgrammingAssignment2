## Get the matrix, check if there is a cached inverse available. If not, calculate its inverse.

## Get the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
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


## Check for cache and calculate an inverse if it's absent.

cacheSolve <- function(x, ...) {
	  m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
