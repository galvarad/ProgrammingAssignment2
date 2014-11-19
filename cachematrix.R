## Together both functions help calculate or fetch the inverse of
## matrix from cache

## makeCacheMatrix is passed a matrix and creates an object of type list.
## The object stores the original matrix and the cached value which is
## originally set to NULL. 

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


## cacheSolve access the object created by makeCacheMatrix function. If
## the inverse has not been calculated cacheSolve calculates it and
## stores it in the object created by the call to makeCacheMatrix() and
## returns the inverse. If the inverse has been calculated earlier then
## cacheSolve prints a message, fetches the inverse and returns it. 

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
