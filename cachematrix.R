## Cache inverse of a matrix

## makeCacheMatrix makes a list to set and get a matrix, as well as set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}



## Below generates inverse of the matrix. If already calculated, uses cache value, if not, calculates.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
        }

