## makeCacheMatrix function contains functions "set", "get", "setinverse", and "getinverse";
## these four functions are stored in a list

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {		##to change main function's matrix input if necessary
		x <<- y
		inv <<- NULL
	}
	get <- function() x		##retrieve main function's matrix input
	setinverse <- function(solve) inv <<- solve		##stores inv into makeCacheMatrix
	getinverse <- function() inv		##returns inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## If the inverse ("inv") is not null, it's been cached and will be returned;
## If it is null, the matrix input is stored in "data", for which the
## inverse is then found and stored in variable "inv" 

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
