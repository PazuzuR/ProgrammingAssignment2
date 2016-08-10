## Functions to cache inverse matrix

## Function to create special "matrix" allowing to:
## - set matrix
## - get matrix
## - set inverse matrix
## - get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setInverse <- function(y) im <<- y
	getInverse <- function() im
	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## If inverse matrix of x is cached, then return it from cache,
## else - calculate

cacheSolve <- function(x, ...) {
        im <- x$getInverse()
        if(!is.null(im)) {
                message("cached value: ")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setInverse(im)
        im
}
