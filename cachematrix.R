## This function created a data structure to store the matrix x and its inverse, if any
## After the first solve of inverse of x, the result is cached in the data structure for further usage

## This function is a data structure whcih can store the inverse of x

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y)
	{
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## If inverse of x has never been calculated, this function calculate it and result will be store in makeCacheMatrix
## If inverse of x has been calculated once, this function return the stored result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i))
	{
		message("getting cahced data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
