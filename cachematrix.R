# These functions are used to cache potentially time consuming computations. 
# More specifically they are used to compute the inverse of a matrix. 
# Instead of recalculating the inverse repeatedly they cache it to save time.

# This function creates a list of functions.
# The specific functions it contains are a:
# (1) get value of a matrix
# (2) set value of a matrix
# (3) get the value of the inverse of the matrix
# (4) set the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse_num <- NULL
    	set <- function(y) 
	{
        	x <<- y
        	inverse_num <<- NULL
    	}
	get <- function() x
	setInverse <- function(inv_var) inverse_num <<- inv_var
	getInverse <- function() inverse_num
	list(set=set, get=get, setinverse=setInverse, getinverse=getInverse)
}

# This function returns the inverse of a matrix.
# It checks first if the inverse has been computed.
# If not, then it computes the inverse, then sets the value
# in the cache with a setinverse function. 
cacheSolve <- function(x, ...) {
	inverse_num <- x$getinverse()
	if(!is.null(inverse_num)) 
	{
		message("Returning the cached invserse.")
		return(inverse_num)
	}
	data <- x$get()
	inverse_num <- solve(data)
	x$setinverse(inverse_num)
	inverse_num
}

