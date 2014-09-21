## These functions return inverse of matrix. If the iverse of matrix is in the cache, return it without computing, if not the second function compute the inverse and return it.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	in <- matrix()
	setmat <- function(nec) {
		x <<- nec
		in <<- matrix()
	}
	getmat <- function() x
	setinv <- function(inverse) in <<- inverse
	getinv <- function() in
	list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


##The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        in <- x$getinv()
	if(in == matrix()) {
		message("getting cached data")
		return(in)
	}
	matrix <- x$getmat()
	in <- solve(matrix, ...)
	in$setinv(in)
	in
}
