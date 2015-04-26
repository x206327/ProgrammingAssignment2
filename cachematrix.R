## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function takes a matrix as input parameter and invert it.
# It creates a list of functions to set the matrix, get the matrix,
# set the inverted matrix of the input matrix (by solve), and get
# the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
		    x <<- y
		    m <<- NULL
		}
		get <- function() x
		setinv <- function(solve) m <<- solve
		getinv <- function() m
		list(  	  set = set,
			  get = get,
			  setinv = setinv,
			  getinv = getinv
		)	  
}

## Write a short comment describing this function
# The function inverts an invertible matrix. First it checks if the
# inversion is alreaduy done. If it is, it gets the inverted matrix
# without having to invert it again. If not, it inverts the matrix by
# solve(). 
# A short demo:
# > x<-matrix(1:4,2,2) # this is an invertible 2x2 matrix
# > y<-makeCacheMatrix(x) 
# > cacheSolve(y)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(y)
# getting cached data <== the inverted matrix is cached, just get it.
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
