## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL						
	set <- function(y){					
     		 x <<- y					
             m <<- NULL					
        }							# set the value of the matrix
        get <- function() x				# get the value of the matrix
        setsolve <- function(solve) m <<- solve # set the inverse of the matrix 
        getsolve <- function() m			# get the inverse of the matrix
        list(set = set, get = get,			
             setsolve = setsolve,
             getsolve = getsolve)			# return the function as a list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getsolve()			# query the cache of the matrix
	if(!is.null(inv)) {			# 
		message("getting cached data")# message informing that is in cache
		return(inv)				# return the cache
	}					
	data <- x$get()				# get the matrix of "makeCacheMatrix"
	inv <- solve(data,...)			# calculate the inverse of the matrix
	x$setsolve(inv)				# store the inverse fo the matrix in cache with the function "makeCacheMatrix"
	inv	
}
