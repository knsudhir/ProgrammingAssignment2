## Functions that cache the inverse of a matrix

## Function creates list of special "matrix" object that can 
## cache its inverse. Matrix supplied is always invertible.
## The list returned is a function containing
## 1) Setting the value of original matrix
## 2) Getting the value of original matrix
## 3) Setting the value of previous matrix to cache
## 4) Getting the value of previous matrix from cache
## 5) Setting the value of matrix inverse to cache
## 6) Getting the value of matrix inverse from cache

makeCacheMatrix <- function(x = matrix()) {

	## Initiate cache variables
	m <- NULL
	n <- NULL

	## Function to set the new matrix and flush out the cache
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Function to get the matrix	
	get <- function() x

	## Function to set previous matrix value to variable n
	setprvs <- function(prvsvalue) n <<- prvsvalue
	
	## Function to get previous matrix value from variable n	
	getprvs <- function() n
	
	## Function to set inverse matrix value to variable m
	setinverse <- function(solve) m <<- solve

	## Function to get inverse matrix value from variable m
	getinverse <- function() m
	
	## Create a list with all the function objects
	list(set = set, get = get,
		setprvs = setprvs,
		getprvs = getprvs,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated  and and the matrix has not changed 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

	## Fethcing previous matrix value from cache
	n <- x$getprvs()
	
	## Fetching the actual matrix into data object
	data <- x$get()
	
	## Checking if the previous matrix exists in cache
	if(!is.null(n)) {
		message("Getting cached previous matrix data")
		
		## Checking all the elements of actual and previous matrix
		if(all(n == data)) {
		
			## Checking if inverse cache has data
			m <- x$getinverse()
			if(!is.null(m)) {
				message("Getting cached inverse data matrix")
				return(m)
			}
		}
	}
	
	## Using solve function create invers of a sqaure matrix
	m <- solve(data, ...)
	
	## Set the actual and inverse matrix into the cache
	x$setinverse(m)
	x$setprvs(data)
	m
}