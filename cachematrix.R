## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
	
	## set
	set <- function(y)
	{
		## assign y to a different environment which is outside of the current environment.
		x <<- y
		m <<- NULL		
	}
	
	## get
	get <- function() x

	## calculate mean
	setsolve <- function(solve) m <<- solve
	
	## retrieve inverse data from cache
	getsolve <- function() m

	## return a list
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        m <- x$getsolve()

	## if the inverse data is already in cacle
	if (!is.null(m))
	{
		message("Getting cache data")
		return(m)
	}			
	else  ##inverse data is not in cache
	{
		data <- x$get()
		
		## take the inverse
		m <- solve(data, ...)

		## save m in cache for later use.
		x$setsolve(m)
		
		## return inverse data
		m

	}        
}
