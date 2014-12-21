## These functions make a matrix object and then cache its inverse in order to save on costly calculation time

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL  ## Initialize the inverse to NULL everytime makeCacheMatrix is called
	
		# A setter which allows the object ot be assigned a new value and resets the inverse to NULL 
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
	
		get <- function() {x} # Returns value of the original matrix
		setinverse <- function(inverse) inv <<- inverse  # Setter which uses superasssignment to set the inverse
		getinverse <- function() inv # Getter which returns the inverse
		
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # Return a list of the getters and setters
}


## Calculate the inverse of the matrix.  If the inverse of the matrix has already been calculated, retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()  # Get the value of the inverse of the matrix
        if (!is.null(inv)) {
        	message("getting cached data") # Send message to console that the cached value is being accessed and returned
        	return(inv)
        }
        origdata <- x$get() # If the data was not cached, we need to get the original matrix
        inv <- solve(origdata, ...) # If the data was not cached, calculate the inverse using the solve function
        x$setinverse(inv)
        inv # Return the inverse
}
