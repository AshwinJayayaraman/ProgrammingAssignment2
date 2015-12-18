## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL # Initializing the inverse matrix to NULL here
     	set <- function(y) 
	{
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv # Once the inverse is calculated it is stored in m
        getinverse <- function() m  # Return the value of the inverse
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
        # Returns a list which contains the matrix along with the inverse. 
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() # To find the value of the inverse , in the first time it will be NULL and thus the if will be skipped
	# Later on it wont be NULL and thus the if statement will run returning the value of the inverse 
        if(!is.null(m)) 
	{
              message("getting cached data")
              return(m)
        }
        data <- x$get() # Get the matrix from the list
        m <- solve(data, ...) # FInd the inverse of the matrix
        x$setinverse(m) # Set the inverse for future use 
        m
}
