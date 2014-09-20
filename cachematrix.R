## Creates an object that contains functions for setting and returning a matrix and its inverse  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
               
        setinv <- function(inverse) inv <<- inverse
   
        getinv <- function() inv 
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Computes the inverse of the matrix stored in the matrix object of the input object  

cacheSolve <- function(x, ...) {

        inv <- x$getinv()
        
        if (!is.null(inv))    
        {
                message("getting cached data")
                return(inv)
        }
        
        else {
        	data <- x$get() 
        	inv <- solve(data, ...)
        	x$setinv(inv)
        	inv 	
        }

}