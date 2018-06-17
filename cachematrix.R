## Put comments here that give an overall description of what your
## functions do

## In statistics, one of the important application of Matricies is to the linear simultaneous equations. And we use the inverse Matrix to solve the equations.
## Its time consuming and daunting task if the matrix is large. If the situation demands to reuse the inverse matrix again then it would be a better if we can use it from a stored memory in the programme
## following functions can be used to lookup the inverse matrix from the cache


## Write a short comment describing this function
## makeCacheMatrix: This function makes a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
## matrix object is square matrix and can be inverse
        m <- NULL
		
## "<<-" operator indicates that the assignment should be made to the parent environment	
        set <- function(y) {
                matrix <<- y
                m <<- NULL
        }
        get <- function() matrix
        setMatrix_inverse <- function(inverse) m <<- inverse
        getMatrix_inverse <- function() m

## creating a list of functions by giving them names, names helps to use "$" to access the elements in other functions
        list(set = set, get = get,
             setMatrix_inverse = setMatrix_inverse,
             getMatrix_inverse = getMatrix_inverse)
			 
}


## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

   cacheSolve <- function(matrix, ...) {
        ## get the value of inverse of matrix from makeCacheMatrix
		m <- matrix$getMatrix_inverse() 
      
	  ## get inverse matrix from the cache and skips the computation. 
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	  
	  ## Calculate the inverse, if its not in cache
        data <- matrix$get()	
		m <- solve(data, ...)
		
		## Set the inverse matrix value to matrix object
        matrix$setMatrix_inverse(m)
        m
}
