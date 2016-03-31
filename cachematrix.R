# The first function creates a matrix with setters and getters for the matrix and its inverse.
# The second matrix calculates the inverse matrix if it's not calculate yet.

# Example:
#   > source('cachematrix.R')
#   > B <- matrix( c(2, 4, 3, 1),nrow=2,ncol=2) 
#   > m <- makeCacheMatrix(B)
#   > cacheSolve(m)
#        [,1] [,2]
#   [1,] -0.1  0.3
#   [2,]  0.4 -0.2

makeCacheMatrix <- function(x = matrix()) {
# This function creates a special "matrix" object that can cache its inverse.
    
    m_inverse <- NULL
    
	  # set matrix and initialize inverse matrix
    set_matrix <- function(y) {
		    # matrix
        x <<- y
		    # init inverse matrix
        m_inverse <<- NULL
    }
    
	  # get_matrix
    get_matrix <- function(){
		  return(x)
	  }
	
    # set inverse matrix
    set_inverse_matrix <- function(inverse){
  		m_inverse <<- inverse 
  	}
	
    # get the inverse matrix
    get_inverse_matrix <- function(){ 
  		return(m_inverse) 
  	}
	
    # Return list with functions to work with the matrix
    return(list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse_matrix = set_inverse_matrix, get_inverse_matrix = get_inverse_matrix))
}

cacheSolve <- function(x, ...) {
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
	
	  # try to get the inverse matrix
    inverse_matrix <- x$get_inverse_matrix()
	
    # Check if the inverse matrix was already calculated
    if (is.null(inverse_matrix)) {
  		# inverse matrix has not been calculated earlier
  		# calculate the inverse
  		inverse_matrix <- solve(x$get_matrix(), ...)
  		
  		# Store the inverse matrix.
  		x$set_inverse_matrix(inverse_matrix)
    }
	
    # return the inverse matrix
    return(inverse_matrix)
}
