# To compute the inverse of a matrix may be a costly computation task, so
# storaging it in cache is better than compute it echa time.
# Fucntions makeCacheMatrix and cacheSolve are defined with this aim.


# function makeCacheMatrix: creates a special matrix, which is really a list containing
# a function to 1) set the value of the matrix, 2) get the value of the matrix,
# 3) set the value of the inverse, and 4) get the value of the inverse

makeCacheMatrix <- function(x=matrix()) {
	
	#initialize the inverse
	inv <- NULL;

	# setter (to set the value of the matrix)
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	# getter (to get the value of the matrix)
	get <- function() x

	# setter (to set the value of the inverse)
      setinv <- function(invMatrix) inv <<- invMatrix

	# getter (to get the value of the inverse)
      getinv <- function() inv

      list(set = set,
	     get = get,
           setinv = setinv,
           getinv = getinv)
}


# function cacheSolve: returns the inverse of matrix m from the stored value
# in the cache (if it has already been computed), or computes it with the 
# solve function (and stores it in cache)

cacheSolve <- function(x,...) {
	
	# check if the inverse value is stored in cache
	# if so, it returns it
	inv <- x$getinv()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }

	# if no value stored in cache, the inverse of the matrix is computed
	# with solve, stored, and returned
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
