# Objectives: 
#   I.   Set the value of the matrix.
#   II.  Get the value of the matrix.
#   III. Set the value of the inverse matrix.
#   IV.  Get the value of the inverse matrix.

makeCasheMatrix <- function(x = numeric()) {
                     inverse <- NULL
                     set <- function(y) {
                     x <<- y
                     inverse <<- NULL
            }
            get <- function() x
            set_inv <- function(inv) inverse <<- inv
            get_inv <- function() inverse
            list(set = set, get = get,
                 set_inv = set_inv,
                 get_inv = get_inv)
} 

# The function below will return the inverse matrix.
# First, it will check if the inverse has been computed already.
# (If it had been computed already, it takes the result
# and skips the computation). Otherwise, it will compute
# the inverse and set the cache value using the set_inv function seen above. 

cacheSolve <- function(x, ...) {
              inverse <- x$get_inv()
              if(!is.null(inverse)) {
                 message("Retrieving your cached data!")
    return(inverse)
  }
  the_data <- x$get()
  inverse <- solve(the_data)
  x$set_inv(inverse)
  inverse
}

x <- rbind(c(1.5, -1/2), c(-1/3, 1.6))
m <- makeCasheMatrix(x)
m$get()

#The first run through will have no cache
cacheSolve(m)

#Now, calling in from the cache above
cacheSolve(m)

# Recall that for this assignment, we assume that 
# the supplied matrix is always invertible.