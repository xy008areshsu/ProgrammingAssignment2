## These functions calculate the inverse of a matrix with cache features


## This function makes a cached version of the already calculated inverse

makeCacheMatrix <- function(x = matrix()) {
      _inverse <- NULL
      set <- function(y) {
        x <<- y
        _inverse <<- NULL
      }
      get <- function() x
      set_inverse <- function(inverse) _inverse <<- inverse
      get_inverse <- function() _inverse
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## This function calculates the matrix inverse, if it is already cached, it will get the cahced value
## Otherwise it will solve it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        _inverse <- x$get_inverse()
        if(!is.null(_inverse)) {
          message("getting cached data")
          return(_inverse)
        }
        data <- x$get()
        _inverse <- solve(data, ...)
        x$set_inverse(_inverse)
        _inverse
}
