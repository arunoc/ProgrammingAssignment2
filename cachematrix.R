## These R functions are able to cache potentially time-consuming computations.
## The following functions can be used to calculate and then retrieve the inverse
## of a matrix from cache if available.

## The first function, makeCacheMatrix creates a list containing a function that:
## 1. sets the value of a matrix.
## 2. gets the value of a matrix.
## 3. sets the value of an inverse matrix.
## 4. gets the value of an inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) inv <<- solve
    getmatrix <- function() inv
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## The second function will return the value of the inverse of a matrix.
## It will check if the inverse has first already been calculated and
## return the value from cache. However, if it has not been calculated
## it will calculate and return the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getmatrix()
    if (!is.null(inv)) {
        message("Retrieving cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setmatrix(inv)
    inv
}

## Example:
## m <- matrix(c(4,1,3,1), nrow = 2, ncol = 2)
## x <- makeCacheMatrix(m)
## cacheSolve(m)
