## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a list containing the functions to set and get a matrix, set and get its inverse
makeCacheMatrix <- function(x = matrix()) {
                  inv <- NULL
                  set <- function(m) {
                    x <<- m
                    inv <<- NULL
                  }
                  get <- function() m
                  setInv <- function(inverse) {
                    inv <<- inverse
                  }
                  getInv <- function() inv
                  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## Get the inverse of a matrix by first checking the cache, if not then calculate its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              m <- x$getInv()
              if(!is.null(m)) {
                message("Retrieve from cache")
                return(m)
              }
              matrix <- x$get()
              m <- solve(matrix)
              x$setInv(m)
              return(m)
}
