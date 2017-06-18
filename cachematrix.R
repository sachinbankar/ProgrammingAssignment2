## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special matrix object that caches inverse

makeCacheMatrix <- function(x = matrix()) 
  {
    inv <- NULL
    set <- function(y)
    {
      x <<- NULL
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solvematrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
  }


## cacheSolve function computes the inverse of the  matrix returned by makeCacheMatrix function

cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv))
    {
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
  }
