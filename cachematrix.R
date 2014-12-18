## https://github.com/ykulikovskiy/ProgrammingAssignment2.git

## makeCacheMatrix created  matrix which contains
## function to get/set matrux value, get/set value of inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(val) {  
    x <<- val
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function inverse of the matrix created by makeCacheMatrix function
## on 1st function run, this function check that inverse of matrix already calculated
## if calculated(cached), than return result. In other case save  matrix.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
