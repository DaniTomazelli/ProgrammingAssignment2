###set/get value of matrix and  inverse
makecachematrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver  <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}

###Returns the inverse of the matrix
cachesolved <- function(x, ...) {
  inver <= x$getinverse()
  if(!is.null(inver)) {
    message("Getting cached data ")
    return( inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$setinverse(inver)
  inver
}
