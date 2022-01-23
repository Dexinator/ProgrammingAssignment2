## For this assignment I've found the inv function from matlib library
## The inv returns the inverse matrix (if exists)
##I've followed the example structure of the vector
##With these 2 functions one can calculate the inverse of a matrix and in
##case of multiple calls with the same input our result is stored in the first call

library(matlib)

## On this function multiple functions are defined inside, they will indicate us the value
##.if it was previously calculated, check that all values, except the  first "m" are stored with <<-
##This function returns a list that our next function will use

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## On this second function we check if we are calculating the same answer as last time it will call
##of the list returned in the first function with the answer, if is different will calculate the inverse and
##store it in the corresponding element of the first function lis return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}

## On this function multiple functions are defined inside, they will indicate us the value
##.if it was previously calculated, check that all values, except the  first "m" are stored with <<-
##This function returns a list that our next function will use

makeCacheMatrix2 <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## On this second function we check if we are calculating the same answer as last time it will call
##of the list returned in the first function with the answer, if is different will calculate the inverse and
##store it in the corresponding element of the first function lis return

cacheSolve2 <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


A<-matrix( c(5, 1, 0,
                           3,-1, 2,
                           4, 0,-1), nrow=3, byrow=TRUE)
B<-makeCacheMatrix2(A)
C<-cacheSolve2(B)
C %*% A
AI %*% A
B$get()
C
AI
solve(A)
