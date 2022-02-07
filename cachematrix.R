##Generate a matrix, cache its inverse, calculate inverse of matrix


##makeCacheMatrix generates a matrix that is assumed to be invertible, and
##the inverse of this matrix is cached

makeCacheMatrix <- function(x = matrix()) {
  specialMatrix <- NULL
  set <- function(y) {
    x<<-y
    specialMatrix<<-NULL
  }
  get <- function() x
  setInverse <- function(solve) specialMatrix<<-solve
  getInverse<-function() specialMatrix
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


##cacheSolve computes the inverse of a matrix, and if the inverse has already been 
##calculated then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  specialMatrix<-x$getInverse()
  if(!is.null(specialMatrix)) {
    return(specialMatrix)
  }
  data<-x$get()
  specialMatrix<-solve(data,...)
  x$setInverse(specialMatrix)
  specialMatrix
}
