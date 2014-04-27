## cachematrix.R file has two R functions,
## 1. makeCacheMatrix - used to set the matrix and its inversed matrix thru the setters(setMatrix and setInversed) 
##                      and get the matrix and its inversed matrix thru its getters(getMatrix and getInversed) 
## 2. cacheSolve      - invoked with the matrix,checks if the given matrix has the inverse cached.if cached, gets
##                      the cache data or calculate the invere of the matrix using solve method and cache it with setInverse method. 

## makeCacheMatrix - Input of this function is x which is a matrix. It has the following funtions defined.
##                   setMatrix - set the given matrix to the value 'x'
##                   getMatrix - returns the value of the matrix 'x'
##                   setInversed - set the inversed matrix value passed to 'm
##                   getInversed - return the inversed matrix value 'm
##                   x is a matrix and m is the inverse of the matrix x.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  setMatrix<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  getMatrix<-function() x
  getInversed<-function() m
  setInversed<-function(inversedMatrix) m<<-inversedMatrix
  list(setMatrix=setMatrix,getMatrix=getMatrix,getInversed=getInversed,setInversed=setInversed)
}


## cacheSolve - Input is the matrix value of 'x'
##              checks if the 'x' has the matrix inversed already, if yes, get the inversed matrix from the cache
##              else calculates the inverse of the matrix 'x' and set the inversed matrix using setInversed method to cache it
##              and returns the inversed matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getInversed()
  if(!is.null(m))
  {
    message('getting cached inversed matrix')
    return(m)
  }
  data<-x$getMatrix()
  m<-solve(data)
  x$setInversed(m)
  m
}
