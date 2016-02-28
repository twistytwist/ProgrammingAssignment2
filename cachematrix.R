
## Allows caller to set/get a matrix, set/get the inverse value of the matrix 
## and check if the inverse is synced to the currently saved matrix value
makeCacheMatrix <- function(matrixVal = matrix()) {
  mi <- NULL
  
  #cache matrix value and indicate that inverse is out of sync with matrix
  set <- function(y) {
    matrixVal <<- y
    mi <<- NULL
  }
  
  #return matrix value
  get <- function() matrixVal
  
  #sets the inverted value of the matrix
  setInversion <- function(inversion) {
    mi <<- inversion
  }
  
  #gets the inverted value of the matrix that is cached
  getInversion <- function() mi

  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion
  )
}


## return the inverse of the passed in matrix
cacheSolve <- function(x, ...) {
  invRet <- x$getInversion()

  if( !is.null(invRet)) {
    return( invRet )
  }
  
  #get the matrix, calculate the inverse and cache it
  matrixVal <- x$get 
  invRet <- solve( matrixVal ) %*% matrixVal
  x$setInversion(invRet)
  
  invRet
}
