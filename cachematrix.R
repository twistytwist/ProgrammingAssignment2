
## Allows caller to set/get a matrix, set/get the inverse value of the matrix 
## and check if the inverse is synced to the currently saved matrix value
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  inverseMatchesMatrix <- NULL
  
  if( is.na(x) )
  {
    matrix(rnorm(1e6),nrow=1e3,ncol=1e3)
  }
  
  #cache matrix value and indicate that inverse is out of sync with matrix
  set <- function(y) {
    matrixVal <<- y
    mi <<- NULL
    inverseMatchesMatrix <<- FALSE
  }
  
  #return matrix value
  get <- function() matrixVal
  
  #sets the inverted value of the matrix
  setInversion <- function(inversion) {
    mi <<- inversion
    inverseMatchesMatrix <<- TRUE
  }
  
  #gets the inverted value of the matrix that is cached
  getInversion <- function() mi
  #indicates if the matrix and inverted value are in sync
  getInversionMatchesMatrix <- function() inverseMatchesMatrix
  
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion,
       getInversionMatchesMatrix = getInversionMatchesMatrix)
}


## return the inverse of the passed in matrix
cacheSolve <- function(x, ...) {
  invRet <- NULL
  inverseSet <- x$getInversionMatchesMatrix()
  
  #if the matrix has been updated without the inversion being recalculated (or is null), recalculate the inversion
  if( is.na(inverseSet) || !inverseSet ) {
    #calculate the inversion and cache it
    invRet <- solve(x$get)
    x$setInversion(inv)
  } else {
    #get the cached inversion
    invRet <- x$getInversion  
  }
  
  return( invRet )
}
