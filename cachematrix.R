## Create a special class of objects that stores a numeric matrix and
## cache its inverse.
 

## defines the class makeCacheMatrix with its
## attributes, constructor, setters and getters 

makeCacheMatrix <- function(cacheMat = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    cacheMat <<- y
    matInverse <<- NULL              # if modify the matrix, clear cache 
  }
  get <- function() cacheMat
  setInverse <- function(newInverse) matInverse <<- newInverse  
  getInverse <- function() matInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cache the inverse of cacheMat or return the value cached previously.

cacheSolve <- function(matr, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInverse <- matr$getInverse()
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  cacheMat <- matr$get()
  matInverse <- solve(cacheMat, ...)
  matr$setInverse(matInverse)
  matInverse
}
