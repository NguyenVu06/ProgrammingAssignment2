## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Set default value of inverse 
  # 
  set <- function(y) { #option to set a new matrix with the same default parameters
    x <<- y
    inv <<- NULL
  }
  get <- function() x #return the matrix
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv #Lex scope to find inv value
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) #listing out all the available functions
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() #see if the inverser has already been done
  if (!is.null(inv)) { #Check if there is a cached stored
    message("getting cached data") #return the cached inverse value
    return(inv)
  }
  mat <- x$get() #get the matrix that needs inversing
  inv <- solve(mat, ...) #get the inverse of the Matrix
  x$setInverse(inv) #set the inverse to inv for storage
  inv
}