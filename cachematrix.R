# Write the following functions:
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.



makeCacheMatrix<- function(x = matrix()){
  #Initialize the objects
  i <- NULL #set m to Null, to be used later within makeCacheMatrix
  #define the set function that takes y as the argument
  set <- function(y){
    x <<- y #assign the y argument input to x to be used in the Parent environment
    i <<- NULL #Assign NULL to m in the parent environment, clears any previous m that may be floating around. 
  }
  
  #define the get function for x
  get <- function() x #leave x on the outside so R knows to get x from the parent environment
  setInverse <- function(solve) i <<- solve #set inverse of matrix, because m is in the parent environmen, we access it after set inverse is completed
  #Define the getter for i. Find i in the global env. The 'solve' used here can be substitue with any word. Just means that this function is apply on to 'i'
  getInverse <- function() i
  #Assign each of the above function to a list. and return the list to the parent environment
  list(set = set, get = get,    #give the name 'set' and 'get' to the set() and set() functions above
       setInverse = setInverse, #give the name 'setInverse' to the setInverse() function above
       getInverse = getInverse) #give the name 'getInverse' to the getInverse() function above
  #List elements allows us to use $ to access function by name rather than using [[]]
}


#create the function to go with makeCacheMatrix
#This is REQUIRED to retrieve the inverse from the makeCacheMatrix as they will not take atomic vector

cacheSolve <- function(x, ...) { #x is the argument, ... allows for passing additional function in 
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached Data")
    return(i)
    #check if there is an existing inverse matrix already, and return the matrix if found. 
  }
  
  data <- x$get() #get data and assign to x
  i <- solve(data, ...) #the only place where solving for the matrix is actually working
  x$setInverse(i)
  i
}

##test code

aMatrix <- matrix(round(rnorm(25, 100, 2)), 5)
Mx <- makeCacheMatrix(aMatrix)
Mx$get()
Mx$set(matrix(round(rnorm(36, 80, 12)), 6))
Mx$getInverse()
Mx$get()
invMx <- cacheSolve(Mx)
Mx$getInverse()
