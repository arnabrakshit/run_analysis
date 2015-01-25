# ASSIGNMENT2-R
makeCacheMatrix <- function(x = matrix()) {

  xinv <- NULL
  set <- function(y) {
	x <<- y
	xinv <<- NULL # it also initialises xinv to null
  }
  get <- function() x 
  setInv <- function(inv) xinv <<- inv
  getInv <- function() xinv
  list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }
  cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  if(!is.null(m)) { # if the inversion result is there
	  message("getting cached data")
	  return(m) # return the calculated inversion
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
  }
  test <- matrix(runif(9,1,100),3,3)'
  testCached <- makeCacheMatrix(test)
  
