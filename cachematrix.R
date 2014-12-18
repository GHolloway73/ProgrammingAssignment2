## This program creates 2 functions for caching and retrieving potentially time-consuming calcalations.  The first 
## function (makeCacheMatrix) calculatesthe inverse of matrix x, and stores it in the memory cache. The second call 
## either retrieves the stored inverse matrix of x (if already cached) or calculates a new inverse matrix if this is 
## the first call for a unique input matrix

## The makeCacheMatrix function calculates the inverse of square matrix x, and stores it in the memory cache. There 
## are 3 sub-functions: get, ## getinverse, and setinverse which 1) retrieves matrix x (get), 2) calculates and stores 
## the inverse matrix of x (set inverse), and 3) retrieves the inverse matrix of x (getinverse)

makeCacheMatrix <- function(x = matrix()) {  # Input is a square matrix x (ie. matrix(rnorm(100),10,10))
  im <- NULL                                 # im (call for inverse matrix) is reset to Null every time 
                                             # makeCacheMatrix is called
  
  get <- function() x                        # Returns the value of input matrix x. This is
                                             # called in the cacheSolve function
  setinverse <- function(solve) im <<- solve # This is called in cacheSolve function
                                             # during the first cacheSolve call. Using the superassignment
                                             # operator, the inverse of matrix x (im) will be calculated 
                                             # and stored here
                                           
  getinverse <- function() im                # Will return the cached value of im on subsequent
                                             # cacheSolve calls for the same matrix x
  list(get = get,                            # get, setinverse, getinverse are accessed here
       setinverse = setinverse,              # each time functions makeCacheMatrix/cacheSolve are called. This
       getinverse = getinverse)              # creates a list so the function knows how to
                                             # access methods
}

##This function retrieves the stored inverse of matrix x (im) if existing, or if not, then calculates a new im

cacheSolve <- function(x, ...) {             #The input here is the output of makeCacheMatrix. An example would the call 
                                             # "test" produced by the makeCacheMatrix call
                                             # (ie. test<-makeCacheMatrix(matrix(rnorm(100),10,10))
  im <- x$getinverse()                       
  if(!is.null(im)) {                         #if inverse matrix im is already cached, then
                                             # returns cached inverse matrix im
    message("getting cached data")
    return(im)
  }
  data <- x$get()                            # if x$getinverse() returns NULL (no cached im), then 
                                             # matrix is x is retrieves, and a new inverse matrix is calculated
  im <- solve(data, ...)
  x$setinverse(im)                           # Stores calculated inverse matrix im
  im                                         # Return the inverse matrix
}
