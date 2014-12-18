
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL #cache matrix reset
  set <- function(y) { #definte set function
    x <<- y #set new input y == x
    m <<- NULL #rest set cache matrix
  }
  get <- function() x #define get function, return x
  setsolve <- function(solve) m <<- solve  #define setsolve, store the solved matrix using superassignment
  getsolve <- function() m #return the cached solved matrix to cacheSolve() on subsequent accesses
  list(set = set, get = get, #list of method
       setsolve = setsolve,
       getsolve = getsolve)
}
cacheSolve <- function(x, ...) {
  m <- x$getsolve() #input current getsolve() and store in cache matrix
  if(!is.null(m)) { #is cache matrix is not emtpy
    message("getting cached data")
    return(m) #output cache matrix
  }
  data <- x$get() #if the cache matrix is empty, get x and store in data
  m <- solve(data) #solve data and put into cache matrix
  x$setsolve(m) #store the solved matrix in x
  m #return the solved matrix
}