makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #setting the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x #getting the matrix
  setinv <- function(inverse) inv <<- inverse #setting the inverse in the cahce
  getinv <- function() inv #getting the inverse
  list(set = set, get = get, #return the matrix
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) { #check if the matrix already calculated, then return it
    message("getting cached data")
    return(inv)
  }
  data <- x$get() 
  inv <- solve(data, ...) #calculate the matrix
  x$setinv(inv) #set the inverse in the cache
  inv #return the inverse
}