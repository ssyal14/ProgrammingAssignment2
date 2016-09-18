## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y  
    m <<- NULL 
  }
  get <- function() x 
  setinverse <- function(solve) 
    m<<- solve
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                
  if(!is.null(m)){                   
    message("Data from cache")   
    return(m)                           
  }
  data <- x$get()                    
  m <- solve(data, ...)               
  x$setinverse(m)    
  return(m)        
   ## Return a matrix that is the inverse of 'x'
}


# Example using the program :
#
# x=matrix(c(2, 4, 3, 1), nrow=2,ncol=2)     
# m = makeCacheMatrix(x)
# m$get()
#       [,1] [,2]
# [1,]    2    3
# [2,]    4    1
#
# Original computation in first time
# cacheSolve(m)
#       [,1] [,2]
#  [1,] -0.1  0.3
#  [2,]  0.4 -0.2
#
# Now the data will be from cache
# cacheSolve(m)
# Data from cache
#       [,1] [,2]
#  [1,] -0.1  0.3
#  [2,]  0.4 -0.2


