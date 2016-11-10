## Put comments here that give an overall description of what your
## functions do

## create a object x which is a matrix and associate some functions to it 

makeCacheMatrix <- function(x = matrix()) {
 
  
  ## define the cache m 
  m <- NULL 
  set <- function(y) { 
    x <<- y ## assign the input matrix y to the variable x in the 
    ## parent environment 
    m <<- NULL ## re-initialize m in the parent environment to null 
  } 
  get <- function() x ## return the matrix x 
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal 
  ## to the inverse of the matrix x 
  getinverse <- function() m ## return the cached inverse of x 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
} 

}


## The function below calculates the inverse of the special "matrix" 
## with the function created above. It will first checks if the inverse 
## has already been computated. If yes, it will receive the inverse from the cache 
## and skips the computation. Or else, it will calculate the matrix inverse 
## and sets the value of the inverse in the cache via the 'setinverse' function. 


cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  
  m <- x$getinverse() 
  if(!is.null(m)) { 
    message("getting cached data") 
    return(m) 
  } 
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinverse(m) 
  m 
} 
