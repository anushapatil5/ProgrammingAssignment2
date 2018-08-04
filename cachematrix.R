# R script as part of Programming Assignment for Week 3
# "R Programming" - Johns Hopkins University - Coursera 
# Assignment: Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # 'makeCacheMatrix' makes a cache matrix from a given matrix
  # 1. initialize the cache Matrix 'cacheMatrix'
  # assign the value NULL for the first initialization
  
  cacheMatrix <- NULL
  
  # 2. 'setMatrix' function is defined

  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  # 3. 'getMatrix' function is defined
  # return the matrix 'x'
  
  getMatrix <- function() x
  
  # 4. define the method named 'setCache'
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  # 5. define the method named 'getCache'
  # that will return the cached inverse of 'x'
  
  getCache <- function() cacheMatrix
  
  # 6. list of the names of all methods: 
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
   
}

  # This function below computes and returns the inverse of a given matrix by using cache 

cacheSolve <- function(x, ...) {
    
  # 1. check the content of cache matrix

  cacheMatrix <- x$getCache()
  
  # 2. if the content is not null then: return the result 
  
  if (!is.null(cacheMatrix)) {
    message("loading cache data")
    return(cacheMatrix)
  }
  
  # 3. if the content is empty then: 
  # get the matrix, create, set, update and return the cache matrix
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
  
}
