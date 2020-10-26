## makeCacheMartrix function creates a special matrix object that can cache its
## inverse. 
## cacheSolve function computes the inverse if the special matrix returned 
## by makeCacheMatrix. if the inverse has already been calculated, the cacheSolve
## will retrieve the inverse from the cache. 


## creates a speecial matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
                  i <- NULL
                  set <- function(y) {
                    x <<- y 
                    i <<- NULL
                  }
                  get <- function()x 
                  setinverse <- function(inverse) i <<-inverse
                  getinverse <- function() i 
                  list(set = set, 
                       get = get, 
                       setinverse = setinverse, 
                       getinverse = getinverse)
                  

}


## computes the inverse of the special matrix returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            i <- x$getinverse()
            if(!is.null(i)){
                  message("getting cache data")
                  return (i)
            }
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
            i
}

