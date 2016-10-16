## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

 inn <- NULL
        set <- function(y) {
                x <<- y
                inn <<- NULL
        }
        get <- function() x
        setinverse <- function(inversem) inn <<- inversem
        getinverse <- function() inn
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {


  m <- x$getinverse()
     
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     
     m


        ## Return a matrix that is the inverse of 'x'
}
