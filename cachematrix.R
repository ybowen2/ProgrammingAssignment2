## Editor: Yashna Bowen

## makeCacheMatrix and cacheSolve work together for both 
## matrix inverse computation and also retrieving the cache
## copy of the inverse when it has already been computed


##--------------------------------------------------------------------

## makeCacheMatrix is based on the fact that R uses lexical scoping in 
## assignments and retrieval. It returns what is basically an object
## of type makeCacheMatrix in the OOP sense. This is in fact a list
## which contains pointers to all the internal functions of the 
## makeCacheMatrix function.

## This way by using the list we can get, set the inverse of matrix 
## indirectly through those inner functions in the makeCacheMatrix 
## environment


makeCacheMatrix <- function(x = matrix()) {

 ##sets the default value of the inverse as NULL for later use
 inn <- NULL


        set <- function(y) {
                x <<- y
                inn <<- NULL
        }

        ## returns the matrix orginal
        get <- function() x

        ## sets the matrix inverse fro cacheSolve computation
        setinverse <- function(inversem) inn <<- inversem

        ## returns the inverse matrix
        getinverse <- function() inn

        ## return list of pointers to functions
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}



##------------------------------------------------------------------

## cacheSolve takes an argument of the type makeCacheMatrix which in
## this case is the list object returned. The $ is then used to get the 
## cached inverse if no changes are found in the matrix previously passed in
## and to compute the inverse for a new matrix. This is done through 
## accessing the nested functions of makeCacheMatrix

cacheSolve <- function(x, ...) {

  ## gets matrix inverse from makeCacheMatrix function
  m <- x$getinverse()
     
     ## if the matrix is cached returns value
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }

     ## else gets the original matrix and computes the inverse
     data <- x$get()
     m <- solve(data)

     ##sets inverse value
     x$setinverse(m)
     
     ##returns the matrix inverse
     m


       
}

## END 