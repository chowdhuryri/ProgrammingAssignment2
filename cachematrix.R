## This is a constructor function which will take an invertible square matrix 
##  that can cache it's inverse and will perform following tasks
## 1. Get the value of the matrix
## 2. Set the value of the inverse into cache
## 3. Get the value of the inverse from cache


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

                   my_inv     <- NULL
                   get        <- function() x
                   setInverse <-function(inverse) my_inv <<- inverse
                   getInverse <-function() my_inv
                   list(get=get, setInverse=setInverse, getInverse=getInverse) 
}


## This function will take special matrix created by previous function as an
## argument and will compute inverse if not in the cache and store it in cache

cacheSolve <- function(x, ...) {    
              my_inv <- x$getInverse()
              if(!is.null(my_inv)){
                     message("Getting cached inverse")
                     return(my_inv)
              }
              my_mat <- x$get()
              my_inv <- solve(my_mat,...)
              x$setInverse(my_inv)
              my_inv
}
