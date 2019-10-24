 makeCacheMatrix <- function(x = matrix()) { ##declaration of a function
   m <- NULL				##variable to store the inverse
   set <- function(y){		##setting the value of the matrix
   x <<- y
   m <<- NULL
   }
   get <- function()x 		##getting the value of the matrix
   setInverse <- function(inverse) m <<- inverse ##setting the inverse of the matrix
   getInverse <- function() m ##getting the inverse of the matrix
   list(set = set, get = get,  ##Returned expression
   setInverse = setInverse, 
   getInverse = getInverse)
 }
 
 
 cacheSolve <- function(x, ...) {
   m <- x$getInverse()          ##getting the inverse for x 
   if(!is.null(m)){				## if the resultant matrix is not NULL, then 
   message("getting cached data") ##print that the result is cached
   return(m)					## rreturn the matrix
   }
   data <- x$get()				##get the value of the matrix
   m <- solve(data,...)			##Find the inverse
   x$setInverse(m)				##After finding the inverse, place it in the cache
   m
 }