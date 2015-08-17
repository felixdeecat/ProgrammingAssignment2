## create a matrix with a property i that holds (the cache of) its inverse

## "set" and "get" functions set / return the value of the matrix's inverse
## "setInverse" will store the new value of inverse, computed by cachSolve
## "getInverse" returns the newly computed inverse, or the cached value, 
##              if no new matrix is applied

makeCacheMatrix <- function(x = matrix())  # the matrix x must be invertible
{
     i<-NULL # create the property i, which will hold the inverse 
     set<-function(y)
     {
          x <<- y  # set x to the matrix, y
          i <- NULL  # it's new, so it doesn't have a cached inverse
     }
     get<-function()
     {
          x    # just return the matrix x
     }
     setInverse <- function(inv)
     {
          i <<- inv # set the inverse to the new paramter inv
     }
     getInverse <- function()
     {
          i   # return the property i, which is the current stored inverse of the matrix
     }
     list(set=set, get=get, setInverse = setInverse, getInverse = getInverse) # map the properties to function names
     
}

## use makeCacheMatrix as a parameter, x, either return the cached inverse, or use solve() to find inverse & return it

cacheSolve <- function(x, ...) 
{
     ## Return a matrix that is the inverse of 'x'
     i <- x$getInverse()  # get the stored inverse...
     if(!is.null(i))      # if there is no stored inverse...
     {
          message("getting cached values")  # return the cached value of i
          return(i)
     }
     data <- x$get()   # retrieve the matrix from cachematrix
     i <- solve(data, ...) # store its new inverse, computed using solve()
     x$setInverse(i)  # use cachematrix property to reset its stored inverse
     i                # return the value of the inverse
}
