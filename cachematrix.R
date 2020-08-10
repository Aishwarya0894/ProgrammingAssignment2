#Caching the inverse of the matrix

## makeCacheMatrix function gets the matrix as input and stores the inverse (calculated from cacheSolve) as cache

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get<-function(){x}
  setinverse<-function(inverse){inv<<-inverse}
  getinverse<-function() inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


## cacheSolve function calculates and returns a matrix that is inverse of x 
## The inverse calculated is stored in x$setinverse() as cache
## If inverse of matrix x is already calculated (and if the matrix x has not changed),then the function would retrieve the inverse from the cache (x$getinverse)


cacheSolve <- function(x, ...) {
  inv<-x$getinverse() 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
      
}

# Testing
a<-makeCacheMatrix(matrix(c(1,2,3,4),2,2))
cacheSolve(a)
cacheSolve(a)

a$set(a$getinverse()) # setting the func to be the inverse calculated
cacheSolve(a) # inverse of the calculated inverse matrix is original matrix


