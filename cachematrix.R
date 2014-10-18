## makeCacheMatrix is used to Cache data via use the <<
## two functions, the makeCacheMatrix function is used to cache the data has been calcalted.
## cacheSolve is used to inverse the matrix if no cached data , and call the make Cachesolve function
## if have will call the catached data rather than calcuate it again

## Cache the data when finish the first calcuate

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y ## <<- this operator is used to assign a value to an object in an environment that is different from the
          ## current environment
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i <<- inverse
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Calcuate the inverse matrix, or call the cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...) # solve function will inverse the matrix data.
  x$setinverse(i)
  i
}
