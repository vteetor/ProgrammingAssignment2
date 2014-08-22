## These two functions make and cache the inverse of a matrix.

## This function creates the matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(solve) m<<-solve
  getinv<-function()m
  list(set=set,get=get,
       setinv=setinv,getinv=getinv)
}
## This function computes the inverse of the matrix.  If the matrix
## has not changed, and the inverse has already been calculated,
## the solution is retreived from the cache.

cacheSolve <- function(x, ...) {
       m<-x$getinv()
       if(!is.null(m)){
         message"Getting cached data"
         return(m)
       }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
}
