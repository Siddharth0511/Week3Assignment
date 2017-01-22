## This Creates a list of setters and getters for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m<-matrix(,nrow=nrow(x),ncol=ncol(x))
  set <-function(y)
  {
  x<<-y
  m<<-matrix(,nrow=nrow(x),ncol=ncol(x))
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Retrieves the inverse from cache or calculate the inverse of the matrix created through makeCacheMatrix()

cacheSolve <- function(x, ...) 
{
  m<-x$getinverse()
  if(!all(is.na(m)))
    {
    message("getting cached data")
    return(m)
    }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
}
