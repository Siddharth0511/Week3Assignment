## This Creates a list of setters and getters for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m<-matrix(,nrow=nrow(x),ncol=ncol(x)) #Declaring a null matrix
  set <-function(y)
  {
  x<<-y
  m<<-matrix(,nrow=nrow(x),ncol=ncol(x)) #Declaring a null matrix when x is reset
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse # Setting m to the inverse
  getinverse<-function() m
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Retrieves the inverse from cache or calculate the inverse of the matrix created through makeCacheMatrix()

cacheSolve <- function(x, ...) 
{
  m<-x$getinverse() #Get inverse from cache
  if(!all(is.na(m))) # Check if all elements are not null so that when not null, cached data is returned
    {
    message("getting cached data")
    return(m)
    }
  data<-x$get() #get the matrix
  m<-solve(data) # calculate inverse assuming invertible square matrix as mentioned in the question
  x$setinverse(m) # set m with the inverse
  m # REturn inverse matrix
}
