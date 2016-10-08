#Jordan DiNardo
#Project Assignment 2

## These two functions create a special object that stores a matric and caches its inverse

## This function ceates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function()x
    setInverse<-function(inverse)inv<<-inverse
    getInverse<-function()inv
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function finds the inverse of the matrix created by the above function. 
## If the inverse has already been calculated, then it will deliver the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
