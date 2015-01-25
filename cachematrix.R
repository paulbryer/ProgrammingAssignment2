## These  two functions cache the inverse of a matrix to avoid
## wasting time repeatedly computing it

## The makeCacheMatrix function takes a matrx as an argument and 
## creates a list of functions to store the matrix, return the matrix, 
## compute and cache the inverse of the matrix, 
## and retrieve a cached inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
  
  inv<-NULL  ##initially sets the cached inverse to NULL
  
  setmatrix<-function(y){
    mat <<- y  ##stores the matrix being passed to the function as mat (within the function environment)
    inv<<-NULL ##sets the cached inverse to NULL (i.e. clears the cache when a new matrix is passed to the function
    ## inv is the cached inverse matrix, within the function environment)
  }
  
  getmatrix<-function() mat  ##this returns the matrix passed as an argument to the function
  
  setinverse<-function(solve) inv<<- solve ##computes the inverse of matrix and stores this as inv (i.e. cache the inverse) 
  
  getinverse<-function() inv  ##this function returns inv, which is our cached inverse matrix (computed in the setiverse function)
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
  ## returns a list containing the functions detailed above
}



## The cacheSolve function takes a matrix as an argument 
## and checks for a previously calculated and cached inverse of that matrix
## if there isn't one it will calculate the inverse matrix, cache it and return it

cacheSolve <- function(mat=matrix(), ...) {
  inv <- mat$getinverse()  ## gets cached inverse of matrix
  if(!is.null(inv)) {  
    
    ## if cache isn't empty
    
    message("getting cached data")  ##lets user know a cached value was retrieved
    return(inv) ##returns cached inverse
  }  
  
  ##if cache is empty...
  
  newmat <- mat$getmatrix() ##stores the matrix as newmat using the getmatrix function
  inv <- solve(newmat, ...) ##computes the inverse of newmat and stores it as inv
  mat$setinverse(inv) ## caches inv using setinverse function
  inv  ##returns the inverse
}
